//! rope, Rope data structure implementation and tests.
const std = @import("std");
const Rc = @import("zigrc").Rc;

//const BRANCH_FACTOR = 7;


/// For convenient indexing inside a Rope. Refers to a row, col
/// position relative to a root node. The first byte is always `row=0,
/// col=0`.
pub const Pos = struct {
    row: usize,
    col: usize,

    /// Less than or equal, self <= other, this pos is before or at
    /// other pos.
    fn lexLe(self: @This(), other: @This()) bool {
        return self.row < other.row or (self.row == other.row and self.col <= other.col);
    }

    fn lexLt(self: @This(), other: @This()) bool {
        return self.row < other.row or (self.row == other.row and self.col < other.col);
    }
};

fn panicAndExit(info: []const u8, extra: anytype) noreturn {
    std.debug.print("{s} {any}", .{ info, extra });
    std.debug.dumpCurrentStackTrace(null);
    std.process.exit(1);
}

pub const Node = NodeBF(7);

/// Every tree node contains aggregate stats for all bytes in its
/// descendents.
const AggregateStats = struct {
    num_newlines: usize,
    num_bytes: usize,
    last_newline_pos: ?usize,

    /// Agg info of empty slice. Equivalent to AggregateStats.fromSlice(.{})
    fn empty() @This() {
        return .{ .num_newlines = 0, .num_bytes = 0, .last_newline_pos = null };
    }

    fn fromSlice(slice: []const u8) @This() {
        var num_newlines: usize = 0;
        var last_newline_pos: ?usize = null;
        for (slice, 0..) |val, i| {
            if (val == '\n') {
                num_newlines += 1;
                last_newline_pos = i;
            }
        }
        return .{ .num_newlines = num_newlines, .num_bytes = slice.len, .last_newline_pos = last_newline_pos };
    }

    test "empty matches fromSlices" {
        const empty_agg = @This().empty();
        const empty_from_slice = @This().fromSlice("");
        try expect(empty_agg.num_newlines == empty_from_slice.num_newlines);
        try expect(empty_agg.num_bytes == empty_from_slice.num_bytes);
        try expect(empty_agg.last_newline_pos == empty_from_slice.last_newline_pos);
    }

    /// Aggregation function - given add info of strings A and B,
    /// computes info of A ++ B.
    fn combine(self: *const @This(), other: @This()) @This() {
        const last_newline_pos = blk: {
            if (other.last_newline_pos) |lnp| {
                break :blk lnp + self.num_bytes;
            } else {
                break :blk self.last_newline_pos;
            }
        };
        return .{
            .num_newlines = self.num_newlines + other.num_newlines,
            .num_bytes = self.num_bytes + other.num_bytes,
            .last_newline_pos = last_newline_pos,
        };
    }
};

const NodeTag = enum {
    leaf,
    inner,
};

/// A rope tree node. Contains aggregate stats for all the bytes
/// stored in descendants. The node itself is either a Leaf or an
/// Inner node; Inner nodes contain refcounted pointers to child
/// nodes.
pub fn NodeBF(branch_factor: comptime_int) type {
    return struct {
        const BRANCH_FACTOR: usize = branch_factor;
        const Self = @This();

        /// Max depth is enough to store 1TB data. Used for Iterator
        /// stack-based recursion. NOTE that the tree need not use the full
        /// available memory - some leaf nodes can store less that
        /// BRANCH_FACTOR bytes, so the tree may be larger.
        const MAX_DEPTH: comptime_int = blk: {
            // 1TB = 10**12 bytes.
            const maxFileSizeBytes: comptime_int = std.math.powi(usize, 10, 12) catch unreachable;
            break :blk std.math.log_int(usize, BRANCH_FACTOR, maxFileSizeBytes) + 2;
        };

        const Leaf = std.BoundedArray(u8, BRANCH_FACTOR);
        // TODO: change to RcArray(BRANCH_FACTOR). Bigg-ish change,
        // affects refcount handling.
        const Inner = RcArray(BRANCH_FACTOR); //std.BoundedArray(Rc(Node), BRANCH_FACTOR);

        agg: AggregateStats,
        node: union(NodeTag) {
            leaf: Leaf,
            inner: Inner,
        },

        /// Byte iterator over all memory stored in this subtree. Contains
        /// UNSAFE NOT REF-COUNTED (!) refs to the root nodes, and some
        /// state to know where it is. DO NOT retain the Iterator after
        /// .release() or .deinit() on the tree nodes!
        /// TODO: this started as code for testing; maybe do a faster
        /// recursive copy-memory function?
        const Iterator = struct {
            rec_stack: std.BoundedArray(struct { node: *const Self, kid_index: usize }, MAX_DEPTH),

            // node: *const Self,
            // kid_index: usize,
            // kid_iterator: ?*Iterator,

            fn next(self: *@This()) ?u8 {
                if (self.rec_stack.len == 0) {
                    return null;
                }
                var top = self.rec_stack.pop();
                if (top.kid_index >= top.node.*.numKidsOrBytes()) {
                    while (self.rec_stack.len > 0) {
                        var new_top = self.rec_stack.pop();
                        if (new_top.kid_index < new_top.node.*.numKidsOrBytes()) {
                            new_top.kid_index += 1;
                            // We just popped it so it should fit.
                            self.rec_stack.append(new_top) catch unreachable;
                            return self.next();
                        }
                    }
                    // Couldn't find any nodes to increment kid indices, must be done.
                    return null;
                }
                switch (top.node.*.node) {
                    .leaf => |leaf_bytes| {
                        const res = leaf_bytes.get(top.kid_index);
                        top.kid_index += 1;
                        // Must work, we just popped it.
                        self.rec_stack.append(top) catch unreachable;
                        return res;
                    },
                    .inner => |kids| {
                        const kid_ptr: *const Self = kids.arr.get(top.kid_index).value;

                        // we just popped it, so it fits.
                        self.rec_stack.append(top) catch unreachable;

                        // This will work unless the stack is MAX_DEPTH,
                        // which COULD but shouldn't happen unless you
                        // manually chain together a long list of parent
                        // nodes. Let's safely crash in Release if it happens.
                        self.rec_stack.append(.{ .node = kid_ptr, .kid_index = 0 }) catch
                            panicAndExit("Can't append, depth too high!\n", .{self.rec_stack});

                        // Recursively get the next byte and update state.
                        return self.next();
                    },
                }
            }
        };

        fn height(self: *const @This()) usize {
            switch (self.node) {
                .leaf => |_| return 0,
                .inner => |kids| {
                    std.debug.assert(kids.arr.len > 0);
                    return 1 + kids.top().value.*.height();
                },
            }
        }

        fn numKidsOrBytes(self: *const @This()) usize {
            switch (self.node) {
                .leaf => |leaf| return leaf.len,
                .inner => |kids| return kids.arr.len,
            }
        }

        /// Releases the refcounted kid pointers, recursively calling
        /// their d-tors if they are deallocated. `self` is value because
        /// this function is passed to `Rc.releaseWithFn`
        pub fn deinit(self: @This()) void {
            switch (self.node) {
                .leaf => |_| return,
                .inner => |kids| {
                    var kids_v = kids;
                    kids_v.release();
                },
            }
        }

        pub fn fromSlice(slice: []const u8, alloc: std.mem.Allocator) !Rc(Self) {
            // Compute depth of resulting tree; needed to make sure all
            // leafs will be on the same level.
            var depth: usize = 0;
            var sz: usize = BRANCH_FACTOR;
            while (sz < slice.len) {
                sz *= BRANCH_FACTOR;
                depth += 1;
            }
            return fromSliceDepth(slice, depth, alloc);
        }

        ///TODO: test with failing allocator!
        fn fromSliceDepth(slice: []const u8, depth: usize, alloc: std.mem.Allocator) !Rc(Self) {
            // Assert that the slice will fit in a tree of the target depth.
            const max_size: usize = try std.math.powi(usize, BRANCH_FACTOR, depth + 1);
            std.debug.assert(slice.len <= max_size);

            if (slice.len <= BRANCH_FACTOR) {
                // Only error is overflow, which cannot happen under len <= BRANCH_FACTOR.
                const leaf = Leaf.fromSlice(slice) catch unreachable;
                const agg = AggregateStats.fromSlice(slice);
                const n = Self{ .node = .{ .leaf = leaf }, .agg = agg };
                var res = try Rc(Self).init(alloc, n);
                errdefer res.releaseWithFn(Self.deinit);

                // All leafs must be on the same level; From this, it
                // follows that we sometimes need a chain
                // node--node--node--leaf. This loop adds the chain of
                // parent nodes to reach the target depth.
                for (0..depth) |_| {
                    var res_inner = Inner {};
                    errdefer res_inner.release();
                    try res_inner.push(res);

                    res.releaseWithFn(Self.deinit);
                    const res_node = Self{ .node = .{ .inner = res_inner }, .agg = agg };
                    res = try Rc(Self).init(alloc, res_node);
                }
                std.debug.assert(res.value.*.height() == depth);
                return res;
            }

            // Split into B evenly sized pieces, but prefer BRANCH_FACTOR-sized leaf nodes.
            const max_piece_size: usize = @max(try std.math.divCeil(usize, slice.len, BRANCH_FACTOR), BRANCH_FACTOR);
            var piece_start: usize = 0;
            var inner: Inner = .{};
            defer inner.release();
            while (piece_start < slice.len) {
                const piece_end = @min(slice.len, piece_start + max_piece_size);
                var node_rc = try Self.fromSliceDepth(slice[piece_start..piece_end], depth - 1, alloc);
                try inner.push(node_rc);
                node_rc.releaseWithFn(Self.deinit);
                piece_start = piece_end;
            }

            return try Self.innerFromSlice(inner.arr.slice(), alloc);
        }

        /// For (rather slowly) iterating though all bytes in the tree
        /// rooted at `self`. Amortized O(1) per byte if the tree is
        /// properly balanced, but recursive, slow-ish, and crashes on
        /// very very deep tres.
        fn allBytesIterator(self: *const @This()) Iterator {
            var res: Iterator = .{ .rec_stack = .{} };
            res.rec_stack.append(.{ .node = self, .kid_index = 0 }) catch unreachable;
            return res;
        }

        /// Creates a new tree that contains concatenated data from `self`
        /// and then `other`, in that order. The new tree will contain
        /// pointers to nodes of `self` and `other`; refcounts will be
        /// incremented. Will allocate `O(max(self.height(),
        /// other.height()))` new nodes. Returns a refcounted pointer to
        /// the new tree. To release the memory, do `result.deinit()` for
        /// recursively decreasing refcounts, and then `result.release()`.
        pub fn concat(self: Rc(Self), other: Rc(Self), alloc: std.mem.Allocator) !Rc(Self) {
            if (self.value.*.agg.num_bytes == 0) {
                var o = other;
                return o.retain();
            }
            if (other.value.*.agg.num_bytes == 0) {
                // Must return
                //const self_rc = Rc(Self).init(alloc, self);
                var self_v = self;
                return self_v.retain();
            }

            //var self_p: Rc(Self) = try Rc(Self).init(alloc, self);
            //defer self_p.release(); // Only release this frame's ptr to
            // the node to prevent memory leaks.

            if (self.value.*.height() >= other.value.*.height()) {
                return Self.concatWithLower(self, other, alloc);
            } else {
                return Self.concatWithHigher(self, other, alloc);
            }
        }



        pub fn splitAt(self: Rc(Self), offset: usize, alloc: std.mem.Allocator) !struct {
            fst: Rc(Self),
            snd: Rc(Self)
        } {
            if (offset > self.value.*.agg.num_bytes) {
                return error.InvalidPos;
            }
            switch (self.value.*.node) {
                .leaf => |leaf_v| {
                    var fst = try Self.fromSlice(leaf_v.slice()[0..offset], alloc);
                    errdefer fst.release();

                    const snd = try Self.fromSlice(leaf_v.slice()[offset..], alloc);
                    return .{.fst=fst, .snd=snd};
                },

                .inner => |inner_v| {
                    var start: usize = 0;
                    const kids = inner_v.arr.slice();
                    for (kids, 0..) |kid, i| {
                        const kid_size: usize = kid.value.*.agg.num_bytes;
                        if (start <= offset and offset <= kid_size) {
                            var kid_split = try splitAt(kid, offset - start, alloc);
                            errdefer kid_split.fst.releaseWithFn(Self.deinit);
                            errdefer kid_split.snd.releaseWithFn(Self.deinit);
                            //_ = i;
                            var a = if (i > 0) try Self.innerFromSlice(
                                kids[0..i], alloc) else try Self.fromSlice("", alloc);
                            errdefer a.releaseWithFn(Self.deinit);

                            const b = if (i+1 < kids.len) try Self.innerFromSlice(
                                kids[(i+1)..], alloc) else try Self.fromSlice("", alloc);

                            //a: Node = Inner.from_kids(kids[:i]) if i > 0 else Leaf.from_bytes(b'')
                            //b: Node = Inner.from_kids(kids[i+1:]) if kids[i+1:] else Leaf.from_bytes(b'')
                            var a_kid_a = try Self.concat(a, kid_split.fst, alloc);
                            errdefer a_kid_a.releaseWithFn(Self.deinit);

                            const kid_b_b = try Self.concat(kid_split.snd, b, alloc);
                            return .{.fst=a_kid_a,
                                     .snd = kid_b_b};

                        }
                        start += kid_size;
                    }
                    unreachable;
                }
            }
            unreachable;
        }

        /// Crashes on failure. Maybe make an error?
        fn assertExtractInner(self: Self) Inner {
            switch (self.node) {
                .inner => |inner_v| return inner_v,
                .leaf => panicAndExit("Logic error, we thought this was an inner node", .{self}),
            }
        }

        /// Crashes on failure. Maybe make an error?
        fn assertExtractLeaf(self: Self) Leaf {
            switch (self.node) {
                .inner => |_| panicAndExit("Logic error, we thought this was a leaf", .{self}),
                .leaf => |leaf_v| return leaf_v,
            }
        }

        /// Takes co-ownership of the kids on success.
        fn innerFromSlice(kids: []const Rc(Self), alloc: std.mem.Allocator) !Rc(Self) {
            std.debug.assert(kids.len <= BRANCH_FACTOR);
             std.debug.assert(kids.len > 0);
            var inner = try Self.Inner.fromSlice(kids);
            errdefer inner.release();

            var agg = AggregateStats.empty();

            for (kids) |kid| {
                agg = agg.combine(kid.value.*.agg);
            }

            return try Rc(Self).init(
                alloc,
                Self {.node = .{.inner = inner}, .agg = agg});
        }


        /// Wrapping struct around BoundedArray(Rc) for keeping track of
        /// refcounts. Increases on push, decreases on pop.
        fn RcArray(sz: usize) type {
            return struct {
                arr: std.BoundedArray(Rc(Self), sz) = .{},

                fn fromSlice(kids: []const Rc(Self)) !@This() {
                    var res: @This() = .{};
                    for (kids) |kid| {
                        try res.push(kid);
                    }
                    return res;
                }

                fn release(self: *@This()) void {
                    while (self.arr.len > 0) {
                        self.pop();
                    }
                }

                fn push(self: *@This(), n: Rc(Self)) !void {
                    var n_copy = n;
                    try self.arr.append(n_copy.retain());

                }

                // Don't return the value because it might have been released.
                fn pop(self: *@This()) void {
                    std.debug.assert(self.arr.len > 0);
                    var popped = self.arr.pop();
                    popped.releaseWithFn(Self.deinit);
                }

                fn top(self: *const @This()) Rc(Self) {
                    std.debug.assert(self.arr.len > 0);
                    return self.arr.get(self.arr.len-1);
                }

                fn clone(self: *const @This()) @This() {
                    var res: @This() = .{};
                    for (self.arr.slice()) |kid| {
                        res.push(kid) catch unreachable;
                    }
                    return res;
                }
            };
        }


        /// Makes a new node out of the kids of `a` and `b`. Asserts that
        /// combined kid/array sizes fit in BRANCH_FACTOR, and that `a`,
        /// `b` have same node type. Result has refcount of ONE; caller is
        /// responsible for releasing.
        fn merge(a: Self, b: Self, alloc: std.mem.Allocator) !Rc(Self) {
            std.debug.assert(a.numKidsOrBytes() + b.numKidsOrBytes() <= BRANCH_FACTOR);
            std.debug.assert(a.height() == b.height());
            std.debug.assert(b: {
                const a_tag: NodeTag = a.node;
                const b_tag: NodeTag = b.node;
                break :b a_tag == b_tag;
            });

            // Should be possible to remove duplication with comptime. TODO!
            switch (a.node) {
                .leaf => |a_leaf| {
                    const b_leaf: Leaf = b.assertExtractLeaf();
                    var res_leaf: Leaf = .{};
                    for (a_leaf.slice()) |p| {
                        res_leaf.append(p) catch unreachable;
                    }
                    for (b_leaf.slice()) |p| {
                        res_leaf.append(p) catch unreachable;
                    }
                    const agg = AggregateStats.fromSlice(res_leaf.slice());
                    const res = Self {.agg=agg, .node = .{.leaf = res_leaf }, };
                    return try Rc(Self).init(alloc, res);
                },
                .inner => |a_inner| {
                    const b_inner: Inner = b.assertExtractInner();
                    var res_inner: Inner = .{};
                    defer res_inner.release();
                    for (a_inner.arr.slice()) |p| {
                        res_inner.push(p) catch unreachable;
                    }
                    for (b_inner.arr.slice()) |p| {
                        res_inner.push(p) catch unreachable;
                    }
                    return try Self.innerFromSlice(res_inner.arr.slice(), alloc);
                }
            }

        }

        /// Concatenation implementation, left big with right small. Also
        /// see the corresponding code in Self._concat_lower in
        /// rope.py. Overall algorithm explained in
        /// https://youtu.be/fbVSXfbNB0M (in Russian, with mutable nodes
        /// rather than immutable as here, and no code). Idea: think the
        /// trees side by side resting on the leafs. Find the rightmost
        /// node (in self) at the same level as the root of the left tree,
        /// and keep the path from the right root (self) to that. Walk up
        /// the chain until you find a node that can be merged with the
        /// right side, then merge and create a new chain or
        /// parents. Sometimes the resulting tree height increases by 1.
        fn concatWithLower(left_p: Rc(Self), right_p: Rc(Self), alloc: std.mem.Allocator) !Rc(Self) {
            //std.debug.print("BEFORE concat\n", .{});

            const h_right = left_p.value.*.height();
            const h_left = right_p.value.*.height();
            std.debug.assert(h_right >= h_left);

            // Terminology: left merge node - the rightmost node of the
            // left tree which is at the same level as the right root.

            // Step (1): construct a chain of nodes from the root to the left merge
            // node inclusive. Refcounts are incremented for things we put
            // in the array and freed on pop/release.
            var rootToLeft: RcArray(MAX_DEPTH) = .{};
            defer rootToLeft.release();

            // No new Rc are created in this block. We don't co-own `left`
            // so it's never retained/released.
            var left: Rc(Self) = left_p;

            rootToLeft.push(left) catch unreachable;
            for (0..h_right - h_left) |_| {
                std.debug.assert(left.value.*.numKidsOrBytes() > 0);
                const left_kids: Inner = left.value.*.assertExtractInner();
                left = left_kids.top();
                rootToLeft.push(left) catch
                    panicAndExit("Can't append, depth too high!\n", .{rootToLeft});
            }

            // Check that we got the correct height:
            std.debug.assert(left.value.*.height() == right_p.value.*.height());



            // Step (2): pad the right node with a bunch of parents until
            // it can be merged with the left node. Pop from rootToLeft so
            // that left=rootToLeft[-1] has the same height as right.

            // We take co-ownership of `right` for this function. When we
            // wrap `right` in parents, we will give away ownership to the parent.
            var right: Rc(Self) = right_p;
            _ = right.retain(); // now it's at N+1
            defer right.releaseWithFn(Self.deinit); // now we can assume it's back at N.


            std.debug.assert(rootToLeft.arr.len > 0);
            while (rootToLeft.top().value.*.numKidsOrBytes() +
                       right.value.*.numKidsOrBytes() > BRANCH_FACTOR) {

                // Move ownership of the previous `right` into a newly
                // created parent, and assign to `right`. Invariant: we
                // still need to release `rigth` after this, which happens
                // though the defer above.
                right = b: {
                    var old_right = right;
                    const new_right = try innerFromSlice(&.{old_right}, alloc);

                    // Now old_right is at N+2
                    old_right.releaseWithFn(Self.deinit); // back at N+1
                    break :b new_right;
                };
                if (rootToLeft.arr.len == 1) {
                    const newParent = try innerFromSlice(&.{rootToLeft.top()}, alloc);
                    // Move ownership of `newParent` to `rootToLeft`.
                    defer {var np = newParent; np.releaseWithFn(Self.deinit);}
                    rootToLeft.pop(); // Safe to release old top, as it's
                    // now co-owned by NewParent.
                    rootToLeft.push(newParent) catch unreachable;
                    // after this block, the sole owner is rootToLeft
                } else {
                    rootToLeft.pop(); // releases ref to .top(), which is
                    // fine since it's owned by the
                    // parent anyway.
                }

                // std.debug.print("after WHILE iteration that wraps `right` and pops path, RIGHT IS\n", .{});
                // recPrintRefcounts(right, 0);

                // std.debug.print("after WHILE iteration that wraps `right` and pops path, LEFT IS\n", .{});
                // recPrintRefcounts(rootToLeft.top(), 0);
            }

            // At this point this function co-owns the pointers
            // `rootToLeft` and `right`. `rootToLeft` has at least one
            // element; `rootToLeft.top()` and `right` are merge-able.
            // std.debug.assert(rootToLeft.arr.len > 0);
            // std.debug.assert(rootToLeft.top().value.*.numKidsOrBytes() +
            //                      right.value.*.numKidsOrBytes() <= BRANCH_FACTOR);

            // std.debug.print("\n\nafter WHILE loop that wraps `right` and pops path, LEFT is\n", .{});
            // // Self.recPrintRefcounts(rootToLeft.top(), 0);

            // std.debug.print("\n\nafter WHILE loop that wraps `right` and pops path, RIGHT is\n", .{});
            // // Self.recPrintRefcounts(right, 0);

            // std.debug.print("\n\nMerging {} with {}\n", .{rootToLeft.top().value.*, right.value.*});

            // NOTE: Allocated by this function and therfore owned by it,
            // but also returned to the caller. Deallocate on errors:
            var merged = try Self.merge(rootToLeft.top().value.*, right.value.*, alloc);
            debugCheck(rootToLeft.top().value, rootToLeft.top().value.*.height());
            debugCheck(right.value, right.value.*.height());
            debugCheck(merged.value, merged.value.*.height());
            // Merged has the same height as rootToLeft.top() -- that is
            // LEFT and `right`.
            // std.debug.print("\n\nMerged result is: \n\n", .{});
            // recPrintRefcounts(merged, 0);
            // errdefer {
            //     for (rootToLeft.arr.slice()) |p| {
            //         recPrintRefcounts(p, 0);
            //     }
            //     //recPrintRefcounts(merged, 0);
            //     //merged.releaseWithFn(Self.deinit);
            // }


            // Step (3); go up the chain `rootToLeft` of parents, and
            // re-create them with `merged` as the rightmost
            // kid. Invariant: `rootToLeft` only contains .inner nodes.

            // Convenient ordering: leftToRoot
            // TODO!!! bug here, reversing only when looking at the array
            // from the start, we look at it from the top.
            // std.mem.reverse(Rc(Self), rootToLeft.arr.slice());
            //const leftToRoot = rootToLeft.arr.slice();

            // std.debug.print("Contents of rootToLeft:\n", .{});
            // for (rootToLeft.arr.slice(), 0..) |ptr, i| {
            //     std.debug.print("\nTree #{}:\n", .{i});
            //     recPrintRefcounts(ptr, 0);
            // }

            rootToLeft.pop();

            // std.debug.print("\nContents of rootToLeft AFTER POP:\n", .{});
            // for (rootToLeft.arr.slice(), 0..) |ptr, i| {
            //     std.debug.print("\nTree #{}:\n", .{i});
            //     recPrintRefcounts(ptr, 0);
            // }



            while (rootToLeft.arr.len > 0) {
                // Clone
                var par_kids: RcArray(BRANCH_FACTOR)  = rootToLeft.top().value.*.assertExtractInner().clone();
                defer par_kids.release();
                rootToLeft.pop();

                std.debug.assert(par_kids.arr.len > 0);
                {
                    const rightmost = par_kids.top();
                    std.debug.assert(rightmost.value.*.height() == merged.value.*.height());
                }
                par_kids.pop();
                par_kids.push(merged) catch unreachable;
                merged.releaseWithFn(Self.deinit);


                // var par_p: Rc(Self)  = b: {
                //     var par_pv = rootToLeft.top();
                //     const res =  par_pv.retain();
                //     rootToLeft.pop();
                //     break :b res;
                // };
                // defer par_p.releaseWithFn(Self.deinit);
                // //const par: Self = par_p.value.*;
                // // const par: Self = b: {
                // //     const par_p  = rootToLeft.top();
                // //     const par: Self = par_p.value.*;
                // //     rootToLeft.pop(); // save to deallocate par_p
                // //     break :b par;
                // // };

                //var par_kids = par_p.value.*.assertExtractInner().clone();
                //defer par_kids.release();
                // std.debug.assert(par_kids.arr.len > 0);

                // const rightmost = par_kids.top();
                // std.debug.print("\nParent is: \n", .{});
                // recPrintRefcounts(par_p, 0);
                // std.debug.print("\nRightmost kid is: \n", .{});
                // recPrintRefcounts(rightmost, 0);
                // std.debug.print("\nIt's going to be replaced with `merged`: \n", .{});
                // recPrintRefcounts(merged, 0);


                // Don't release the kid; it's still owned by its tree.
                //std.debug.assert(rightmost.value.*.height() == merged.value.*.height());
                //par_kids.pop(); // pops `rightmost`

                // `merged` has refcount 1. This MOVES the ownership to
                // `par_kids` without changing the refcount. Note that
                // `merged` is overwritten below.
                //par_kids.push(merged) catch unreachable;
                merged = try Self.innerFromSlice(par_kids.arr.slice(), alloc);

                // var par_agg = AggregateStats.empty();
                // for (par_kids.arr.slice()) |kid| {
                //     par_agg = par_agg.combine(kid.value.*.agg);
                // }
                // const new_par = Self {.agg = par_agg, .node = .{.inner = par_kids}};
                // merged = try Rc(Self).init(alloc, new_par);
                debugCheck(merged.value, merged.value.*.height());
            }

            // TODO! We are at line 178 of the python code. Think about
            // what to do with the refcounts! Should rootToLeft contain
            // rc? but self can't be rc!

            //_ = rootToLeft;
            //_ = self;
            //_ = other;
            //_ = alloc;
            return merged;
        }

        fn concatWithHigher(self_p: Rc(Self), other: Rc(Self), alloc: std.mem.Allocator) !Rc(Self) {
            _ = self_p;
            _ = other;
            _ = alloc;
            unreachable;
        }

        const PosError = error{
            InvalidPos,
        };

        fn debugPrintSlice(self: *const @This()) void {
            var iter = self.allBytesIterator();
            std.debug.print("'", .{});
            while (iter.next()) |b| {
                std.debug.print("{c}", .{b});
            }
            std.debug.print("'\n", .{});
        }

        /// Translates Pos-based indices to offset-based
        /// indices. E.g. `Pos {.row=0, .col=0}` is 0 whenever the rope
        /// contains at least one byte. Returns an error if the tree
        /// doesn't have that position; e.g. `.{row=0, col=1000}` when the
        /// first line has fewer than 1000 bytes. TODO: maybe return what
        /// went wrong? error.LineTooShort / error.TooFewLines?
        fn posToOffset(self: *const @This(), p: Pos) PosError!usize {
            switch (self.node) {
                .leaf => |leaf_p| {
                    const bytes: []const u8 = leaf_p.slice();
                    var row: usize = p.row;
                    var curr_line_start: usize = 0;
                    while (row > 0) {
                        const line_break = std.mem.indexOfScalarPos(u8, bytes, curr_line_start, '\n');
                        if (line_break) |lbp| {
                            curr_line_start = lbp + 1;
                            row -= 1;
                        } else {
                            // Searching for a line beyond this subtree.
                            return PosError.InvalidPos;
                        }
                    }

                    // Check whether the current line contains at least col chars:
                    const next_line_break = std.mem.indexOfScalarPos(u8, bytes, curr_line_start, '\n');
                    const curr_line_length = blk: {
                        if (next_line_break) |nlb| {
                            //"aoeuaoeu\n\n";
                            //          ^ - curr line start, nlb is one away (OKAY)
                            break :blk nlb - curr_line_start;
                        } else {
                            //"aoeuaoeu\n";
                            //          ^ - curr line start, len is one away (OKAY)
                            break :blk bytes.len - curr_line_start;
                        }
                    };
                    if (curr_line_length < p.col) {
                        // Not enough characters in line!
                        return PosError.InvalidPos;
                    }
                    return curr_line_start + p.col;
                },
                .inner => |inner_p| {
                    const kids: []const Rc(Self) = inner_p.arr.slice();
                    var start = Pos{ .col = 0, .row = 0 };
                    var bytes_in_kids: usize = 0;
                    for (kids) |kid| {
                        const kid_agg = kid.value.*.agg;
                        const end_col: usize = blk: {
                            // TODO something fishy here! But tests do pass...
                            if (kid_agg.num_newlines == 0) {
                                break :blk start.col + kid_agg.num_bytes;
                            } else {
                                std.debug.assert(kid_agg.last_newline_pos != null);
                                break :blk kid_agg.num_bytes - 1 -
                                    (kid_agg.last_newline_pos orelse unreachable);
                            }
                        };
                        const end = Pos{ .row = start.row + kid_agg.num_newlines, .col = end_col };
                        if (start.lexLe(p) and p.lexLt(end)) {
                            const kid_p = blk: {
                                if (p.row == start.row) {
                                    break :blk Pos{ .row = 0, .col = p.col - start.col };
                                } else {
                                    // Too tired to think about this; it's
                                    // in the python code, and the tests pass.
                                    std.debug.assert(p.row > start.row);
                                    break :blk Pos{ .row = p.row - start.row, .col = p.col };
                                }
                            };
                            return bytes_in_kids + try kid.value.*.posToOffset(kid_p);
                        }
                        start = end;
                        bytes_in_kids += kid_agg.num_bytes;
                    }
                    return PosError.InvalidPos;
                },
            }
        }

        fn debugCheck(node: *const Self, expected_height: usize) void {
            // Recurslively check that it's balanced:
            switch (node.*.node) {
                .leaf => {
                    std.debug.assert(expected_height == 0);
                },
                .inner => |kids| {
                    for (kids.arr.slice()) |kid| {
                        debugCheck(kid.value, expected_height - 1);
                    }
                },
            }

            // Check that the bytes iterator matches the info in .agg
            var iter = node.*.allBytesIterator();
            var num_newlines: usize = 0;
            var num_bytes: usize = 0;
            var last_newline_pos: ?usize = null;

            while (iter.next()) |b| {
                if (b == '\n') {
                    num_newlines += 1;
                    last_newline_pos = num_bytes;
                }
                num_bytes += 1;
            }
            const agg = node.*.agg;
            std.debug.assert(num_bytes == agg.num_bytes);
            std.debug.assert(num_newlines == agg.num_newlines);
            std.debug.assert(last_newline_pos == agg.last_newline_pos);
        }

        fn recPrintRefcounts(node: Rc(Self), indent: usize) void {
            for (0..indent) |_| {
                std.debug.print("  ", .{});
            }
            const node_tag: NodeTag = node.value.*.node;
            std.debug.print("rc={} ptr=0x{x} {s} {}\n", .{node.strongCount(), @intFromPtr(node.value), @tagName(node_tag), node.value.*.agg});
            if (node_tag == .inner) {
                for (node.value.*.assertExtractInner().arr.slice()) |kid_p| {
                    recPrintRefcounts(kid_p, indent+1);
                }
            }
        }

    };
}

const expect = std.testing.expect;

test "B at least 2" {
    // Can't store data in a tree with branching factor < 2.
    try comptime expect(Node.BRANCH_FACTOR >= 2);
}

test "can create leaf node" {
    const leaf = Node.Leaf{};
    _ = Node{ .node = .{ .leaf = leaf }, .agg = AggregateStats.empty() };
}

/// Creates refcounted leaf node; should have a single strong pointer.
fn leafNodeForTest(alloc: std.mem.Allocator) !Rc(Node) {
    const leaf_content = "ab";
    std.debug.assert(leaf_content.len <= Node.BRANCH_FACTOR);
    const leaf = try Node.Leaf.fromSlice(leaf_content);
    const n = Node{ .node = .{ .leaf = leaf }, .agg = AggregateStats.empty() };
    return try Rc(Node).init(alloc, n);
}

/// Creates a refcounted inner node with a single leaf node; both have
/// 1 strong pointer each.
fn innerNodeForTest(alloc: std.mem.Allocator) !Rc(Node) {
    const leaf = try leafNodeForTest(alloc);

    // release the kid pointer unconditionally, independently of
    // whether parent allocation fails. If it succeeds, the leaf
    // release will not deallocate the leaf, as parent creation
    // increases the refcount.
    defer leaf.releaseWithFn(Node.deinit);
    const parent = try Node.innerFromSlice(&.{leaf}, alloc);
    return parent;

    // var inner = try Node.Inner.init(1);
    // inner.set(0, leaf);
    // const inner_node = Node{ .node = .{ .inner = inner }, .agg = Node.AggregateStats.empty() };
    // return try Rc(Node).init(alloc, inner_node);
}

fn cleanUpNode(node: Rc(Node)) void {
    node.releaseWithFn(Node.deinit);
    //node.value.*.deinit();

}

test "can create leaf node with refcount no leaks" {
    const node = try leafNodeForTest(std.testing.allocator);
    cleanUpNode(node);
}

test "leaf node height is 0" {
    const node = try leafNodeForTest(std.testing.allocator);
    defer node.releaseWithFn(Node.deinit);
    try expect(node.value.*.height() == 0);
}

test "parent node height is 1" {
    var leaf = try leafNodeForTest(std.testing.allocator);
    defer leaf.releaseWithFn(Node.deinit);
    var inner = Node.Inner {};
    try inner.push(leaf);

    var parent_node = Node{ .node = .{ .inner = inner }, .agg = AggregateStats.empty() };
    defer parent_node.deinit();

    try expect(parent_node.height() == 1);
}

test "can create inner node no leaks" {
    const inner: Rc(Node) = try innerNodeForTest(std.testing.allocator);
    cleanUpNode(inner);
}

test "numKidsOrBytes leaf is num bytes" {
    const leaf_content = "ab";
    const leaf = try Node.Leaf.fromSlice(leaf_content);
    //defer leaf.releaseWithFn(Node.deinit);
    const n = Node{ .node = .{ .leaf = leaf }, .agg = AggregateStats.empty() };
    try expect(n.numKidsOrBytes() == leaf_content.len);
}

test "numKidsOrBytes inner is num kids" {
    var leaf =try leafNodeForTest(std.testing.allocator);
    defer leaf.releaseWithFn(Node.deinit);
    var inner = Node.Inner {};
    try inner.push(leaf);
    var inner_node = Node{ .node = .{ .inner = inner }, .agg = AggregateStats.empty() };
    defer inner_node.deinit();

    try expect(inner_node.numKidsOrBytes() == 1);
}

test "can call fromSlice on short slice no leaks" {
    const some_bytes_shorter = "ab";
    const node = try Node.fromSlice(some_bytes_shorter, std.testing.allocator);
    cleanUpNode(node);
}

test "can call fromSlice on longer slice no leaks" {
    const some_bytes_longer = "test string with \n some newlines \n definitely over \n 16 chars";
    const node = try Node.fromSlice(some_bytes_longer, std.testing.allocator);
    cleanUpNode(node);
}

test "can call iterator and next" {
    const leaf = try leafNodeForTest(std.testing.allocator);
    defer leaf.releaseWithFn(Node.deinit);
    var iter = leaf.value.*.allBytesIterator();
    _ = iter.next();
}

test "iterator returns correct values for short text" {
    const text = "ab";
    const node = try Node.fromSlice(text, std.testing.allocator);
    defer cleanUpNode(node);

    var iter = node.value.*.allBytesIterator();
    for (text) |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);
}

const longer_text =
    \\test string with some newlines
    \\\n definitely over 16 chars actually
    \\longer than 64 chars
    \\and some more text
    \\below to pad it out
    \\yes really
;

test "iterator returns correct values for longer text" {
    const node = try Node.fromSlice(longer_text, std.testing.allocator);
    defer cleanUpNode(node);

    var iter = node.value.*.allBytesIterator();
    for (longer_text) |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);
}

const PosIterator = struct {
    slice: []const u8,
    idx: usize = 0,
    row: usize = 0,
    col: usize = 0,

    fn next(self: *@This()) ?Pos {
        if (self.idx >= self.slice.len) return null;
        const res = .{ .row = self.row, .col = self.col };
        if (self.slice[self.idx] == '\n') {
            self.row += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        self.idx += 1;
        return res;
    }
};

test "pos to index in matches PosIterator" {
    const node_p = try Node.fromSlice(longer_text, std.testing.allocator);
    defer cleanUpNode(node_p);

    const node: Node = node_p.value.*;
    var it = PosIterator{ .slice = longer_text };

    for (longer_text, 0..) |_, i| {
        const p = it.next() orelse unreachable;
        //std.debug.print("i: {}, pos: {}, byte: {c}\n", .{i, p, b});
        const offset = node.posToOffset(p) catch unreachable;
        //std.debug.print("offset: {}\n", .{offset});
        try expect(offset == i);
    }
    try expect(it.next() == null);
}

test "posToIndex returns error on invalid col in line" {
    const node_p = try Node.fromSlice(longer_text, std.testing.allocator);
    defer cleanUpNode(node_p);

    const node: Node = node_p.value.*;
    try expect(node.posToOffset(.{ .row = 0, .col = 100000 }) == Node.PosError.InvalidPos);
}

test "posToIndex returns error on invalid line in text" {
    const node_p = try Node.fromSlice(longer_text, std.testing.allocator);
    defer cleanUpNode(node_p);

    const node: Node = node_p.value.*;
    try expect(node.posToOffset(.{ .row = 10000000, .col = 0 }) == Node.PosError.InvalidPos);
}

test "fromSlice tree is balanced and has correct agg" {
    const node = try Node.fromSlice(longer_text, std.testing.allocator);
    defer cleanUpNode(node);

    const height = node.value.*.height();

    node.value.*.debugCheck(height);
}

test "print max depth" {
    // Apparently it's 21 when B is 4 and MAX_DEPTH=11 when B is 16.
    std.debug.print("MAX_DEPTH: {}\n", .{Node.MAX_DEPTH});
}


test "can call concat non-empty with empty" {
    const node_p = try Node.fromSlice(longer_text, std.testing.allocator);
    defer cleanUpNode(node_p);

    const empty_node_p = try Node.fromSlice("", std.testing.allocator);
    defer cleanUpNode(empty_node_p);

    const concat_node_p = try Node.concat(node_p, empty_node_p, std.testing.allocator);
    defer cleanUpNode(concat_node_p);

    var iter = concat_node_p.value.*.allBytesIterator();
    for (longer_text) |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);


    // std.debug.print("Refcounts: long root:\n", .{});
    // recPrintRefcounts(node_p, 0);

    // std.debug.print("Refcounts: empty leaf:\n", .{});
    // recPrintRefcounts(empty_node_p, 0);

    // std.debug.print("Refcounts: combined :\n", .{});
    // recPrintRefcounts(concat_node_p, 0);
}

test "can call concat empty with non-empty" {
    const node_p = try Node.fromSlice(longer_text, std.testing.allocator);
    defer cleanUpNode(node_p);

    const empty_node_p = try Node.fromSlice("", std.testing.allocator);
    defer cleanUpNode(empty_node_p);

    const concat_node_p = try Node.concat(empty_node_p, node_p, std.testing.allocator);
    defer cleanUpNode(concat_node_p);

    var iter = concat_node_p.value.*.allBytesIterator();
    for (longer_text) |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);

    // std.debug.print("Refcounts: long root:\n", .{});
    // recPrintRefcounts(node_p, 0);

    // std.debug.print("Refcounts: empty leaf:\n", .{});
    // recPrintRefcounts(empty_node_p, 0);

    // std.debug.print("Refcounts: combined :\n", .{});
    // recPrintRefcounts(concat_node_p, 0);
}


test "can concat short non-empty with short non-empty" {

    const ab = try Node.fromSlice("ab", std.testing.allocator);
    defer cleanUpNode(ab);

    const cd = try Node.fromSlice("cd", std.testing.allocator);
    defer cleanUpNode(cd);

    const abcd = try Node.concat(ab, cd, std.testing.allocator);
    defer cleanUpNode(abcd);

    var iter = abcd.value.*.allBytesIterator();
    for ("abcd") |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);

    // std.debug.print("Refcounts: ab:\n", .{});
    // recPrintRefcounts(ab, 0);

    // std.debug.print("Refcounts: cd:\n", .{});
    // recPrintRefcounts(cd, 0);

    // std.debug.print("Refcounts: combined :\n", .{});
    // recPrintRefcounts(abcd, 0);
}

test "can concat longer non-empty with short non-empty" {
    const abcd = try Node.fromSlice("abcd", std.testing.allocator);
    defer cleanUpNode(abcd);

    const cd = try Node.fromSlice("cd", std.testing.allocator);
    defer cleanUpNode(cd);

    const abcdcd = try Node.concat(abcd, cd, std.testing.allocator);
    defer cleanUpNode(abcdcd);

    var iter = abcdcd.value.*.allBytesIterator();
    for ("abcdcd") |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);

}


test "can concat long non-empty with short non-empty" {
    //std.debug.print("\n\n\n", .{});
    const long_node = try Node.fromSlice(longer_text, std.testing.allocator);
    defer cleanUpNode(long_node);

    const cd = try Node.fromSlice("cd", std.testing.allocator);
    defer cleanUpNode(cd);

    const long_node_cd = try Node.concat(long_node, cd, std.testing.allocator);
    defer cleanUpNode(long_node_cd);

    var iter = long_node_cd.value.*.allBytesIterator();
    for (longer_text ++ "cd") |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);
}

test "can concat short with self" {
    //std.debug.print("\n\n\n", .{});
    const ab = try Node.fromSlice("ab", std.testing.allocator);
    defer cleanUpNode(ab);

    // const cd = try Node.fromSlice("cd", std.testing.allocator);
    // defer cleanUpNode(cd);

    const ab_ab = try Node.concat(ab, ab, std.testing.allocator);
    defer cleanUpNode(ab_ab);

    var iter = ab_ab.value.*.allBytesIterator();
    for ("abab") |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);
}

test "can concat long with self" {
    //std.debug.print("\n\n\n", .{});
    const longer = try Node.fromSlice(longer_text, std.testing.allocator);
    defer cleanUpNode(longer);

    // const cd = try Node.fromSlice("cd", std.testing.allocator);
    // defer cleanUpNode(cd);

    const longer_longer = try Node.concat(longer, longer, std.testing.allocator);
    defer cleanUpNode(longer_longer);

    var iter = longer_longer.value.*.allBytesIterator();
    for (longer_text ++ longer_text) |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);
}

test "can concat long with other long" {
    //std.debug.print("\n\n\n", .{});
    const longer = try Node.fromSlice(longer_text, std.testing.allocator);
    defer cleanUpNode(longer);

    const longer2 = try Node.fromSlice(longer_text, std.testing.allocator);
    defer cleanUpNode(longer2);

    // const cd = try Node.fromSlice("cd", std.testing.allocator);
    // defer cleanUpNode(cd);

    const longer_longer = try Node.concat(longer, longer2, std.testing.allocator);
    defer cleanUpNode(longer_longer);

    var iter = longer_longer.value.*.allBytesIterator();
    for (longer_text ++ longer_text) |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);

    Node.debugCheck(longer_longer.value, longer_longer.value.*.height());
}


test "can concat long with lots of stuff" {
    //std.debug.print("\n\n\n", .{});
    const longer = try Node.fromSlice(longer_text, std.testing.allocator);
    defer cleanUpNode(longer);

    inline for (0..longer_text.len) |l| {
        const other = try Node.fromSlice(longer_text[0..l], std.testing.allocator);
        defer cleanUpNode(other);

        const longer_other = try Node.concat(longer, other, std.testing.allocator);
        defer cleanUpNode(longer_other);

        var iter = longer_other.value.*.allBytesIterator();
        for (longer_text ++ longer_text[0..l]) |expected| {
            const actual = iter.next();
            try expect(actual != null);
            try expect(expected == actual orelse unreachable);
        }
        try expect(iter.next() == null);

        //recPrintRefcounts(longer_other, 0);

        Node.debugCheck(longer_other.value, longer_other.value.*.height());
    }
}


test "no leaks when short concat with failing allocator" {
    var failing_allocator = std.testing.FailingAllocator.init(
        std.testing.allocator,
        .{.fail_index = 2}
    );
    const alloc = failing_allocator.allocator();
    const ab = Node.fromSlice("ab", alloc) catch return;
    defer cleanUpNode(ab);


    const ab2 = Node.fromSlice("ab", alloc) catch return;
    defer cleanUpNode(ab2);

    const ab_ab2 = Node.concat(ab, ab2, alloc) catch return;
    defer cleanUpNode(ab_ab2);
}

test "No leaks in concat with different branch factors" {
    inline for (2..10) |bf| {
        const CustomNode = NodeBF(bf);
        // var failing_allocator = std.testing.FailingAllocator.init(
        //     std.testing.allocator,
        //     .{.fail_index = 2}
        // );
        //const alloc = failing_allocator.allocator();
        const longer = try CustomNode.fromSlice(longer_text, std.testing.allocator);
        defer longer.releaseWithFn(CustomNode.deinit);

        const longer2 = CustomNode.fromSlice(longer_text, std.testing.allocator) catch return;
        defer longer2.releaseWithFn(CustomNode.deinit);

        const longer_longer = CustomNode.concat(longer, longer2, std.testing.allocator) catch return;
        defer longer_longer.releaseWithFn(CustomNode.deinit);
    }
}

test "concat looks correct with different branch factors" {
    @setEvalBranchQuota(10000);
    inline for (2..10) |bf| {
        const CustomNode = NodeBF(bf);
        const longer = try CustomNode.fromSlice(longer_text, std.testing.allocator);
        defer longer.releaseWithFn(CustomNode.deinit);

        inline for (0..longer_text.len) |l| {
            const other = try CustomNode.fromSlice(longer_text[0..l], std.testing.allocator);
            defer other.releaseWithFn(CustomNode.deinit);


            const longer_other = CustomNode.concat(longer, other, std.testing.allocator) catch return;
            defer longer_other.releaseWithFn(CustomNode.deinit);

            var iter = longer_other.value.*.allBytesIterator();
            for (longer_text ++ longer_text[0..l]) |expected| {
                const actual = iter.next();
                try expect(actual != null);
                try expect(expected == actual orelse unreachable);
            }
            try expect(iter.next() == null);
            CustomNode.debugCheck(longer_other.value, longer_other.value.*.height());
        }
    }
}

test "concat doesn't leak at allocator failure" {
    //@setEvalBranchQuota(10000);

    const CustomNode = NodeBF(2);
    const longer = try CustomNode.fromSlice(longer_text, std.testing.allocator);
    defer longer.releaseWithFn(CustomNode.deinit);

    const shorter = try CustomNode.fromSlice("ab", std.testing.allocator);
    defer shorter.releaseWithFn(CustomNode.deinit);

    for (2..10) |l| {
        var failing_allocator = std.testing.FailingAllocator.init(
            std.testing.allocator,
            .{.fail_index = l}
        );
        const alloc = failing_allocator.allocator();

        const longer_other = CustomNode.concat(longer, shorter, alloc) catch b: {
            std.debug.print("Expected refcount is 1, actual is: {}, l is: {}\n", .{longer.strongCount(), l});
            //CustomNode.recPrintRefcounts(longer, 0);
            try expect(longer.strongCount() == 1);
            break :b try CustomNode.fromSlice("", std.testing.allocator);
        };

        defer longer_other.releaseWithFn(CustomNode.deinit);
        // CustomNode.recPrintRefcounts(longer, 0);
        // CustomNode.recPrintRefcounts(longer_other, 0);
        //std.debug.print("\n\n\n", .{});
    }
}


test "Can split ab" {
    //@setEvalBranchQuota(10000);
    const node = try Node.fromSlice("ab", std.testing.allocator);
    defer node.releaseWithFn(Node.deinit);
    const splt = try Node.splitAt(node, 1, std.testing.allocator);
    defer splt.fst.releaseWithFn(Node.deinit);
    defer splt.snd.releaseWithFn(Node.deinit);

    try expect(splt.fst.value.*.agg.num_bytes == 1);
    try expect(splt.snd.value.*.agg.num_bytes == 1);
}


test "splits ab into a and b" {
    //@setEvalBranchQuota(10000);
    const node = try Node.fromSlice("ab", std.testing.allocator);
    defer node.releaseWithFn(Node.deinit);
    const splt = try Node.splitAt(node, 1, std.testing.allocator);
    defer splt.fst.releaseWithFn(Node.deinit);
    defer splt.snd.releaseWithFn(Node.deinit);

    var a_iter = splt.fst.value.*.allBytesIterator();
    var b_iter = splt.snd.value.*.allBytesIterator();

    try expect(a_iter.next() == 'a');
    try expect(a_iter.next() == null);

    try expect(b_iter.next() == 'b');
    try expect(b_iter.next() == null);
}


