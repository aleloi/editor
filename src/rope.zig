//! Rope data structure implementation and tests. A Rope is a
//! balanced tree with a constant branching factor. It's useful for
//! e.g. implementing fast text editing operations on large files. A
//! rope has O(max depth) operations for indexing, concatenation with
//! other ropes and splitting into two. The API of this rope treat all
//! ropes as immutable. All operations (indexing, splitting,
//! concatenation) return new ropes that may have internal shared
//! pointers pointing to the input ropes.
const std = @import("std");

// https://github.com/Aandreba/zigrc. Currently using single-threaded
// Rc. Guard refcount-changing operations with a lock if doing
// multithreading.
const Rc = @import("forked_rc.zig").Rc;

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

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const gpa_alloc = gpa.allocator();

/// Standard node side (TODO it's supposed to perform better when
/// matching cache lines. Read up on it and do some benchmarks! Should
/// probably ensure they are memory-aligned and with a different
/// size.). Keep nodes in Rc(Node), and release with
/// `node.releaseWithFn(Node.deinit)`.
pub const Node = NodeBF(16, gpa_alloc);

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

/// A rope tree node. Each node aggregate stats for all the bytes
/// stored in descendants. The node itself is either a Leaf or an
/// Inner node; Inner nodes contain refcounted pointers to child
/// nodes. Keep nodes in Rc(Self), and release with
/// `node.releaseWithFn(Self.deinit)`.
pub fn NodeBF(branch_factor: comptime_int,
              alloc_arg: std.mem.Allocator) type {
    return struct {
        const BRANCH_FACTOR: usize = branch_factor;
        const Self = @This();
        pub const RcSelf = Rc(Self, alloc);
        const alloc: std.mem.Allocator = alloc_arg;

        // Make the leafs have same size as the inner nodes to not waste memory.
        const LEAF_SIZE: usize = BRANCH_FACTOR * @sizeOf(Rc(void, alloc_arg))-1;

        /// Max depth is enough to store 1TB data. Used for Iterator
        /// stack-based recursion. NOTE that the tree need not use the full
        /// available memory - some leaf nodes can store less that
        /// BRANCH_FACTOR bytes, so the tree may be larger.
        const MAX_DEPTH: comptime_int = blk: {
            // 1TB = 10**12 bytes.
            const maxFileSizeBytes: comptime_int = std.math.powi(usize, 10, 12) catch unreachable;
            break :blk std.math.log_int(usize, BRANCH_FACTOR, maxFileSizeBytes / LEAF_SIZE) + 3;
        };

        const Leaf = std.BoundedArray(u8, LEAF_SIZE);
        const Inner = RcArray(BRANCH_FACTOR);

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
        pub const Iterator = struct {
            /// Note the *MAX_DEPTH*. Will crash if you try to iterate
            /// over a tree deeper than that. Should actually be quite
            /// safe, as MAX_DEPTH is enough for a tree holding a
            /// couple TB.
            rec_stack: std.BoundedArray(struct { node: *const Self, kid_index: usize }, MAX_DEPTH),

            /// Next byte or null if it has reached the end.
            pub fn next(self: *@This()) ?u8 {
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

        /// Recursively computes the height. Doesn't have to be
        /// public, but used in the API tests.
        pub fn height(self: *const @This()) usize {
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
        /// their d-tors if they are deallocated. `self` is value
        /// because this function is passed to
        /// `Rc.releaseWithFn`. It's NOT safe to use a node after
        /// passing it to `deinit`; Inner nodes still retain kid
        /// pointers that may have been deallocated.
        pub fn deinit(self: @This()) void {
            switch (self.node) {
                .leaf => |_| return,
                .inner => |kids| {
                    // Copies the ds containing the kid POINTERS to a
                    // mutable data structure so they can be released.
                    var kids_v = kids;
                    kids_v.release();
                },
            }
        }

        /// Main constructor. Constructs a new refcounted tree from a
        /// byte slice.
        pub fn fromSlice(slice: []const u8) !RcSelf {
            // Compute depth of resulting tree; needed to make sure all
            // leafs will be on the same level.
            var depth: usize = 0;
            var sz: usize = LEAF_SIZE;
            while (sz < slice.len) {
                sz *= BRANCH_FACTOR;
                depth += 1;
            }
            return fromSliceDepth(slice, depth);
        }

        ///TODO: test with failing allocator! Ensures that the tree
        ///has provided depth. It's needed to put all leaves on the same
        ///level in recursive calls. Recursive.
        fn fromSliceDepth(slice: []const u8, depth: usize) !RcSelf {
            // Assert that the slice will fit in a tree of the target depth.
            const max_size: usize = try std.math.powi(usize, BRANCH_FACTOR, depth) * LEAF_SIZE;
            std.debug.assert(slice.len <= max_size);

            if (slice.len <= LEAF_SIZE) {
                // Only error is overflow, which cannot happen under len <= BRANCH_FACTOR.
                const leaf = Leaf.fromSlice(slice) catch unreachable;
                const agg = AggregateStats.fromSlice(slice);
                const n = Self{ .node = .{ .leaf = leaf }, .agg = agg };
                var res = try RcSelf.init(n);
                errdefer res.releaseWithFn(Self.deinit);

                // All leafs must be on the same level; From this, it
                // follows that we sometimes need a chain
                // node--node--node--leaf. This loop adds the chain of
                // parent nodes to reach the target depth.
                for (0..depth) |_| {
                    var res_inner = Inner{};
                    errdefer res_inner.release();
                    try res_inner.push(res);

                    res.releaseWithFn(Self.deinit);
                    const res_node = Self{ .node = .{ .inner = res_inner }, .agg = agg };
                    res = try RcSelf.init(res_node);
                }
                std.debug.assert(res.value.*.height() == depth);
                return res;
            }

            // Split into B evenly sized pieces, but prefer LEAF_SIZE-sized leaf nodes.
            const max_piece_size: usize = @max(try std.math.divCeil(usize, slice.len, BRANCH_FACTOR),
                                               LEAF_SIZE);
            var piece_start: usize = 0;
            var inner: Inner = .{};
            defer inner.release();
            while (piece_start < slice.len) {
                const piece_end = @min(slice.len, piece_start + max_piece_size);
                const node_rc = try Self.fromSliceDepth(slice[piece_start..piece_end], depth - 1);
                try inner.push(node_rc);
                node_rc.releaseWithFn(Self.deinit);
                piece_start = piece_end;
            }

            return try Self.innerFromSlice(inner.arr.slice());
        }

        /// For (rather slowly) iterating though all bytes in the tree
        /// rooted at `self`. Amortized O(1) per byte if the tree is
        /// properly balanced, but recursive, slow-ish, and crashes on
        /// very very deep tres.
        pub fn allBytesIterator(self: *const @This()) Iterator {
            var res: Iterator = .{ .rec_stack = .{} };
            // Safe, MAX_DEPTH is guaranteed to be > 0.
            res.rec_stack.append(.{ .node = self, .kid_index = 0 }) catch unreachable;
            return res;
        }

        /// Creates a new tree that contains concatenated data from `self`
        /// and then `other`, in that order. The new tree will contain
        /// pointers to nodes of `self` and `other`; refcounts will be
        /// incremented. Will allocate `O(max(self.height(),
        /// other.height()))` new nodes. Returns a refcounted pointer to
        /// the new tree. Release it with `result.releaseWithFn(Self.deinit)`
        pub fn concat(self: RcSelf, other: RcSelf) !RcSelf {
            if (self.value.*.agg.num_bytes == 0) {
                var o = other;
                return o.retain();
            }
            if (other.value.*.agg.num_bytes == 0) {
                var self_v = self;
                return self_v.retain();
            }

            // Two cases based on the heights for keeping the tree
            // balanced. Algorithm explained in
            // https://youtu.be/fbVSXfbNB0M.
            if (self.value.*.height() > other.value.*.height()) {
                return Self.concatWithLower(self, other);
            } else {
                return Self.concatWithHigher(self, other);
            }
        }

        /// Returns two new trees, the first containing
        /// bytes[0..offset], the second with bytes[offset..]. Returns
        /// `PosError.InvalidPos` when passed a too large
        /// offset. Tested not to leak on different branch factors and
        /// with a failing allocator.
        pub fn splitAt(self: RcSelf, offset: usize) !struct { fst: RcSelf, snd: RcSelf } {
            if (offset > self.value.*.agg.num_bytes) {
                return error.InvalidPos;
            }
            switch (self.value.*.node) {
                .leaf => |leaf_v| {
                    const fst = try Self.fromSlice(leaf_v.slice()[0..offset]);
                    errdefer fst.releaseWithFn(Self.deinit);

                    const snd = try Self.fromSlice(leaf_v.slice()[offset..]);
                    return .{ .fst = fst, .snd = snd };
                },

                .inner => |inner_v| {
                    var start: usize = 0;
                    const kids = inner_v.arr.slice();
                    for (kids, 0..) |kid, i| {
                        // Find the kid we should split along.
                        const kid_size: usize = kid.value.*.agg.num_bytes;
                        if (start <= offset and offset <= start + kid_size) {
                            // Found the kid, now split recursively,
                            // and combine the results with .concat.
                            const kid_split = try splitAt(kid, offset - start);
                            defer kid_split.fst.releaseWithFn(Self.deinit);
                            defer kid_split.snd.releaseWithFn(Self.deinit);

                            // Avoid creating empty Inner nodes; well
                            // formed trees always end in Leafs.
                            const a = if (i > 0) try Self.innerFromSlice(kids[0..i]) else try Self.fromSlice("");
                            defer a.releaseWithFn(Self.deinit);

                            const b = if (i + 1 < kids.len) try Self.innerFromSlice(kids[(i + 1)..]) else try Self.fromSlice("");
                            defer b.releaseWithFn(Self.deinit);

                            // Combine the recursion results with the other kids.
                            const a_kid_a = try Self.concat(a, kid_split.fst);
                            errdefer a_kid_a.releaseWithFn(Self.deinit);

                            const kid_b_b = try Self.concat(kid_split.snd, b);
                            return .{ .fst = a_kid_a, .snd = kid_b_b };
                        }
                        start += kid_size;
                    }
                    // Shouldn't be possible, since we check the offset first.
                    unreachable;
                },
            }
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
        fn innerFromSlice(kids: []const RcSelf) !RcSelf {
            std.debug.assert(kids.len <= BRANCH_FACTOR);
            std.debug.assert(kids.len > 0);
            var inner = try Self.Inner.fromSlice(kids);
            errdefer inner.release();

            var agg = AggregateStats.empty();

            for (kids) |kid| {
                agg = agg.combine(kid.value.*.agg);
            }

            return try RcSelf.init(Self{ .node = .{ .inner = inner }, .agg = agg });
        }

        /// Wrapping struct around BoundedArray(Rc) for keeping track of
        /// refcounts. Increases on push, decreases on pop.
        fn RcArray(sz: usize) type {
            return struct {
                arr: std.BoundedArray(RcSelf, sz) = .{},

                fn fromSlice(kids: []const RcSelf) !@This() {
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

                fn push(self: *@This(), n: RcSelf) !void {
                    var n_copy = n;
                    try self.arr.append(n_copy.retain());
                }

                // Don't return the value because it might have been released.
                fn pop(self: *@This()) void {
                    std.debug.assert(self.arr.len > 0);
                    self.arr.pop().releaseWithFn(Self.deinit);
                }

                fn top(self: *const @This()) RcSelf {
                    std.debug.assert(self.arr.len > 0);
                    return self.arr.get(self.arr.len - 1);
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
        fn merge(a: Self, b: Self) !RcSelf {

            std.debug.assert(a.height() == b.height());
            std.debug.assert(b: {
                const a_tag: NodeTag = a.node;
                const b_tag: NodeTag = b.node;
                break :b a_tag == b_tag;
            });

            // Should be possible to remove duplication with
            // comptime. TODO! Also inefficient for leafs.
            switch (a.node) {
                .leaf => |a_leaf| {
                    std.debug.assert(a.numKidsOrBytes() + b.numKidsOrBytes() <= LEAF_SIZE);
                    const b_leaf: Leaf = b.assertExtractLeaf();
                    var res_leaf: Leaf = .{};
                    for (a_leaf.slice()) |p| {
                        res_leaf.append(p) catch unreachable;
                    }
                    for (b_leaf.slice()) |p| {
                        res_leaf.append(p) catch unreachable;
                    }
                    const agg = AggregateStats.fromSlice(res_leaf.slice());
                    const res = Self{
                        .agg = agg,
                        .node = .{ .leaf = res_leaf },
                    };
                    return try RcSelf.init(res);
                },
                .inner => |a_inner| {
                    std.debug.assert(a.numKidsOrBytes() + b.numKidsOrBytes() <= BRANCH_FACTOR);
                    // A bit inefficient: we push everything into
                    // `res_inner` and then copy it over into a new
                    // `Inner` inside the Self constructor, then pop
                    // everything back though `.release()`. All this
                    // to re-use the agg combine block in
                    // .innerFromSlice.
                    const b_inner: Inner = b.assertExtractInner();
                    var res_inner: Inner = .{};
                    defer res_inner.release();
                    for (a_inner.arr.slice()) |p| {
                        res_inner.push(p) catch unreachable;
                    }
                    for (b_inner.arr.slice()) |p| {
                        res_inner.push(p) catch unreachable;
                    }
                    return try Self.innerFromSlice(res_inner.arr.slice());
                },
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
        fn concatWithLower(left_p: RcSelf, right_p: RcSelf) !RcSelf {
            const h_right = left_p.value.*.height();
            const h_left = right_p.value.*.height();
            std.debug.assert(h_right >= h_left);

            // Terminology: left merge node - the rightmost node of the
            // left tree which is at the same level as the right root.

            // Step (1): construct a chain of nodes from the root to the left merge
            // node inclusive. Refcounts are incremented for things we put
            // in the array and freed on pop/release.

            var leftDownRight: RcArray(MAX_DEPTH) = getRootDown(left_p, h_right - h_left + 1, .right);
            defer leftDownRight.release();

            // Step (2): pad the right node with a bunch of parents until
            // it can be merged with the left node. Pop from leftDownRight so
            // that left=leftDownRight[-1] has the same height as right.

            // We take co-ownership of `right` for this function. When we
            // wrap `right` in parents, we will give away ownership to the parent.

            const right = try prepareForMerge(right_p, &leftDownRight);
            defer right.releaseWithFn(Self.deinit);

            // NOTE: Allocated by this function and therfore owned by
            // it. Thefore released before returning.
            const merged = try Self.merge(leftDownRight.top().value.*, right.value.*);
            defer merged.releaseWithFn(Self.deinit);
            debugCheck(leftDownRight.top().value, leftDownRight.top().value.*.height());
            debugCheck(right.value, right.value.*.height());
            debugCheck(merged.value, merged.value.*.height());

            leftDownRight.pop();

            return try bubbleUpKid(merged, &leftDownRight, .right);
        }

        /// Helper function for `concatWith(Higher|Lower)`; produces a
        /// co-owned RcArray. The result contains a chain or parents
        /// (earlier nodes are parents of later) from root along the
        /// RIGHT- or LEFT-MOST border of the tree. The chain contains
        /// `depth` elements. Refcounts of all elements of the chain
        /// are increased, because the returned data structure co-owns
        /// them. Asserts that the tree is at most MAX_DEPTH (it will
        /// be if it's mostly balanced), and that the chain
        /// exists. This means that that all nodes in the chain except
        /// the last are parent nodes (i.e. Inner) and that all nodes
        /// have kids (they will have in a tree constructed by and
        /// modified by the public API).
        fn getRootDown(root: RcSelf, depth: usize, dir: enum { left, right }) RcArray(MAX_DEPTH) {
            std.debug.assert(depth <= MAX_DEPTH);

            var pos = root;
            var res = RcArray(MAX_DEPTH){};
            // Mostly redundant, as the only error that can happen is
            // too high depth, and we crash on that.
            errdefer res.release();

            // We don't want to assert that the last node in the chain
            // is Inner, hence first pushing outside the loop. Not
            // that push increases refcounts - it shares ownership of
            // the pointer with the RcArray container.
            res.push(pos) catch unreachable;
            for (0..depth - 1) |_| {
                const kids = pos.value.*.assertExtractInner();
                const kid_pos = if (dir == .left) 0 else kids.arr.len - 1;
                std.debug.assert(kids.arr.len > 0);
                pos = kids.arr.get(kid_pos);
                res.push(pos) catch panicAndExit("Can't push!", .{ res, pos });
            }
            return res;
        }

        fn canMerge(self: Self, other: Self) bool {
            std.debug.assert(b: {
                const a_tag: NodeTag = self.node;
                const b_tag: NodeTag = other.node;
                break :b a_tag == b_tag;
            });

            switch (self.node) {
                .leaf => return self.numKidsOrBytes() + other.numKidsOrBytes() <= LEAF_SIZE,
                .inner => return self.numKidsOrBytes() + other.numKidsOrBytes() <= BRANCH_FACTOR,
            }
        }

        /// Wraps `root` in newly created parents while popping from
        /// `chain` until the top of `chain` and the wrapped `root`
        /// together have at most BRANCH_FACTOR kids. This ensures
        /// that `root` and the `chain` top's kids/bytes can be merged
        /// together into a new node. Releases all newly created nodes
        /// on failure (but will still have popped from `chain` as a
        /// side effect even if it fails). Assumptions: `chain` is a
        /// chain of parent nodes par -> kid -> kids' kid -> ..., the
        /// end of `chain` has the same height as `root`.
        fn prepareForMerge(root: RcSelf, chain: *RcArray(MAX_DEPTH)) !RcSelf {
            var small = b: {
                var r = root;
                break :b r.retain();
            }; // Bumps small to N+1
            // TODO: Can this be done simpler? This is needed for the
            // `b` block below to handle refcounts correctly - this
            // function holds an EXTRA ref to the whole chain of
            // parents from `small` to `root`, and that extra refcount
            // is transfered to the new parent when wrapping `small`.
            errdefer small.releaseWithFn(Self.deinit); // now we can assume it's back at N.

            std.debug.assert(chain.*.arr.len > 0);
            while (!chain.*.top().value.canMerge(small.value.*)) {

                // Move ownership of the previous `small` into a newly
                // created parent, and assign to `small`. Invariant: we
                // still need to release `small` after this, which happens
                // though the defer above.
                small = b: {
                    const new_small = try innerFromSlice(&.{small});
                    // Now small is at N+2
                    small.releaseWithFn(Self.deinit); // back at N+1
                    break :b new_small;
                };
                if (chain.*.arr.len == 1) {
                    const newParent = try innerFromSlice(&.{chain.*.top()}); // RC=1
                    // Move ownership of `newParent` to `chain`.
                    chain.*.pop(); // Safe to release old top, as it's
                    // now co-owned by NewParent.
                    chain.*.push(newParent) catch unreachable; // RC=2
                    newParent.releaseWithFn(Self.deinit); // RC back at 1; owned by `chain`
                    // after this block, the sole owner is chain
                } else {
                    chain.*.pop(); // releases ref to .top(), which is
                    // fine since it's owned by the
                    // parent anyway.
                }
            }
            return small;
        }

        /// `chain` is an array of inner nodes root -> kid -> kid's
        /// kid -> ... Creates a new root node for a new tree where
        /// the leftmost kid has been replaced by `leftmost`. That is,
        /// take the last element of `chain` and replace it's leftmost
        /// kid with `leftmost`, then modify the rest of the
        /// chain. But nodes are immutable, so this creates a new
        /// parallel chain. The nodes in `chain` are released by
        /// .pop(). Allocated memory should be cleaned up on
        /// failure. Has ONE isolated test with a failing allocator,
        /// and is also more comprehensively tested though the .concat
        /// tests. Warning: it was pretty tricky to correctly keep
        /// track of the refcounts and release them on failure,
        /// dangerous to modify!
        fn bubbleUpKid(kid: RcSelf, chain: *RcArray(MAX_DEPTH), kid_pos: enum { left, right }) !RcSelf {
            // Usage:
            //   var res = try bubbleUpLeftmostKid(leftmost, &chain, alloc);

            // If we do `res.release()` we're back at where we
            // started, except that `chain` is now empty. So
            //    var res = try bubbleUpLeftmostKid(leftmost, &chain, alloc);
            //    res.releaseWithFn(deinit)
            // should be equilavent to
            //    chain.release();

            // When things go wrong, it should be a no-op provided
            // there is a `defer chain.release()` at the caller.

            // This fn co-owns `res`.
            var res = b: {
                var l = kid;
                break :b l.retain();
            }; // N+1
            errdefer res.releaseWithFn(Self.deinit);

            while (chain.arr.len > 0) {
                // The loop transfers ownership of `res` to a newly
                // created parent. Also safe during errors.

                // co-owning `par_kids`.
                var par_kids: RcArray(BRANCH_FACTOR) = chain.top().value.*.assertExtractInner().clone();
                defer par_kids.release(); //
                chain.pop(); // Safe, since we cloned what we need (increasing refcount).

                std.debug.assert(par_kids.arr.len > 0);
                {
                    // Give co-ownership of `res` to `par_kids`, and
                    // kick out `leftmost_kid`.
                    const kid_idx = if (kid_pos == .left) 0 else par_kids.arr.len - 1;
                    const edge_kid = par_kids.arr.get(kid_idx);
                    std.debug.assert(edge_kid.value.*.height() == res.value.*.height());
                    par_kids.arr.set(kid_idx, res.retain()); // now at N+2, will be back at N+1 after par_kids.release().
                    edge_kid.releaseWithFn(Self.deinit);
                }

                // Don't release the kid; it's still owned by its tree.
                const new_res = try Self.innerFromSlice(par_kids.arr.slice()); // Now at N+3
                res.releaseWithFn(Self.deinit); // Back at N+2. Emulates assignment operator.
                res = new_res;

                debugCheck(res.value, res.value.*.height());
                // N+1 thanks to par_kids.release(). ONLY owned by new_res which is now res.
            }
            return res;
        }

        /// Very similar to concatWithLower.
        fn concatWithHigher(left_p: RcSelf, right_p: RcSelf) !RcSelf {
            //std.debug.print("BEFORE concat\n", .{});

            const h_right = left_p.value.*.height();
            const h_left = right_p.value.*.height();
            std.debug.assert(h_right <= h_left);

            var rightDownLeft: RcArray(MAX_DEPTH) = getRootDown(right_p, h_left - h_right + 1, .left);
            defer rightDownLeft.release();
            std.debug.assert(rightDownLeft.arr.len == h_left - h_right + 1);

            std.debug.assert(rightDownLeft.top().value.*.height() == left_p.value.*.height());

            const left = try prepareForMerge(left_p, &rightDownLeft);
            defer left.releaseWithFn(Self.deinit);

            const merged = try Self.merge(left.value.*, rightDownLeft.top().value.*);
            defer merged.releaseWithFn(Self.deinit);
            debugCheck(rightDownLeft.top().value, rightDownLeft.top().value.*.height());
            debugCheck(left.value, left.value.*.height());
            debugCheck(merged.value, merged.value.*.height());

            rightDownLeft.pop();

            return try bubbleUpKid(merged, &rightDownLeft, .left);
        }

        /// Returned by e.g. the split and pos2offset functions.
        /// Could also add error.LineTooShort / error.TooFewLines?
        pub const PosError = error{
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
        pub fn posToOffset(self: *const @This(), p: Pos) PosError!usize {
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
                    // Find which kid contains `p: Pos`.
                    const kids: []const RcSelf = inner_p.arr.slice();
                    var start = Pos{ .col = 0, .row = 0 };
                    var bytes_in_kids: usize = 0;
                    for (kids) |kid| {
                        // Compute where this kid begins and ends in
                        // the parent's frame of reference.
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
                            // This kid contains `p`, now translate
                            // `p` into this kid's coordinates,
                            // recurse, and translate the result back.
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

        /// Exported because it's called from tests. Not sure how to
        /// hide it from the API without having to move all tests to
        /// this file. Checks that agg stats are correct, that all
        /// leafs are at the same height, and that all branches end
        /// with leafs.
        pub fn debugCheck(node: *const Self, expected_height: usize) void {
            if (!std.debug.runtime_safety) return;
            // Recurslively check that it's balanced:
            switch (node.*.node) {
                .leaf => {
                    std.debug.assert(expected_height == 0);
                },
                .inner => |kids| {
                    std.debug.assert(kids.arr.len > 0);
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

        fn recPrintRefcounts(node: RcSelf, indent: usize) void {
            for (0..indent) |_| {
                std.debug.print("  ", .{});
            }
            const node_tag: NodeTag = node.value.*.node;
            std.debug.print("rc={} ptr=0x{x} {s} {}", .{ node.strongCount(), @intFromPtr(node.value), @tagName(node_tag), node.value.*.agg });
            if (node_tag == .inner) {
                std.debug.print("\n", .{});
                for (node.value.*.assertExtractInner().arr.slice()) |kid_p| {
                    recPrintRefcounts(kid_p, indent + 1);
                }
            } else {
                std.debug.print(" '{s}'\n", .{node.value.*.assertExtractLeaf().slice()});
            }
        }
    };
}

// Tests of private functions below. Tests of public API in
// tests/rope_tests.zig.
const expect = std.testing.expect;
const TestNode = NodeBF(2, std.testing.allocator);

const longer_text =
    \\test string with some newlines
    \\\n definitely over 16 chars actually
    \\longer than 64 chars
    \\and some more text
    \\below to pad it out
    \\yes really
;

test "can create leaf node" {
    const leaf = TestNode.Leaf{};
    _ = TestNode{ .node = .{ .leaf = leaf }, .agg = AggregateStats.empty() };
}



/// Creates refcounted leaf node; should have a single strong pointer.
fn leafNodeForTest() !TestNode.RcSelf {
    const leaf_content = "ab";
    std.debug.assert(leaf_content.len <= TestNode.LEAF_SIZE);
    const leaf = try TestNode.Leaf.fromSlice(leaf_content);
    const n = TestNode{ .node = .{ .leaf = leaf }, .agg = AggregateStats.empty() };
    return try TestNode.RcSelf.init(n);
}

/// Creates a refcounted inner node with a single leaf node; both have
/// 1 strong pointer each.
fn innerNodeForTest() !TestNode.RcSelf {
    const leaf = try leafNodeForTest();

    // release the kid pointer unconditionally, independently of
    // whether parent allocation fails. If it succeeds, the leaf
    // release will not deallocate the leaf, as parent creation
    // increases the refcount.
    defer leaf.releaseWithFn(TestNode.deinit);
    const parent = try TestNode.innerFromSlice(&.{leaf});
    return parent;
}

test "can call iterator and next" {
    const leaf = try leafNodeForTest();
    defer leaf.releaseWithFn(TestNode.deinit);
    var iter = leaf.value.*.allBytesIterator();
    _ = iter.next();
}

fn cleanUpNode(node: TestNode.RcSelf) void {
    node.releaseWithFn(TestNode.deinit);
}

test "print max depth" {
    // Apparently it's 21 when B is 4 and MAX_DEPTH=11 when B is 16.
    std.debug.print("MAX_DEPTH: {}\n", .{Node.MAX_DEPTH});
    std.debug.print("BRANCH_FACTOR: {}\n", .{Node.BRANCH_FACTOR});
    std.debug.print("LEAF_SIZE: {}\n", .{Node.LEAF_SIZE});
}

test "can create leaf node with refcount no leaks" {
    const node = try leafNodeForTest();
    cleanUpNode(node);
}

test "leaf node height is 0" {
    const node = try leafNodeForTest();
    defer node.releaseWithFn(TestNode.deinit);
    try expect(node.value.*.height() == 0);
}

test "fromSlice produces sinlge leaf on small strings" {
    const small_buf: [TestNode.LEAF_SIZE] u8 = undefined;
    const node = try TestNode.fromSlice(&small_buf);
    defer cleanUpNode(node);

    const height = node.value.*.height();
    try expect(height == 0);
    node.value.*.debugCheck(height);
}


test "fromSlice produces inner with leaf parents on medium strings" {
    const medium_buf: [TestNode.LEAF_SIZE * TestNode.BRANCH_FACTOR] u8 = undefined;
    const node = try TestNode.fromSlice(&medium_buf);
    defer cleanUpNode(node);

    const height = node.value.*.height();
    try expect(height == 1);
    node.value.*.debugCheck(height);
}

test "fromSlice produces depth 2 nodes on longer strings" {
    const medium_buf: [TestNode.LEAF_SIZE * TestNode.BRANCH_FACTOR + 1] u8 = undefined;
    const node = try TestNode.fromSlice(&medium_buf);
    defer cleanUpNode(node);

    const height = node.value.*.height();
    try expect(height == 2);
    node.value.*.debugCheck(height);
}

test "parent node height is 1" {
    const leaf = try leafNodeForTest();
    defer leaf.releaseWithFn(TestNode.deinit);
    var inner = TestNode.Inner{};
    try inner.push(leaf);

    const parent_node = TestNode{ .node = .{ .inner = inner }, .agg = AggregateStats.empty() };
    defer parent_node.deinit();

    try expect(parent_node.height() == 1);
}

test "can create inner node no leaks" {
    const inner: TestNode.RcSelf = try innerNodeForTest();
    cleanUpNode(inner);
}

test "numKidsOrBytes leaf is num bytes" {
    const leaf_content = "ab";
    const leaf = try TestNode.Leaf.fromSlice(leaf_content);
    const n = TestNode{ .node = .{ .leaf = leaf }, .agg = AggregateStats.empty() };
    try expect(n.numKidsOrBytes() == leaf_content.len);
}

test "numKidsOrBytes inner is num kids" {
    const leaf = try leafNodeForTest();
    defer leaf.releaseWithFn(TestNode.deinit);
    var inner = TestNode.Inner{};
    try inner.push(leaf);
    const inner_node = TestNode{ .node = .{ .inner = inner }, .agg = AggregateStats.empty() };
    defer inner_node.deinit();

    try expect(inner_node.numKidsOrBytes() == 1);
}

test "RootDownRight doesn't leak" {
    const lmost = try TestNode.fromSlice("ab");
    defer lmost.releaseWithFn(TestNode.deinit);

    try expect(lmost.value.*.node == .leaf);

    const tree = try TestNode.fromSlice(longer_text ++ longer_text);
    defer tree.releaseWithFn(TestNode.deinit);

    const height = tree.value.*.height();
    try expect(height > 0);

    var chain = TestNode.getRootDown(tree, tree.value.*.height(), .left);
    defer chain.release();
}

var failing_allocator = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = 8 });
const failing_alloc = failing_allocator.allocator();


// TODO it fails in the wrong place:
test "bubbleUpKid doesn't leak on long array" {
    const CustomNode = NodeBF(2, failing_alloc);
    const lmost = try CustomNode.fromSlice("a");
    defer lmost.releaseWithFn(CustomNode.deinit);

    try expect(lmost.value.*.node == .leaf);

    const tree = CustomNode.fromSlice(longer_text) catch return;
    defer tree.releaseWithFn(CustomNode.deinit);

    var chain = CustomNode.getRootDown(tree, tree.value.*.height(), .left);
    defer chain.release();


    const new_root = CustomNode.bubbleUpKid(lmost, &chain, .left) catch {
        // idempotent and safe
        chain.release();
        try expect(tree.strongCount() == 1);
        return;
        //break :b try CustomNode.fromSlice("");
    };

    defer new_root.releaseWithFn(CustomNode.deinit);
}

test "B at least 2" {
    // Can't store data in a tree with branching factor < 2.
    try comptime expect(Node.BRANCH_FACTOR >= 2);
}

test "print sizes and offsets" {
    std.debug.print("Sizef and offsets:\n-------\n", .{});
    std.debug.print("sizeof(Node): {},\t sizeof(Leaf): {},\t sizeof(Inner): {}\n",
                    .{@sizeOf(Node), @sizeOf(Node.Leaf), @sizeOf(Node.Inner)});

    std.debug.print("sizeof(Rc(u8)): {},\t sizeof(Rc(u16)): {},\t sizeof(Rc(u64)): {}\n",
                    .{@sizeOf(Rc(u8, std.testing.allocator)), @sizeOf(Rc(u16, std.testing.allocator)),
                      @sizeOf(Rc(u64, std.testing.allocator))});

    std.debug.print("sizeof(AggregateStats): {}\n",
                    .{@sizeOf(AggregateStats)});

    const szN = @sizeOf(Node);

    std.debug.print("offset(Node.agg): {}/{}\t offset(Node.node): {}/{} \n",
                    .{@offsetOf(Node, "agg"), szN, @offsetOf(Node, "node"), szN});

    std.debug.print("offset(Rc.value): {}/{}\n",
                    .{@offsetOf(Node.RcSelf, "value"), @sizeOf(Node.RcSelf),
                      //@offsetOf(Rc(Node), "alloc"), @sizeOf(Rc(Node)),
                      });

    std.debug.print("sizeof(std.mem.Allocator): {}\n",
                    .{@sizeOf(std.mem.Allocator)});

    var node: Node = .{.agg=undefined, .node = .{.leaf = undefined}};
    const leaf_ptr: *Node.Leaf = b: {
        switch (node.node) {
            .leaf => |*l| break :b l,
            .inner => unreachable,
        }
    };

    node.node = .{.inner = undefined};
    const inner_ptr: *Node.Inner = b: {
        switch (node.node) {
            .leaf => unreachable,
            .inner => |*i| break :b i,
        }
    };

    const node_ptr = &node;
    const leaf_offset: usize = @intFromPtr(leaf_ptr) - @intFromPtr(node_ptr);
    const inner_offset: usize = @intFromPtr(inner_ptr) - @intFromPtr(node_ptr);
    std.debug.print("Leaf offset (not guaranteed): {}/{}\t Inner offset (same): {}/{}\n",
                    .{leaf_offset, szN, inner_offset, szN});

    //std.debug.print("at #c3778df, the tree is huge")
}


var buf: [1_000_000_000] u8 = undefined;
var fixed_buf_alloc = std.heap.FixedBufferAllocator.init(&buf);
const fb_alloc = fixed_buf_alloc.allocator();
const CustomNodeFixed = NodeBF(16, fb_alloc);

const CountNodesRet = struct {inner: usize, leaf: usize};
fn countNodes(n: CustomNodeFixed.RcSelf) CountNodesRet {
    //var res = 1;
    switch (n.value.*.node) {
        .leaf => return .{.inner=0, .leaf=1},
        .inner => |kids| {
            // If I drop the type, zig thinks .inner, .leaf are comptime_int.
            var res: CountNodesRet = .{.inner=1, .leaf=0};
            for (kids.arr.slice()) |kid| {
                const kid_res = countNodes(kid);
                res.inner += kid_res.inner;
                res.leaf += kid_res.leaf;
            }
            return res;
        }
    }
}

test "memory usage okular inspection" {


    inline for (&.{//0,
        1, 10, 20, 50, 100,

        CustomNodeFixed.LEAF_SIZE,

        200, 500, 1000,
        CustomNodeFixed.LEAF_SIZE * CustomNodeFixed.BRANCH_FACTOR,

        10_000, 20_000, 50_000,
        CustomNodeFixed.LEAF_SIZE * CustomNodeFixed.BRANCH_FACTOR * CustomNodeFixed.BRANCH_FACTOR,

        100_000, 200_000, 500_000,
        CustomNodeFixed.LEAF_SIZE * CustomNodeFixed.BRANCH_FACTOR * CustomNodeFixed.BRANCH_FACTOR * CustomNodeFixed.BRANCH_FACTOR,

        1_000_000, 2_000_000, 5_000_000,

                   }) |sz| {
        fixed_buf_alloc.reset();
        var sz_buf: [sz] u8 = undefined;
        const nd = try CustomNodeFixed.fromSlice(&sz_buf);
        const nodes = countNodes(nd);
        const allocated_bytes = fixed_buf_alloc.end_index;
        const overhead_pct = allocated_bytes * 100 / sz;
        const avg_payload_per_leaf = sz / nodes.leaf;
        const avg_payload_per_node = sz / (nodes.leaf + nodes.inner);
        const used_bytes = (nodes.leaf + nodes.inner) * (@sizeOf(CustomNodeFixed) + @sizeOf(CustomNodeFixed.RcSelf));
        const used_pct = used_bytes * 100 / sz;
        std.debug.print(
            "size: {}\t allocated (%): {},\t used (%): {},\t inner: {},\t leafs: {},\t per leaf: {},\t per node: {} \n",
            .{sz, overhead_pct, used_pct, nodes.inner, nodes.leaf, avg_payload_per_leaf, avg_payload_per_node}
        );
    }
}
