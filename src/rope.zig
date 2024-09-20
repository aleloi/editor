//! rope, Rope data structure implementation and tests.
const std = @import("std");
const rc = @import("zigrc");

const BRANCH_FACTOR = 16;

/// Max depth is enough to store 1TB data. Used for Iterator
/// stack-based recursion. NOTE that the tree need not use the full
/// available memory - some leaf nodes can store less that
/// BRANCH_FACTOR bytes, so the tree may be larger.
const MAX_DEPTH: comptime_int = blk: {
    // 1TB = 10**12 bytes.
    const maxFileSizeBytes: comptime_int = std.math.powi(usize, 10, 12) catch unreachable;
    break :blk std.math.log_int(comptime_int, BRANCH_FACTOR, maxFileSizeBytes) + 2;
};

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

/// A rope tree node. Contains aggregate stats for all the bytes
/// stored in descendants. The node itself is either a Leaf or an
/// Inner node; Inner nodes contain refcounted pointers to child
/// nodes.
pub const Node = struct {
    /// Every tree node contains aggregate stats for all bytes in its
    /// descendents.
    const AggregateStats = struct {
        num_newlines: usize,
        num_bytes: usize,
        last_newline_pos: ?usize,

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

    const Leaf = std.BoundedArray(u8, BRANCH_FACTOR);
    const Inner = std.BoundedArray(rc.Rc(Node), BRANCH_FACTOR);

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
        rec_stack: std.BoundedArray(struct { node: *const Node, kid_index: usize }, MAX_DEPTH),

        // node: *const Node,
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
                    const kid_ptr: *const Node = kids.get(top.kid_index).value;

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
                std.debug.assert(kids.len > 0);
                return 1 + kids.buffer[0].value.*.height();
            },
        }
    }

    fn numKidsOrBytes(self: *const @This()) usize {
        switch (self.node) {
            .leaf => |leaf| return leaf.len,
            .inner => |kids| return kids.len,
        }
    }

    /// Recursively releases the refcounted kid pointers.
    pub fn deinit(self: *@This()) void {
        switch (self.node) {
            .leaf => |_| return,
            .inner => |*kids| {
                for (kids.*.slice()) |kid| {
                    kid.value.*.deinit();
                    kid.release();
                }
                // clear the content; BoundedArray.clear() is >=0.14 or something.
                kids.*.len = 0;
            },
        }
    }

    pub fn fromSlice(slice: []const u8, alloc: std.mem.Allocator) !rc.Rc(Node) {
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

    fn fromSliceDepth(slice: []const u8, depth: usize, alloc: std.mem.Allocator) !rc.Rc(Node) {
        // Assert that the slice will fit in a tree of the target depth.
        const max_size: usize = try std.math.powi(usize, BRANCH_FACTOR, depth + 1);
        std.debug.assert(slice.len <= max_size);

        if (slice.len <= BRANCH_FACTOR) {
            // Only error is overflow, which cannot happen under len <= BRANCH_FACTOR.
            const leaf = Leaf.fromSlice(slice) catch unreachable;
            const agg = AggregateStats.fromSlice(slice);
            const n = Node{ .node = .{ .leaf = leaf }, .agg = agg };
            var res = try rc.Rc(Node).init(alloc, n);

            // All leafs must be on the same level; From this, it
            // follows that we sometimes need a chain
            // node--node--node--leaf. This loop adds the chain of
            // parent nodes to reach the target depth.
            for (0..depth) |_| {
                const res_kid: [1]rc.Rc(Node) = .{res};
                const res_inner = try Inner.fromSlice(&res_kid);
                const res_node = Node{ .node = .{ .inner = res_inner }, .agg = agg };
                res = try rc.Rc(Node).init(alloc, res_node);
            }
            std.debug.assert(res.value.*.height() == depth);
            return res;
        }

        // Split into B evenly sized pieces, but prefer BRANCH_FACTOR-sized leaf nodes.
        const max_piece_size: usize = @max(try std.math.divCeil(usize, slice.len, BRANCH_FACTOR), BRANCH_FACTOR);
        var piece_start: usize = 0;
        var inner: Inner = .{};
        while (piece_start < slice.len) {
            const piece_end = @min(slice.len, piece_start + max_piece_size);
            const node_rc = try Node.fromSliceDepth(slice[piece_start..piece_end], depth - 1, alloc);
            try inner.append(node_rc);
            piece_start = piece_end;
        }

        var agg: AggregateStats = Node.AggregateStats.empty();
        for (inner.slice()) |kid| {
            agg = agg.combine(kid.value.*.agg);
        }
        const n = Node{ .node = .{ .inner = inner }, .agg = agg };
        return try rc.Rc(Node).init(alloc, n);
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
    pub fn concat(self: @This(), other: @This(), alloc: std.mem.Allocator) !rc.Rc(@This()) {
        if (self.agg.num_bytes == 0) {
            return try rc.Rc(Node).init(alloc, other);
        }
        if (other.agg.num_bytes == 0) {
            return try rc.Rc(Node).init(alloc, self);
        }

        if (self.height() >= other.height()) {
            return self.concatWithLower(other, alloc);
        } else {
            return self.concatWithHigher(other, alloc);
        }
    }

    /// Crashes on failure. Maybe make an error?
    fn assertExtractInner(self: @This()) Inner {
        switch (self.node) {
            .inner => |inner_v| return inner_v,
            .leaf => panicAndExit("Logic error, we thought this was an inner node", .{self}),
        }
    }

    /// Concatenation implementation, left big with right small. Also
    /// see the corresponding code in Node._concat_lower in
    /// rope.py. Overall algorithm explained in
    /// https://youtu.be/fbVSXfbNB0M (in Russian, with mutable nodes
    /// rather than immutable as here, and no code). Idea: think the
    /// trees side by side resting on the leafs. Find the rightmost
    /// node (in self) at the same level as the root of the left tree,
    /// and keep the path from the right root (self) to that. Walk up
    /// the chain until you find a node that can be merged with the
    /// right side, then merge and create a new chain or
    /// parents. Sometimes the resulting tree height increases by 1.
    fn concatWithLower(self: @This(), right: @This(), alloc: std.mem.Allocator) !rc.Rc(@This()) {
        const h_right = self.height();
        const h_left = right.height();
        std.debug.assert(h_right >= h_right);

        // Terminology: left merge node - the rightmost node of the
        // left tree which is at the same level as the right root.

        // Constructs a chain of nodes from the root to the left
        // merge node inclusive.
        var rootToLeft: std.BoundedArray(Node, MAX_DEPTH) = .{};
        var left = self;
        rootToLeft.append(left) catch unreachable;
        for (0..h_left - h_right) |_| {
            std.debug.assert(left.numKidsOrBytes() > 0);
            const left_kids = left.assertExtractInner();
            left = left_kids.get(left_kids.len - 1).value.*;
            rootToLeft.append(left) catch
                panicAndExit("Can't append, depth too high!\n", .{rootToLeft});
        }

        // Check that we got the correct height:
        std.debug.assert(left.height() == right.height());

        // TODO! We are at line 178 of the python code. Think about
        // what to do with the refcounts! Should rootToLeft contain
        // rc? but self can't be rc!

        //_ = rootToLeft;
        // _ = self;
        // _ = other;
        _ = alloc;
        unreachable;
    }

    fn concatWithHigher(self: @This(), other: @This(), alloc: std.mem.Allocator) !rc.Rc(@This()) {
        _ = self;
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
                const kids: []const rc.Rc(Node) = inner_p.slice();
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
                                // To tired to think about this; it's
                                // in the python code.
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
};

const expect = std.testing.expect;

test "B at least 2" {
    // Can't store data in a tree with branching factor < 2.
    try comptime expect(BRANCH_FACTOR >= 2);
}

test "can create leaf node" {
    const leaf = Node.Leaf{};
    _ = Node{ .node = .{ .leaf = leaf }, .agg = Node.AggregateStats.empty() };
}

/// Creates refcounted leaf node; should have a single strong pointer.
fn leafNodeForTest(alloc: std.mem.Allocator) !rc.Rc(Node) {
    const leaf_content = "abc";
    const leaf = try Node.Leaf.fromSlice(leaf_content);
    const n = Node{ .node = .{ .leaf = leaf }, .agg = Node.AggregateStats.empty() };
    return try rc.Rc(Node).init(alloc, n);
}

/// Creates a refcounted inner node with a single leaf node; both have
/// 1 strong pointer each.
fn innerNodeForTest(alloc: std.mem.Allocator) !rc.Rc(Node) {
    const leaf = try leafNodeForTest(alloc);
    var inner = try Node.Inner.init(1);
    inner.set(0, leaf);
    const inner_node = Node{ .node = .{ .inner = inner }, .agg = Node.AggregateStats.empty() };
    return try rc.Rc(Node).init(alloc, inner_node);
}

fn cleanUpNode(node: rc.Rc(Node)) void {
    node.value.*.deinit();
    node.release();
}

test "can create leaf node with refcount no leaks" {
    const node = try leafNodeForTest(std.testing.allocator);
    cleanUpNode(node);
}

test "leaf node height is 0" {
    const node = try leafNodeForTest(std.testing.allocator);
    defer node.release();
    try expect(node.value.*.height() == 0);
}

test "parent node height is 1" {
    const leaf: [1]rc.Rc(Node) = .{try leafNodeForTest(std.testing.allocator)};
    const inner = try Node.Inner.fromSlice(&leaf);
    var parent_node = Node{ .node = .{ .inner = inner }, .agg = Node.AggregateStats.empty() };
    defer parent_node.deinit();

    try expect(parent_node.height() == 1);
}

test "can create inner node no leaks" {
    const inner: rc.Rc(Node) = try innerNodeForTest(std.testing.allocator);
    cleanUpNode(inner);
}

test "numKidsOrBytes leaf is num bytes" {
    const leaf_content = "abc";
    const leaf = try Node.Leaf.fromSlice(leaf_content);
    const n = Node{ .node = .{ .leaf = leaf }, .agg = Node.AggregateStats.empty() };
    try expect(n.numKidsOrBytes() == 3);
}

test "numKidsOrBytes inner is num kids" {
    const leaf: [1]rc.Rc(Node) = .{try leafNodeForTest(std.testing.allocator)};
    const inner = try Node.Inner.fromSlice(&leaf);
    var inner_node = Node{ .node = .{ .inner = inner }, .agg = Node.AggregateStats.empty() };
    defer inner_node.deinit();

    try expect(inner_node.numKidsOrBytes() == 1);
}

test "can call fromSlice on short slice no leaks" {
    const some_bytes_shorter = "abc";
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
    defer leaf.release();
    var iter = leaf.value.*.allBytesIterator();
    _ = iter.next();
}

test "iterator returns correct values for short text" {
    const text = "abc";
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

fn debugCheck(node: *const Node, expected_height: usize) void {
    // Recurslively check that it's balanced:
    switch (node.*.node) {
        .leaf => {
            std.debug.assert(expected_height == 0);
        },
        .inner => |kids| {
            for (kids.slice()) |kid| {
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

    debugCheck(node.value, height);
}

test "print max depth" {
    // Apparently it's 21 when B is 4 and MAX_DEPTH=11 when B is 16.
    std.debug.print("MAX_DEPTH: {}\n", .{MAX_DEPTH});
}

test "can call concat" {
    const node_p = try Node.fromSlice(longer_text, std.testing.allocator);
    defer cleanUpNode(node_p);

    const node = node_p.value.*;
    const new_node_p = try node.concat(node, std.testing.allocator);
    cleanUpNode(new_node_p);
}
