const std = @import("std");
const rc = @import("zigrc");

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

const BRANCH_FACTOR = 4;

/// Max depth is enough to store 1TB data. Used for Iterator
/// stack-based recursion. NOTE that the tree need not use the full
/// available memory - some leaf nodes can store less that
/// BRANCH_FACTOR bytes, so the tree may be larger.
const MAX_DEPTH: comptime_int = blk: {
    // 1TB = 10**12 bytes.
    const maxFileSizeBytes: comptime_int = std.math.powi(usize, 10, 12) catch unreachable;
    break :blk std.math.log_int(comptime_int, BRANCH_FACTOR, maxFileSizeBytes) + 2;
};

pub const Node = struct {
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

                    // This will work unless the stack is MAX_DEPTH, whith shouldn't happen.
                    // Let's crash if it does.
                    self.rec_stack.append(.{ .node = kid_ptr, .kid_index = 0 }) catch {
                        std.debug.print("Can't append! {}\n", .{self.rec_stack});
                        std.debug.dumpCurrentStackTrace(null);
                        std.process.exit(1);
                        unreachable;
                    };
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

        var agg: AggregateStats = AggregateStats.empty();
        for (inner.slice()) |kid| {
            agg = agg.combine(kid.value.*.agg);
        }
        const n = Node{ .node = .{ .inner = inner }, .agg = agg };
        return try rc.Rc(Node).init(alloc, n);
    }

    fn allBytesIterator(self: *const @This()) Iterator {
        // const self: [1] *const Node = .{};
        var res: Iterator = .{ .rec_stack = .{} };
        res.rec_stack.append(.{ .node = self, .kid_index = 0 }) catch unreachable;
        return res;
    }

    // /// Checks that all leafs are at the right level, crashes on failure.
    // fn debugCheck(self: *const @This(), expected_height: ?usize) void {

    // }

};

const expect = std.testing.expect;

test "B at least 2" {
    // Can't store data in a tree with branching factor < 2.
    try comptime expect(BRANCH_FACTOR >= 2);
}

test "can create leaf node" {
    const leaf = Leaf{};
    _ = Node{ .node = .{ .leaf = leaf }, .agg = AggregateStats.empty() };
}

/// Creates refcounted leaf node; should have a single strong pointer.
fn leafNodeForTest(alloc: std.mem.Allocator) !rc.Rc(Node) {
    const leaf_content = "abc";
    const leaf = try Leaf.fromSlice(leaf_content);
    const n = Node{ .node = .{ .leaf = leaf }, .agg = AggregateStats.empty() };
    return try rc.Rc(Node).init(alloc, n);
}

/// Creates a refcounted inner node with a single leaf node; both have
/// 1 strong pointer each.
fn innerNodeForTest(alloc: std.mem.Allocator) !rc.Rc(Node) {
    const leaf = try leafNodeForTest(alloc);
    var inner = try Inner.init(1);
    inner.set(0, leaf);
    const inner_node = Node{ .node = .{ .inner = inner }, .agg = AggregateStats.empty() };
    return try rc.Rc(Node).init(alloc, inner_node);
}

test "can create leaf node with refcount no leaks" {
    const node = try leafNodeForTest(std.testing.allocator);
    defer node.release();
}

test "leaf node height is 0" {
    const node = try leafNodeForTest(std.testing.allocator);
    defer node.release();
    try expect(node.value.*.height() == 0);
}

test "parent node height is 1" {
    const leaf: [1]rc.Rc(Node) = .{try leafNodeForTest(std.testing.allocator)};
    const inner = try Inner.fromSlice(&leaf);
    var parent_node = Node{ .node = .{ .inner = inner }, .agg = AggregateStats.empty() };
    defer parent_node.deinit();

    try expect(parent_node.height() == 1);
}

test "can create inner node no leaks" {
    const inner: rc.Rc(Node) = try innerNodeForTest(std.testing.allocator);
    defer inner.release();
    defer inner.value.*.deinit();
}

test "numKidsOrBytes leaf is num bytes" {
    const leaf_content = "abc";
    const leaf = try Leaf.fromSlice(leaf_content);
    const n = Node{ .node = .{ .leaf = leaf }, .agg = AggregateStats.empty() };
    try expect(n.numKidsOrBytes() == 3);
}

test "numKidsOrBytes inner is num kids" {
    const leaf: [1]rc.Rc(Node) = .{try leafNodeForTest(std.testing.allocator)};
    const inner = try Inner.fromSlice(&leaf);
    var inner_node = Node{ .node = .{ .inner = inner }, .agg = AggregateStats.empty() };
    defer inner_node.deinit();

    try expect(inner_node.numKidsOrBytes() == 1);
}

test "can call fromSlice on short slice no leaks" {
    const some_bytes_shorter = "abc";
    var node = try Node.fromSlice(some_bytes_shorter, std.testing.allocator);
    defer node.release();
    defer node.value.*.deinit();
}

test "can call fromSlice on longer slice no leaks" {
    const some_bytes_longer = "test string with \n some newlines \n definitely over \n 16 chars";
    var node = try Node.fromSlice(some_bytes_longer, std.testing.allocator);
    defer node.release();
    defer node.value.*.deinit();
}

test "can call iterator and next" {
    const leaf = try leafNodeForTest(std.testing.allocator);
    defer leaf.release();
    var iter = leaf.value.*.allBytesIterator();
    _ = iter.next();
}

test "iterator returns correct values for short text" {
    const text = "abc";
    var node = try Node.fromSlice(text, std.testing.allocator);
    defer node.release();
    defer node.value.*.deinit();

    var iter = node.value.*.allBytesIterator();
    for (text) |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);
}

test "iterator returns correct values for longer text" {
    const text =
        \\test string with some newlines
        \\\n definitely over 16 chars actually
        \\longer than 64 chars
        \\and some more text
        \\below to pad it out
        \\yes really
    ;
    var node = try Node.fromSlice(text, std.testing.allocator);
    defer node.release();
    defer node.value.*.deinit();

    var iter = node.value.*.allBytesIterator();
    for (text) |expected| {
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

    std.debug.print("\n'", .{});
    while (iter.next()) |b| {
        std.debug.print("{c}", .{b});
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

test "fromSlice tree is balanced and has correct agg" {
    const text =
        \\test string with some newlines
        \\\n definitely over 16 chars actually
        \\longer than 64 chars
        \\and some more text
        \\below to pad it out
        \\yes really
    ;
    var node = try Node.fromSlice(text, std.testing.allocator);
    defer node.release();
    defer node.value.*.deinit();

    const height = node.value.*.height();

    debugCheck(node.value, height);
}
