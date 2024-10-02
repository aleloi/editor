const std = @import("std");
const rope = @import("rope");
const Rc = @import("zigrc").Rc;

const NodeBF = rope.NodeBF;
const TestNode = NodeBF(16, std.testing.allocator);
const RcTestNode = TestNode.RcSelf;
const Pos = rope.Pos;
const AggregateStats = rope.AggregateStats;


const expect = std.testing.expect;

const longer_text =
    \\test string with some newlines
    \\\n definitely over 16 chars actually
    \\longer than 64 chars
    \\and some more text
    \\below to pad it out
    \\yes really
    \\But wait! The rope memory overhead was 30x the contained memory
    \\and I modified the leaf size, the refcounts and some other things.
    \\Now leafs are much larger so I have to increase the size
    \\of this text string, otherwise tests wouldn't cover the chain
    \\constrcution, the bubbling up and the merging. Also, the alloc
    \\is now part of the type, which makes it messier to construct the
    \\custom ropes. But we save 32 bytes per refcount!
    \\
;

// Was supposed to run the tests in rope.zig, but doesn't do anything:
// test {
//     std.testing.refAllDeclsRecursive(@This());
// }


var failing_allocator = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = 0 });
const failing_alloc = failing_allocator.allocator();


fn cleanUpNode(node: RcTestNode) void {
    node.releaseWithFn(TestNode.deinit);
}


test "can call fromSlice on short slice no leaks" {
    const some_bytes_shorter = "ab";
    const node = try TestNode.fromSlice(some_bytes_shorter);
    cleanUpNode(node);
}

test "can call fromSlice on longer slice no leaks" {
    const some_bytes_longer = "test string with \n some newlines \n definitely over \n 16 chars";
    const node = try TestNode.fromSlice(some_bytes_longer);
    cleanUpNode(node);
}

test "iterator returns correct values for short text" {
    const text = "ab";
    const node = try TestNode.fromSlice(text);
    defer cleanUpNode(node);

    var iter = node.value.*.allBytesIterator();
    for (text) |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);
}

test "iterator returns correct values for longer text" {
    const node = try TestNode.fromSlice(longer_text);
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

test "pos to index on empty tree" {
    const node_p = try TestNode.fromSlice("");
    defer cleanUpNode(node_p);

    const node: TestNode = node_p.value.*;
    const result = node.posToOffset(.{.row=0, .col=0});
    //std.debug.print("r0, c0 on empty: {any}\n\n", .{result});
    try expect(result catch 1000  == 0);
}


test "line length on empty tree" {
    const node_p = try TestNode.fromSlice("");
    defer cleanUpNode(node_p);

    const node: TestNode = node_p.value.*;
    const result = try node.rowLength(0);
    try expect(result == 0);
}

test "line 0 of newline tree" {
    const node_p = try TestNode.fromSlice("\n");
    defer cleanUpNode(node_p);

    const node: TestNode = node_p.value.*;
    const result = try node.rowLength(0);
    try expect(result == 0);
}

test "line 1 of newline tree" {
    const node_p = try TestNode.fromSlice("\n");
    defer cleanUpNode(node_p);

    const node: TestNode = node_p.value.*;
    const result = try node.rowLength(1);
    try expect(result == 0);
}

test "line 0 of nonempty tree" {
    const node_p = try TestNode.fromSlice(" ");
    defer cleanUpNode(node_p);

    const node: TestNode = node_p.value.*;
    const result = try node.rowLength(0);
    try expect(result == 1);
}

test "line 1 of nonempty newline tree" {
    const node_p = try TestNode.fromSlice(" \n");
    defer cleanUpNode(node_p);

    const node: TestNode = node_p.value.*;
    const result = try node.rowLength(1);
    try expect(result == 0);
}

test "rowLength gives posError on invalid line" {
    const node_p = try TestNode.fromSlice("");
    defer cleanUpNode(node_p);

    const node: TestNode = node_p.value.*;
    const result = node.rowLength(1);
    try expect(result == TestNode.PosError.InvalidPos);
}


// test "pos to index on newline tree" {
//     const node_p = try TestNode.fromSlice("\n");
//     defer cleanUpNode(node_p);

//     const node: TestNode = node_p.value.*;
//     try expect(node.posToOffset(.{.row=0, .col=0}) == TestNode.PosError.InvalidPos);
// }

test "pos to index in matches PosIterator" {
    const text = "hej\nhopp";
    const node_p = try TestNode.fromSlice(text);
    defer cleanUpNode(node_p);

    const node: TestNode = node_p.value.*;
    var it = PosIterator{ .slice = text };

    for (text, 0..) |_, i| {
        const p = it.next() orelse unreachable;
        const offset = node.posToOffset(p) catch unreachable;
        try expect(offset == i);
    }
    try expect(it.next() == null);

    // Check that the last value points to one after the end of the data:
    const after_last: Pos = .{.row = it.row, .col=it.col};
    const offset = node.posToOffset(after_last) catch unreachable;
    try expect(offset == node_p.value.*.agg.num_bytes);

}

test "posToIndex returns error on invalid col in line" {
    const node_p = try TestNode.fromSlice(longer_text);
    defer cleanUpNode(node_p);

    const node: TestNode = node_p.value.*;
    try expect(node.posToOffset(.{ .row = 0, .col = 100000 }) == TestNode.PosError.InvalidPos);
}

test "posToIndex returns error on invalid line in text" {
    const node_p = try TestNode.fromSlice(longer_text);
    defer cleanUpNode(node_p);

    const node: TestNode = node_p.value.*;
    try expect(node.posToOffset(.{ .row = 10000000, .col = 0 }) == TestNode.PosError.InvalidPos);
}

test "fromSlice tree is balanced and has correct agg" {
    const node = try TestNode.fromSlice(longer_text);
    defer cleanUpNode(node);

    const height = node.value.*.height();
    node.value.*.debugCheck(height);
}

test "can call concat non-empty with empty" {
    const node_p = try TestNode.fromSlice(longer_text);
    defer cleanUpNode(node_p);

    const empty_node_p = try TestNode.fromSlice("");
    defer cleanUpNode(empty_node_p);

    const concat_node_p = try TestNode.concat(node_p, empty_node_p);
    defer cleanUpNode(concat_node_p);

    var iter = concat_node_p.value.*.allBytesIterator();
    for (longer_text) |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);

}

test "can call concat empty with non-empty" {
    const node_p = try TestNode.fromSlice(longer_text);
    defer cleanUpNode(node_p);

    const empty_node_p = try TestNode.fromSlice("");
    defer cleanUpNode(empty_node_p);

    const concat_node_p = try TestNode.concat(empty_node_p, node_p);
    defer cleanUpNode(concat_node_p);

    var iter = concat_node_p.value.*.allBytesIterator();
    for (longer_text) |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);
}


test "can concat short non-empty with short non-empty" {
    const ab = try TestNode.fromSlice("ab");
    defer cleanUpNode(ab);

    const cd = try TestNode.fromSlice("cd");
    defer cleanUpNode(cd);

    const abcd = try TestNode.concat(ab, cd);
    defer cleanUpNode(abcd);

    var iter = abcd.value.*.allBytesIterator();
    for ("abcd") |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);
}

test "can concat longer non-empty with short non-empty" {
    const abcd = try TestNode.fromSlice("abcd");
    defer cleanUpNode(abcd);

    const cd = try TestNode.fromSlice("cd");
    defer cleanUpNode(cd);

    const abcdcd = try TestNode.concat(abcd, cd);
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
    const long_node = try TestNode.fromSlice(longer_text);
    defer cleanUpNode(long_node);

    const cd = try TestNode.fromSlice("cd");
    defer cleanUpNode(cd);

    const long_node_cd = try TestNode.concat(long_node, cd);
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
    const ab = try TestNode.fromSlice("ab");
    defer cleanUpNode(ab);

    const ab_ab = try TestNode.concat(ab, ab);
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
    const longer = try TestNode.fromSlice(longer_text);
    defer cleanUpNode(longer);

    const longer_longer = try TestNode.concat(longer, longer);
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
    const longer = try TestNode.fromSlice(longer_text);
    defer cleanUpNode(longer);

    const longer2 = try TestNode.fromSlice(longer_text);
    defer cleanUpNode(longer2);

    const longer_longer = try TestNode.concat(longer, longer2);
    defer cleanUpNode(longer_longer);

    var iter = longer_longer.value.*.allBytesIterator();
    for (longer_text ++ longer_text) |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);

    TestNode.debugCheck(longer_longer.value, longer_longer.value.*.height());
}


test "can concat long with lots of stuff" {
    const longer = try TestNode.fromSlice(longer_text);
    defer cleanUpNode(longer);

    inline for (0..longer_text.len) |l| {
        const other = try TestNode.fromSlice(longer_text[0..l]);
        defer cleanUpNode(other);

        const longer_other = try TestNode.concat(longer, other);
        defer cleanUpNode(longer_other);

        var iter = longer_other.value.*.allBytesIterator();
        for (longer_text ++ longer_text[0..l]) |expected| {
            const actual = iter.next();
            try expect(actual != null);
            try expect(expected == actual orelse unreachable);
        }
        try expect(iter.next() == null);


        TestNode.debugCheck(longer_other.value, longer_other.value.*.height());
    }

}


test "no leaks when short concat with failing allocator" {
    const FailingNode = NodeBF(2, failing_alloc);
    failing_allocator = std.testing.FailingAllocator.init(
        std.testing.allocator,
        .{.fail_index = 2}
    );

    const ab = FailingNode.fromSlice("ab") catch return;
    defer ab.releaseWithFn(FailingNode.deinit);

    const ab2 = FailingNode.fromSlice("ab") catch return;
    defer ab2.releaseWithFn(FailingNode.deinit);

    const ab_ab2 = FailingNode.concat(ab, ab2) catch return;
    defer ab_ab2.releaseWithFn(FailingNode.deinit);
}

test "No leaks in concat with different branch factors" {

    inline for (2..10) |bf| {
        const CustomNode = NodeBF(bf, std.testing.allocator);
        const longer = try CustomNode.fromSlice(longer_text);
        defer longer.releaseWithFn(CustomNode.deinit);

        const longer2 = CustomNode.fromSlice(longer_text) catch return;
        defer longer2.releaseWithFn(CustomNode.deinit);

        const longer_longer = CustomNode.concat(longer, longer2) catch return;
        defer longer_longer.releaseWithFn(CustomNode.deinit);
        }

}

test "concat is correct with different branch factors" {
    @setEvalBranchQuota(20000);
    inline for (2..5) |bf| {
        const CustomNode = NodeBF(bf, std.testing.allocator);
        const longer = try CustomNode.fromSlice(longer_text);
        defer longer.releaseWithFn(CustomNode.deinit);

        const longer_longer = longer_text ++ longer_text ++ longer_text ++ longer_text;

        var buf: [longer_longer.len * 2] u8 = undefined;

        for (0..longer_longer.len) |l| {
            const other = try CustomNode.fromSlice(longer_longer[0..l]);
            defer other.releaseWithFn(CustomNode.deinit);


            const longer_other = CustomNode.concat(longer, other) catch return;
            defer longer_other.releaseWithFn(CustomNode.deinit);

            var iter = longer_other.value.*.allBytesIterator();

            // Looks nicer with comptime, but takes forever to execute:
            std.mem.copyForwards(u8, &buf, longer_text);
            std.mem.copyForwards(u8, (&buf)[longer_text.len..], longer_longer[0..l]);
            const expected_slice = buf[0..(longer_text.len + l)];

            for (expected_slice) |expected| {
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
    failing_allocator = std.testing.FailingAllocator.init(
            std.testing.allocator,
            .{.fail_index = 1000}
        );
    const CustomNode = NodeBF(2, failing_alloc);
    const longer = try CustomNode.fromSlice(longer_text);
    defer longer.releaseWithFn(CustomNode.deinit);

    const shorter = try CustomNode.fromSlice("ab");
    defer shorter.releaseWithFn(CustomNode.deinit);

    for (2..10) |l| {
        failing_allocator = std.testing.FailingAllocator.init(
            std.testing.allocator,
            .{.fail_index = l}
        );

        const longer_other = CustomNode.concat(longer, shorter) catch {
            try expect(longer.strongCount() == 1);
            continue;
            // COULD do this, but would have to reset the failing alloc first.
            //break :b try CustomNode.fromSlice("");
        };

        defer longer_other.releaseWithFn(CustomNode.deinit);
    }
}


test "Can split ab" {
    const node = try TestNode.fromSlice("ab");
    defer node.releaseWithFn(TestNode.deinit);
    const splt = try TestNode.splitAt(node, 1);
    defer splt.fst.releaseWithFn(TestNode.deinit);
    defer splt.snd.releaseWithFn(TestNode.deinit);

    try expect(splt.fst.value.*.agg.num_bytes == 1);
    try expect(splt.snd.value.*.agg.num_bytes == 1);
}




test "splits ab into a and b" {
    const node = try TestNode.fromSlice("ab");
    defer node.releaseWithFn(TestNode.deinit);
    const splt = try TestNode.splitAt(node, 1);
    defer splt.fst.releaseWithFn(TestNode.deinit);
    defer splt.snd.releaseWithFn(TestNode.deinit);

    var a_iter = splt.fst.value.*.allBytesIterator();
    var b_iter = splt.snd.value.*.allBytesIterator();

    try expect(a_iter.next() == 'a');
    try expect(a_iter.next() == null);

    try expect(b_iter.next() == 'b');
    try expect(b_iter.next() == null);
}


test "concat short with long" {
    const ab = try TestNode.fromSlice("ab");
    defer ab.releaseWithFn(TestNode.deinit);

    const longer = try TestNode.fromSlice(longer_text);
    defer longer.releaseWithFn(TestNode.deinit);

        const ab_concat = try TestNode.concat(ab, longer);
        defer ab_concat.releaseWithFn(TestNode.deinit);


    var iter = ab_concat.value.*.allBytesIterator();
    for ("ab" ++ longer_text) |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);
}



test "Can split longer text" {
    const node = try TestNode.fromSlice(longer_text);
    defer node.releaseWithFn(TestNode.deinit);

    const splt = try TestNode.splitAt(node, 50);
    defer splt.fst.releaseWithFn(TestNode.deinit);
    defer splt.snd.releaseWithFn(TestNode.deinit);
}


test "Can split longer text in multiple places" {
    const node = try TestNode.fromSlice(longer_text);
    defer node.releaseWithFn(TestNode.deinit);

    for (0..(longer_text.len+1)) |l| {
        const splt = try TestNode.splitAt(node, l);
        defer splt.fst.releaseWithFn(TestNode.deinit);
        defer splt.snd.releaseWithFn(TestNode.deinit);

        var a_iter = splt.fst.value.*.allBytesIterator();
        var b_iter = splt.snd.value.*.allBytesIterator();

        for (0..l) |i| {
            try expect(a_iter.next() == longer_text[i]);
        }
        try expect(a_iter.next() == null);

        for (l..(longer_text.len)) |i| {
            try expect(b_iter.next() == longer_text[i]);
        }
        try expect(b_iter.next() == null);
    }
}


test "Can split with failing allocator without leaking" {
    inline for (2..16) |rc| {
        failing_allocator = std.testing.FailingAllocator.init(
                    std.testing.allocator,
                    .{.fail_index = 100}
                );
        const CustomNode = NodeBF(rc, failing_alloc);
        for (0..(longer_text.len+1)) |tl| {
            for (0..20) |l| {
                failing_allocator = std.testing.FailingAllocator.init(
                    std.testing.allocator,
                    .{.fail_index = l}
                );

                const node = CustomNode.fromSlice(longer_text) catch continue;
                {
                    defer node.releaseWithFn(CustomNode.deinit);
                    const splt = CustomNode.splitAt(node, tl) catch continue;

                    splt.fst.releaseWithFn(CustomNode.deinit);
                    splt.snd.releaseWithFn(CustomNode.deinit);
                }
            }
        }
    }
}

