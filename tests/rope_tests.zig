const std = @import("std");
const rope = @import("rope");
const Rc = @import("zigrc").Rc;

const NodeBF = rope.NodeBF;
const Node = rope.Node;
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
;

// Was supposed to run the tests in rope.zig, but doesn't do anything:
// test {
//     std.testing.refAllDeclsRecursive(@This());
// }




fn cleanUpNode(node: Rc(Node)) void {
    node.releaseWithFn(Node.deinit);
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
        const offset = node.posToOffset(p) catch unreachable;
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
    const ab = try Node.fromSlice("ab", std.testing.allocator);
    defer cleanUpNode(ab);

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
    const longer = try Node.fromSlice(longer_text, std.testing.allocator);
    defer cleanUpNode(longer);

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
    const longer = try Node.fromSlice(longer_text, std.testing.allocator);
    defer cleanUpNode(longer);

    const longer2 = try Node.fromSlice(longer_text, std.testing.allocator);
    defer cleanUpNode(longer2);

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
        const longer = try CustomNode.fromSlice(longer_text, std.testing.allocator);
        defer longer.releaseWithFn(CustomNode.deinit);

        const longer2 = CustomNode.fromSlice(longer_text, std.testing.allocator) catch return;
        defer longer2.releaseWithFn(CustomNode.deinit);

        const longer_longer = CustomNode.concat(longer, longer2, std.testing.allocator) catch return;
        defer longer_longer.releaseWithFn(CustomNode.deinit);
        }

}

test "concat is correct with different branch factors" {
    @setEvalBranchQuota(10000);
    inline for (2..10) |bf| {
        const CustomNode = NodeBF(bf);
        const longer = try CustomNode.fromSlice(longer_text, std.testing.allocator);
        defer longer.releaseWithFn(CustomNode.deinit);

        const longer_longer = longer_text ++ longer_text ++ longer_text ++ longer_text;

        inline for (0..longer_longer.len) |l| {
            const other = try CustomNode.fromSlice(longer_longer[0..l], std.testing.allocator);
            defer other.releaseWithFn(CustomNode.deinit);


            const longer_other = CustomNode.concat(longer, other, std.testing.allocator) catch return;
            defer longer_other.releaseWithFn(CustomNode.deinit);

            var iter = longer_other.value.*.allBytesIterator();
            for (longer_text ++ longer_longer[0..l]) |expected| {
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
            try expect(longer.strongCount() == 1);
            break :b try CustomNode.fromSlice("", std.testing.allocator);
        };

        defer longer_other.releaseWithFn(CustomNode.deinit);
    }
}


test "Can split ab" {
    const node = try Node.fromSlice("ab", std.testing.allocator);
    defer node.releaseWithFn(Node.deinit);
    const splt = try Node.splitAt(node, 1, std.testing.allocator);
    defer splt.fst.releaseWithFn(Node.deinit);
    defer splt.snd.releaseWithFn(Node.deinit);

    try expect(splt.fst.value.*.agg.num_bytes == 1);
    try expect(splt.snd.value.*.agg.num_bytes == 1);
}




test "splits ab into a and b" {
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


test "concat short with long" {
    const ab = try Node.fromSlice("ab", std.testing.allocator);
    defer ab.releaseWithFn(Node.deinit);

    const longer = try Node.fromSlice(longer_text, std.testing.allocator);
    defer longer.releaseWithFn(Node.deinit);

        const ab_concat = try Node.concat(ab, longer, std.testing.allocator);
        defer ab_concat.releaseWithFn(Node.deinit);


    var iter = ab_concat.value.*.allBytesIterator();
    for ("ab" ++ longer_text) |expected| {
        const actual = iter.next();
        try expect(actual != null);
        try expect(expected == actual orelse unreachable);
    }
    try expect(iter.next() == null);
}



test "Can split longer text" {
    const node = try Node.fromSlice(longer_text, std.testing.allocator);
    defer node.releaseWithFn(Node.deinit);

    const splt = try Node.splitAt(node, 50, std.testing.allocator);
    defer splt.fst.releaseWithFn(Node.deinit);
    defer splt.snd.releaseWithFn(Node.deinit);
}


test "Can split longer text in multiple places" {
    const node = try Node.fromSlice(longer_text, std.testing.allocator);
    defer node.releaseWithFn(Node.deinit);

    for (0..(longer_text.len+1)) |l| {
        const splt = try Node.splitAt(node, l, std.testing.allocator);
        defer splt.fst.releaseWithFn(Node.deinit);
        defer splt.snd.releaseWithFn(Node.deinit);

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
        const CustomNode = NodeBF(rc);
        for (0..(longer_text.len+1)) |tl| {
            for (0..20) |l| {
                var failing_allocator = std.testing.FailingAllocator.init(
                    std.testing.allocator,
                    .{.fail_index = l}
                );
                const alloc = failing_allocator.allocator();

                const node = CustomNode.fromSlice(longer_text, alloc) catch continue;
                {
                    defer node.releaseWithFn(CustomNode.deinit);
                    const splt = CustomNode.splitAt(node, tl, alloc) catch continue;

                    splt.fst.releaseWithFn(CustomNode.deinit);
                    splt.snd.releaseWithFn(CustomNode.deinit);
                }
            }
        }
    }
}

