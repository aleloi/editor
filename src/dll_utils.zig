//! dll_utils, DLL for file rows
const std = @import("std");
const expect = std.testing.expect;
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const DllError = error{
    /// buffer too small
    OutOfMemory,
    /// asked to read past eof
    EofError,
    /// line without terminating newline
    EolError,
    Other,
    /// failed to join properly on deletion
    DeleteError,
    /// given invalid {row, col}
    InvalidIndex,
    /// tried to unpack null
    UnpackError,
};

/// pair {row, col}.
/// row 1-indexed, col 0-indexed.
const Point = struct {
    usize,
    usize,
};

fn point(row: usize, col: usize) Point {
    return .{ row, col };
}

fn unpack(comptime T: type, maybe: ?T) !T {
    if (maybe) |val| return val;
    return DllError.UnpackError;
}

fn up(maybe: anytype) !@TypeOf(maybe orelse unreachable) {
    return maybe orelse error.Null;
}

test "unpack " {
    const logger = @import("logging.zig").default_logger;
    var opt: ?u8 = undefined;

    logger.debug("unpack {} \n", .{try up(opt)});
    opt = null;
    logger.debug("unpack {} \n", .{up(opt) catch 0});
}

const MAX_ROW_LEN: usize = 10;
const row_arr: type = [MAX_ROW_LEN]u8;
const row_arr_d: row_arr = .{0} ** MAX_ROW_LEN;

/// begin node, end node, or regular node
const NodeType = enum { fst, mid, lst };

/// element in a linked list of rows
///
/// member properties
///
/// node_type: NodeType = NodeType.mid,
///
/// prev: ?*Node = null,
///
/// next: ?*Node = null,
///
/// data: row_arr = row_arr_d,
const Node = struct {
    node_type: NodeType = NodeType.mid,
    prev: ?*Node = null,
    next: ?*Node = null,
    data: row_arr = row_arr_d,
    /// reads from data until first '\n'
    fn getData(self: *Node, buf: []u8) !usize {
        if (self.node_type != NodeType.mid) return 0;
        for (self.data, 0..) |ch, i| {
            if (i >= buf.len) return DllError.OutOfMemory;
            buf[i] = ch;
            if (ch == '\n') return i + 1;
        } else {
            return DllError.EolError;
        }
    }
    /// get part [from, to)
    fn getPart(self: *Node, buf: []u8, from: usize, to: usize) !void {
        if (self.node_type != NodeType.mid or to > self.data.len) return DllError.Other;
        if (from >= to) return;
        for (self.data[from..to], 0..) |ch, i| {
            if (i >= buf.len) return DllError.OutOfMemory;
            buf[i] = ch;
            if (ch == '\n' and from + i + 1 != to) return DllError.Other;
        }
    }
    fn getLen(self: *Node) !usize {
        if (self.node_type != NodeType.mid) return 0;
        for (self.data, 0..) |ch, i| {
            if (ch == '\n') return i + 1;
        } else {
            return DllError.EolError;
        }
    }
    fn getNode(self: *Node, index: usize) !*Node {
        var curr: *Node = self;
        var idx = index;
        while (idx > 0) {
            curr = curr.next orelse return DllError.InvalidIndex;
            idx -= 1;
        }
        return curr;
    }
    fn getNodeRev(self: *Node, index: usize) !*Node {
        var curr: *Node = self;
        var idx = index;
        while (idx > 0) {
            curr = curr.prev orelse return DllError.InvalidIndex;
            idx -= 1;
        }
        return curr;
    }
};

/// linear, avoid if possible
fn getNumLinesAfter(node: *Node) !usize {
    var iter = NodeIterator{ .curr = node };
    var ret: usize = 0;
    while (iter.next()) |_| {
        ret += 1;
    }
    return ret;
}

/// connects the list ending on fst and the list starting on sec.
///
/// sets fst.next = sec, and sec.prev = fst
fn link(fst: ?*Node, sec: ?*Node) !void {
    (try unpack(*Node, fst)).next = sec;
    (try unpack(*Node, sec)).prev = fst;
}

fn isValidIndex(root: *Node, index: Point) bool {
    const node = root.getNode(index[0]) catch return false;
    return index[1] < node.getLen() catch return false;
}

/// unpacks elems of a pair
fn unpackPair(comptime T: type, pair: struct { ?T, ?T }) !struct { T, T } {
    const fst, const lst = pair;
    return .{ try unpack(T, fst), try unpack(T, lst) };
}

/// replace the segment old (inclusive) with the segment new (inclusive).
///
/// all four should be mid-type nodes.
/// link(old_fst.prev, new_fst); link (new_lst, old_lst.next)
fn replaceSegment(old: struct { ?*Node, ?*Node }, new: struct { ?*Node, ?*Node }) !void {
    const old_from, const old_to = try unpackPair(*Node, old);
    const new_from, const new_to = try unpackPair(*Node, new);
    try link(old_from.prev, new_from);
    try link(new_to, old_to.next);
}

/// given row, col (row is relative to root)
///
/// with[0] placed at (row, col)
fn insert(allocator: Allocator, root: *Node, at: Point, with: []const u8) !void {
    const row, const col = at;

    var orig_row: row_arr = row_arr_d;
    var node = try root.getNode(row);
    const node_len = try node.getData(&orig_row);

    if (col >= node_len) return DllError.InvalidIndex;

    var list = std.ArrayList(u8).init(allocator);
    defer list.deinit();

    // while (col >= node_len) {
    //     row += 1;
    //     col %= node_len;
    //     node = try node.getNode(1);
    //     @memset(orig_row[0..node_len], 0);
    //     node_len = try node.getData(&orig_row);
    // }

    // row before cursor
    try list.appendSlice(orig_row[0..col]);
    // inserted text
    try list.appendSlice(with);
    // row from cursor
    try list.appendSlice(orig_row[col..node_len]);
    const new_fst, const new_lst = try fromStr(allocator, list.items);
    // try link(node.prev, new_fst.next);
    // try link(new_lst.prev, node.next);
    try replaceSegment(.{ node, node }, .{ new_fst.next, new_lst.prev });
}

/// the element at to becomes the element at from
///
/// expect previous elem[to] == new elem[from]
fn delete(allocator: Allocator, root: *Node, from: Point, to: Point) !void {
    const fr_row, const fr_col = from;
    const to_row, const to_col = to;

    var list = std.ArrayList(u8).init(allocator);
    defer list.deinit();

    // for debug, to compare old elem[to] and new elem[from]
    var compare_buf = [2]u8{ 0, 0 };

    // navigate to from-Point
    var orig_fr_row: row_arr = row_arr_d;
    const fr_node = try root.getNode(fr_row);
    const fr_node_len = try fr_node.getData(&orig_fr_row);
    if (fr_col >= fr_node_len) return DllError.InvalidIndex;

    // navigate to to-Point
    var orig_to_row: row_arr = row_arr_d;
    const to_node = try fr_node.getNode(to_row - fr_row);
    const to_node_len = try to_node.getData(&orig_to_row);
    if (to_col >= to_node_len) return DllError.InvalidIndex;

    try to_node.getPart(compare_buf[0..], to_col, to_col + 1);

    try list.appendSlice(orig_fr_row[0..fr_col]);
    try list.appendSlice(orig_to_row[to_col..to_node_len]);

    const new_fst, const new_lst = try fromStr(allocator, list.items);
    try replaceSegment(.{ fr_node, to_node }, .{ new_fst.next, new_lst.prev });

    try (try root.getNode(fr_row)).getPart(compare_buf[1..], fr_col, fr_col + 1);
    print("orig_to_char {}\n", .{compare_buf[0]});
    print("new_fr_char {}\n", .{compare_buf[1]});
    if (compare_buf[0] != compare_buf[1]) @panic("failed to parse on deletion!\n");
}

fn replace(allocator: Allocator, root: *Node, from: Point, to: Point, maybe_with: ?[]const u8) !void {
    try delete(allocator, root, from, to);
    try insert(allocator, root, from, maybe_with orelse return DllError.Other);
}

fn printLines(root: *Node) !void {
    print("==================\n", .{});
    print("printLines\n", .{});
    var buf: row_arr = row_arr_d;
    var n_it = NodeIterator{ .curr = root };
    while (n_it.next()) |n| {
        if (n.node_type == NodeType.mid) {
            print("{any}\n", .{buf[0..(try n.getData(&buf))]});
        }
    }
    print("==================\n", .{});
}

fn printLinesBuf(buf: []const u8) !void {
    print("==================\n", .{});
    print("printLinesBuf\n", .{});
    var l_it = LineIterator{ .data = buf };
    while (l_it.next()) |l| if (l.len > 0) print("{any}\n", .{l});
    print("==================\n", .{});
}

fn printNode(opt_node: ?*Node) !void {
    print("===============\n", .{});
    print("printNode\n", .{});
    if (opt_node) |node| {
        print("addr {any}\n", .{@intFromPtr(node)});
        print("node_type {any}\n", .{node.node_type});
        print("prev {any}\n", .{@intFromPtr(node.prev)});
        print("next {any}\n", .{@intFromPtr(node.next)});
        print("data.len {}\n", .{try node.getLen()});
        print("data {any}\n", .{node.data});
    }
    print("===============\n", .{});
}

fn printNodes(root: *Node) !void {
    print("==================\n", .{});
    print("printNodes\n", .{});
    // print("{any}\n", .{root});
    try printNode(root);
    var n_it = NodeIterator{ .curr = root };
    while (n_it.next()) |n| {
        // print("{any}\n", .{n});
        try printNode(n);
    }
    print("==================\n", .{});
}

/// NB excludes the given node, yields all following ones
const NodeIterator = struct {
    curr: ?*Node,
    reverse: bool = false,
    fn next(self: *@This()) ?*Node {
        if (self.curr) |curr| {
            if (self.reverse) {
                if (curr.node_type == NodeType.fst) {
                    self.curr = null;
                } else {
                    self.curr = curr.prev;
                }
            } else {
                if (curr.node_type == NodeType.lst) {
                    self.curr = null;
                } else {
                    self.curr = curr.next;
                }
            }
        }
        return self.curr;
    }
};

const LineIterator = struct {
    data: []const u8,
    // idx: usize = 0,
    fn next(self: *@This()) ?[]const u8 {
        // print("next data {any}\n", .{self.data});
        // var end = idx+1;
        var idx: usize = 0;
        var ret: ?[]const u8 = null;
        while (true) {
            if (idx > self.data.len) {
                return ret;
            }
            idx += 1;
            if (idx >= self.data.len or self.data[idx - 1] == '\n') {
                ret = self.data[0..idx];
                self.data = self.data[idx..];
                return ret;
            }
        }
    }
};

/// uses Node argument to reduce number of optional params
fn initNode(allocator: Allocator, node: Node, maybe_data: ?[]const u8) !*Node {
    var ret: *Node = try allocator.create(Node);
    ret.* = node;
    ret.data[0] = '\n';
    if (maybe_data) |data| {
        const len = @min(data.len, row_arr_d.len);
        @memcpy(ret.data[0..len], data[0..len]);
        if (data.len == 0 or data[data.len - 1] != '\n') ret.data[data.len] = '\n';
    }
    // print("=====================\n", .{});
    // print("allocated at {any}\n", .{@intFromPtr(ret)});
    // print("node_type {any}\n", .{ret.node_type});
    // print("prev {any}\n", .{@intFromPtr(ret.prev)});
    // print("next {any}\n", .{@intFromPtr(ret.next)});
    // print("data.len {}\n", .{try ret.getData(&ret.data)});
    // print("data {any}\n", .{ret.data});
    // print("=====================\n", .{});
    return ret;
}

/// create DLL from string, returning root node, end node
fn fromStr(allocator: Allocator, str: []const u8) !struct { *Node, *Node } {
    const root: *Node = try initNode(allocator, .{ .node_type = NodeType.fst }, null);
    var prev: *Node = root;
    try printLinesBuf(str);
    var line_it = LineIterator{ .data = str };
    //std.mem.splitSequence(u8, str, "\n");
    while (line_it.next()) |line| {
        // print("line_it {any}\n", .{line});
        // print("line_it len {any}\n", .{line.len});
        prev.next = try initNode(allocator, .{ .prev = prev }, line);
        prev = prev.next orelse unreachable;
    }
    prev.next = try initNode(allocator, .{ .node_type = NodeType.lst, .prev = prev }, null);
    print("=====================\n", .{});
    print("fromStr\n", .{});
    print("str {any}\n", .{str});
    // print("iterating...\n", .{});
    // var node_iter = NodeIterator{ .curr = root };
    // while (node_iter.next()) |curr| {
    //     print("{any}\n", .{curr});
    // }
    print("=====================\n", .{});
    return .{ root, prev.next.? };
}

/// write all lines (after root) to buffer
///
/// returns bytes written
///
/// may raise DllError
fn toStr(root: *Node, orig_buf: []u8) !usize {
    // print("toStr\n", .{});
    var buf = orig_buf[0..];
    var node_iter = NodeIterator{ .curr = root };
    while (node_iter.next()) |curr| {
        buf = buf[(try curr.getData(buf))..];
    }
    // print("toStr end\n", .{});
    return orig_buf.len - buf.len;
}

/// write n_lines lines to buffer starting from root
///
/// returns bytes written
///
/// may raise DllError.
fn getLines(root: *Node, orig_buf: []u8, maybe_fst_line: ?usize, maybe_n_lines: ?usize) !usize {
    const fst_line = maybe_fst_line orelse 1;
    const n_lines = maybe_n_lines orelse try getNumLinesAfter(root);
    // orig_buf = orig_buf[1..];
    if (n_lines == 0) return 0;
    var buf = orig_buf;
    var curr: *Node, var bytes_read: usize = try getLine(root, buf, fst_line);
    buf = buf[bytes_read..];
    for (0..(n_lines - 1)) |_| {
        curr, bytes_read = try getLine(curr, buf, 1);
        buf = buf[bytes_read..];
    }
    return orig_buf.len - buf.len;
}

/// write line with index index (relative to root) to buffer
///
/// returns pointer to node, and bytes written
///
/// may raise DllError
fn getLine(root: *Node, buf: []u8, index: usize) !struct { *Node, usize } {
    var curr = try root.getNode(index);
    return .{ curr, try curr.getData(buf) };
}

test "test fromStr" {
    // if (true) return error.SkipZigTest;
    print("===============================================\n", .{});
    print("test fromStr\n", .{});
    var buf: [1000]u8 = .{0} ** 1000;
    const str = [_]u8{ 'a', 'a', 'a' };
    // const str = [_]u8{ 'a', 'b', 'c', ' ', 'a', 's', 'd', '\n', 'a', 'a', 's', 'd', 'a', 's' };
    print("str_orig {s}\n", .{str});
    print("str_orig.len {}\n", .{str.len});

    var buffer: [1000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    for (0..str.len) |i| buf[i] = str[i];
    const root, _ = try fromStr(allocator, buf[0..str.len]);
    print("num_lines {any}\n", .{getNumLinesAfter(root)});
    var ret_buf: [100]u8 = .{0} ** 100;
    const bytes_read = try toStr(root, &ret_buf);
    print("bytes_read {any}\n", .{bytes_read});
    print("{s}\n", .{ret_buf[0..bytes_read]});

    try expect(std.mem.eql(u8, str[0..str.len], ret_buf[0..str.len]));

    try replace(
        allocator,
        root,
        point(1, 1),
        point(1, 1),
        &[3]u8{ 'b', 10, 'b' },
    );

    const bytes_read_2 = try toStr(root, &ret_buf);
    print("bytes_read {any}\n", .{bytes_read_2});
    print("\"{any}\"\n", .{ret_buf[0..bytes_read_2]});
}

test "test LineIterator" {
    // if (true) return error.SkipZigTest;
    print("===============================================\n", .{});
    print("test LineIterator\n", .{});
    var buf: [1000]u8 = undefined;
    const test_str = "bla\nbl\n\n\nbl";
    const test_len: usize = 11;
    print("test_str {any}\n", .{test_str});
    for (0..test_len) |i| buf[i] = test_str[i];
    var li = LineIterator{ .data = buf[0..test_len] };
    while (li.next()) |line| print("line {any}\n", .{line});
}

test "test insert" {
    // if (true) return error.SkipZigTest;
    print("===============================================\n", .{});
    print("test insert\n", .{});
    var buffer: [1000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    var buf: [1000]u8 = undefined;
    const test_str = "bla\nbl\n\n\nbl\n";
    const test_str_mod = "bla\na\nbl\n\n\nbl\n";
    const test_len: usize = 12;
    try expect(test_len == test_str.len);
    print("test_str {any}\n", .{test_str});
    for (0..test_len) |i| buf[i] = test_str[i];
    const root, _ = try fromStr(allocator, buf[0..test_len]);
    try printLines(root);

    try insert(allocator, root, point(1, 4), &[1]u8{'a'});
    // var li = LineIterator{ .data = buf[0..test_len] };
    // while (li.next()) |line| print("line {any}\n", .{line});
    try printLinesBuf(buf[0..test_len]);

    print("num_nodes {}\n", .{try getNumLinesAfter(root)});

    // print("toStr {any}\n", .{root});
    var ret_buf: [100]u8 = .{0} ** 100;
    const bytes_read = try toStr(root, &ret_buf);
    print("bytes_read {any}\n", .{bytes_read});
    print("{s}\n", .{ret_buf[0..bytes_read]});
    // // print("a {any}\n", .{ret_buf[0..bytes_read]});
    // // print("b {any}\n", .{test_str[0..bytes_read]});

    // var ret_buf_2: [100]u8 = .{0} ** 100;
    // const bytes_read_2 = try getLines(root, &ret_buf_2, null, null);
    // print("bytes_read_2 {any}\n", .{bytes_read_2});
    // print("{s}\n", .{ret_buf_2[0..bytes_read_2]});
    try expect(bytes_read == test_str_mod.len);
    try expect(std.mem.eql(
        u8,
        ret_buf[0..bytes_read],
        test_str_mod[0..bytes_read],
    ));

    try printLines(root);
}

test "insert after line vs before next line" {
    // print("{c}\n", .{170});
    // if (true) return error.SkipZigTest;
    print("===============================================\n", .{});
    print("test insert after line vs before next line\n", .{});
    var buffer: [1000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    var buf: [100]u8 = undefined;
    const test_str = "bla\nbl\n\n\nbl\n";
    try expect(test_str.len == 12);
    @memcpy(buf[0..test_str.len], test_str);

    const root1, _ = try fromStr(allocator, buf[0..test_str.len]);
    const root2, _ = try fromStr(allocator, buf[0..test_str.len]);
    const insert_str = "c";
    const at1 = point(1, 4);
    const at2 = point(2, 0);
    try insert(allocator, root1, at1, insert_str);
    try insert(allocator, root2, at2, insert_str);
    try printLines(root1);
    try printLines(root2);

    var ret_buf1: [100]u8 = undefined;
    var ret_buf2: [100]u8 = undefined;
    const len1 = try toStr(root1, &ret_buf1);
    try expect(len1 == try toStr(root2, &ret_buf2));
    try printLines(root1);
    try printLines(root2);

    try expect(std.mem.eql(u8, ret_buf1[0..len1], ret_buf2[0..len1]));
}

test "replace wrap" {
    // if (true) return error.SkipZigTest;
    print("===============================================\n", .{});
    print("replace wrap\n", .{});
    var buffer: [1000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    var buf: [100]u8 = undefined;
    const test_str = "bla\nbl\n\n\nbl\n";
    try expect(test_str.len == 12);
    @memcpy(buf[0..test_str.len], test_str);

    const root1, _ = try fromStr(allocator, buf[0..test_str.len]);
    const root2, _ = try fromStr(allocator, buf[0..test_str.len]);
    const insert_str = "c";
    const at1 = point(1, 4);
    const at2 = point(2, 0);
    try replace(allocator, root1, at1, at2, insert_str);
    try replace(allocator, root2, at2, at2, insert_str);
    try printLines(root1);
    try printLines(root2);

    var ret_buf1: [100]u8 = undefined;
    var ret_buf2: [100]u8 = undefined;
    const len1 = try toStr(root1, &ret_buf1);
    try expect(len1 == try toStr(root2, &ret_buf2));
    try printLines(root1);
    try printLines(root2);

    try expect(std.mem.eql(u8, ret_buf1[0..len1], ret_buf2[0..len1]));
}

test "replace general" {
    // if (true) return error.SkipZigTest;
    print("===============================================\n", .{});
    print("replace general\n", .{});
    var buffer: [1000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    var buf: [100]u8 = undefined;
    const test_str = "bla\nbl\n\n\nbl\n";
    try expect(test_str.len == 12);
    @memcpy(buf[0..test_str.len], test_str);

    const root1, _ = try fromStr(allocator, buf[0..test_str.len]);
    print("orig\n", .{});
    try printLines(root1);

    // const root2, _ = try fromStr(allocator, buf[0..test_str.len]);
    // const insert_str = "c";
    const at1 = point(1, 4);
    const at2 = point(5, 1);
    try delete(allocator, root1, at1, at2);
    // try replace(allocator, root2, at2, at2, insert_str);
    try printLines(root1);
    // try printLines(root2);

    // var ret_buf1: [100]u8 = undefined;
    // var ret_buf2: [100]u8 = undefined;
    // const len1 = try toStr(root1, &ret_buf1);
    // try expect(len1 == try toStr(root2, &ret_buf2));
    // try printLines(root1);
    // try printLines(root2);

    // try expect(std.mem.eql(u8, ret_buf1[0..len1], ret_buf2[0..len1]));
}

test "delete all" {
    // if (true) return error.SkipZigTest;
    print("===============================================\n", .{});
    print("delete all\n", .{});
    var buffer: [1000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    var buf: [100]u8 = undefined;
    const test_str = "bla\nbl\n\n\nbl\n";
    try expect(test_str.len == 12);
    @memcpy(buf[0..test_str.len], test_str);

    const root1, _ = try fromStr(allocator, buf[0..test_str.len]);
    print("orig\n", .{});
    try printLines(root1);

    // const root2, _ = try fromStr(allocator, buf[0..test_str.len]);
    // const insert_str = "c";
    const at1 = point(1, 4);
    const at2 = point(7, 0);
    try delete(allocator, root1, at1, at2);
    // try replace(allocator, root2, at2, at2, insert_str);
    try printLines(root1);
    // try printLines(root2);

    // var ret_buf1: [100]u8 = undefined;
    // var ret_buf2: [100]u8 = undefined;
    // const len1 = try toStr(root1, &ret_buf1);
    // try expect(len1 == try toStr(root2, &ret_buf2));
    // try printLines(root1);
    // try printLines(root2);

    // try expect(std.mem.eql(u8, ret_buf1[0..len1], ret_buf2[0..len1]));
}
