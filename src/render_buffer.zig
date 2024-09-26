const std = @import("std");
const rope = @import("rope");
//const Rc = rope.Rc;
const Pos = rope.Pos;
//const Rope = Rc(rope.Node);

/// Specifies what portion of a document to display.
pub const ViewPort = struct {
    start: Pos = .{ .row = 0, .col = 0 },
    height: usize,
    width: usize,
};

/// TODO this is a copy of an identical PosIterator in the rope
/// tests. Move it to a new utils.zig or something. Also make some
/// tests for it, and add some docs.
pub const PosIterator = struct {
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

/// Buffers required for rendering the document and functions for
/// initializing and filling these buffers. Alse fns for extracting
/// info from them.
pub const RenderBuffer = struct {
    /// A line is just a dynamic byte array. Could have a max line
    /// size maybe? Or not, screens can get many 100s chars wide.
    const Line = std.ArrayList(u8);

    /// The output format; each thing represents an existing line to
    /// display. Lines are capped by the Viewport parameter, and you
    /// get an indication of whether the actual line is longer.
    pub const LineSlice = struct {
        line: []const u8,
        line_continues_past_buf: bool,
        line_ends_before_buf: bool,
    };

    /// Internal buffers; don't use directly.
    lines: std.ArrayList(Line),
    slices: std.ArrayList(LineSlice),

    /// Governs what this struct fills it's buffer
    /// with. viewport.start.row is ignored: it's assumed that the
    /// slice you feed the RenderBuffer with already starts at
    /// viewport.start.row.
    viewport: ViewPort,

    fn init(alloc: std.mem.Allocator, viewport: ViewPort) !@This() {
        var res: @This() = .{ .lines = undefined, .slices = undefined, .viewport = viewport };
        res.lines = try @TypeOf(res.lines).initCapacity(alloc, viewport.height);
        errdefer {
            for (res.lines.items) |line| {
                line.deinit();
            }
            res.lines.deinit();
        }

        res.slices = try @TypeOf(res.slices).initCapacity(alloc, viewport.height);
        errdefer res.slices.deinit();

        for (0..viewport.height) |_| {
            res.lines.appendAssumeCapacity(try Line.initCapacity(alloc, viewport.width));
        }
        return res;
    }

    fn resize(self: *@This(), alloc: std.mem.Allocator, viewport: ViewPort) !void {
        // do this last when we know everything succeeds.
        defer self.viewport = viewport;

        while (self.lines.items.len > viewport.height) {
            const l = self.lines.pop();
            l.deinit();
        }
        //if (self.lines.capa)

        try self.lines.ensureTotalCapacity(viewport.height);
        for (self.lines.items) |*itm| {
            try itm.*.ensureTotalCapacity(viewport.width);
        }
        while (self.lines.items.len < viewport.height) {
            self.lines.appendAssumeCapacity(try Line.initCapacity(alloc, viewport.width));
        }
    }

    fn deinit(self: *@This()) void {
        self.slices.deinit();
        for (self.lines.items) |line| {
            line.deinit();
        }
        self.lines.deinit();
    }

    /// Test impl, not thinking about efficiency. Assumes the text is
    /// ALL the text from the first line to the last line in the view
    /// port. Extracts the part that will be visible.
    pub fn fillFromText(self: *@This(), text: []const u8) []const LineSlice {
        std.debug.assert(self.lines.items.len == self.viewport.height);

        self.slices.clearRetainingCapacity();
        for (self.lines.items) |*line| {
            line.*.clearRetainingCapacity();
            std.debug.assert(line.*.capacity >= self.viewport.width);
        }

        //var row = 0;
        const offset: isize = @intCast(self.viewport.start.col);
        // var curr_line: [] u8 = self.lines.items[row].items;
        var iter = PosIterator{ .slice = text };

        for (text) |c| {
            const pos: Pos = iter.next().?;
            std.debug.assert(pos.row <= self.viewport.height);
            const curr_line: *Line = &self.lines.items[pos.row];
            const relative_pos: isize = @as(isize, @intCast(pos.col)) - offset;

            // Save char to internal buffer if it's inside our viewport.
            if (0 <= relative_pos and relative_pos < self.viewport.width and c != '\n') {
                //std.debug.print("curr line: {s}, relative pos: {}, char: {c} \n", .{curr_line.*.items, relative_pos, c});
                std.debug.assert(curr_line.*.items.len == relative_pos);
                curr_line.*.appendAssumeCapacity(c);
            }

            // WRONG, because then we skip too short lines! They should be empty instead.
            const has_not_reached_window: bool = relative_pos < 0;
            // if (has_not_reached_window) continue;

            // Update or set what we know about the current row.
            const is_past_window: bool = relative_pos >= self.viewport.width;
            const curr_line_slice = LineSlice{ .line = curr_line.*.items, .line_continues_past_buf = is_past_window, .line_ends_before_buf = has_not_reached_window };
            std.debug.assert(self.slices.items.len == pos.row or self.slices.items.len == pos.row + 1);
            if (self.slices.items.len == pos.row) {
                self.slices.appendAssumeCapacity(curr_line_slice);
            } else {
                self.slices.items[pos.row] = curr_line_slice;
            }
        }

        return self.slices.items;
    }

    // /// If returned len is < height, it's because the document has
    // /// ended, and there are no more lines.
    // pub fn getLines(self: @This()) []const LineSlice {
    //     return self.slices.items;
    // }
};

const expect = std.testing.expect;
const test_alloc = std.testing.allocator;
test "can create render buffer no leaks" {
    const vp = ViewPort{ .start = .{ .row = 0, .col = 0 }, .height = 3, .width = 4 };
    var rb = try RenderBuffer.init(test_alloc, vp);
    rb.deinit();
}

test "can resize to larger no leaks" {
    const vp = ViewPort{ .start = .{ .row = 0, .col = 0 }, .height = 3, .width = 4 };
    const vp2 = ViewPort{ .start = .{ .row = 0, .col = 0 }, .height = 4, .width = 5 };
    var rb = try RenderBuffer.init(test_alloc, vp);
    try rb.resize(test_alloc, vp2);
    rb.deinit();
}

test "can resize to smaller no leaks" {
    const vp = ViewPort{ .start = .{ .row = 0, .col = 0 }, .height = 3, .width = 4 };
    const vp2 = ViewPort{ .start = .{ .row = 0, .col = 0 }, .height = 1, .width = 1 };
    var rb = try RenderBuffer.init(test_alloc, vp);
    try rb.resize(test_alloc, vp2);
    rb.deinit();
}

test "can write in buffer after init" {
    const vp = ViewPort{ .start = .{ .row = 0, .col = 0 }, .height = 3, .width = 4 };
    var rb = try RenderBuffer.init(test_alloc, vp);
    defer rb.deinit();

    rb.lines.items[2].appendAssumeCapacity(0);
    rb.lines.items[2].appendAssumeCapacity(1);
    rb.lines.items[2].appendAssumeCapacity(2);
    rb.lines.items[2].appendAssumeCapacity(4);

    try expect(rb.lines.items.len == vp.height);
    try expect(rb.lines.items[1].capacity >= vp.width);
}

test "can write in buffer after resize" {
    const vp = ViewPort{ .start = .{ .row = 0, .col = 0 }, .height = 1, .width = 1 };
    var rb = try RenderBuffer.init(test_alloc, vp);
    defer rb.deinit();

    const vp2 = ViewPort{ .start = .{ .row = 0, .col = 0 }, .height = 10, .width = 10 };
    try rb.resize(test_alloc, vp2);

    rb.lines.items[9].appendAssumeCapacity(0);
    rb.lines.items[9].appendAssumeCapacity(1);
    rb.lines.items[9].appendAssumeCapacity(2);
    rb.lines.items[9].appendAssumeCapacity(4);

    try expect(rb.lines.items.len == vp2.height);
    try expect(rb.lines.items[vp2.height - 1].capacity >= vp2.width);
}

test "fill from text correct for simple case" {
    const vp = ViewPort{ .start = .{ .row = 0, .col = 0 }, .height = 1, .width = 1 };
    var rb = try RenderBuffer.init(test_alloc, vp);
    defer rb.deinit();

    const lines = rb.fillFromText("a");
    try expect(lines.len == 1);
    try expect(!lines[0].line_continues_past_buf);
    try expect(lines[0].line.len == 1);
    try expect(lines[0].line[0] == 'a');
}

test "fill from text when line continues" {
    const vp = ViewPort{ .start = .{ .row = 0, .col = 0 }, .height = 1, .width = 1 };
    var rb = try RenderBuffer.init(test_alloc, vp);
    defer rb.deinit();

    const lines = rb.fillFromText("abcde");
    try expect(lines.len == 1);
    try expect(lines[0].line_continues_past_buf);
    try expect(lines[0].line.len == 1);
    try expect(lines[0].line[0] == 'a');
}

test "fill from text when line continues and offset" {
    const vp = ViewPort{ .start = .{ .row = 0, .col = 2 }, .height = 1, .width = 1 };
    var rb = try RenderBuffer.init(test_alloc, vp);
    defer rb.deinit();

    const lines = rb.fillFromText("abcde");
    try expect(lines.len == 1);
    try expect(lines[0].line_continues_past_buf);
    try expect(lines[0].line.len == 1);
    try expect(lines[0].line[0] == 'c');
}

test "fill from text multiple lines and offset" {
    const vp = ViewPort{ .start = .{ .row = 0, .col = 2 }, .height = 3, .width = 2 };
    var rb = try RenderBuffer.init(test_alloc, vp);
    defer rb.deinit();

    const text =
        \\abcde
        \\fghij
        \\01234
    ;

    const lines = rb.fillFromText(text);
    try expect(lines.len == 3);
    for (0..3) |i| {
        try expect(lines[i].line_continues_past_buf);
    }

    try expect(std.mem.eql(u8, lines[0].line, "cd"));
    try expect(std.mem.eql(u8, lines[1].line, "hi"));
    try expect(std.mem.eql(u8, lines[2].line, "23"));
}

test "fill from fewer lines" {
    const vp = ViewPort{ .start = .{ .row = 0, .col = 2 }, .height = 3, .width = 2 };
    var rb = try RenderBuffer.init(test_alloc, vp);
    defer rb.deinit();

    const text = " ";

    const lines = rb.fillFromText(text);
    try expect(lines.len == 1);
    try expect(lines[0].line.len == 0);
    try expect(lines[0].line_ends_before_buf);
}

test "fill after resize" {
    const vp = ViewPort{ .start = .{ .row = 0, .col = 2 }, .height = 3, .width = 2 };
    var rb = try RenderBuffer.init(test_alloc, vp);
    defer rb.deinit();

    const vp2 = ViewPort{ .start = .{ .row = 0, .col = 0 }, .height = 10, .width = 10 };
    try rb.resize(test_alloc, vp2);

    const text = " ";

    const lines = rb.fillFromText(text);
    try expect(lines.len == 1);
    try expect(lines[0].line.len == 1);
    try expect(lines[0].line[0] == ' ');
    try expect(!lines[0].line_continues_past_buf);
}
