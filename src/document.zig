const std = @import("std");
const rope = @import("rope");
const rb = @import("render_buffer.zig");

const Rc = rope.Rc;
pub const Pos = rope.Pos;
const RopeRc = rope.Node.RcSelf;
const Rope = rope.Node;
pub const RenderBuffer = rb.RenderBuffer;
pub const ViewPort = rb.ViewPort;

pub const LineSlice = RenderBuffer.LineSlice;

/// movable head, immovable anchor.
/// regular cursor/empty selection: head = anchor
/// Idea: have a nullable selection instead of empty selection?
pub const Selection = struct {
    head: Pos,
    anchor: Pos,

    pub fn emptySel(pos: Pos) Selection {
        return .{ .head = pos, .anchor = pos };
    }
};

pub const Direction = enum {
    up,
    down,
    left,
    right,
    pub fn pt(self: Direction) struct { isize, isize } {
        switch (self) {
            Direction.up => return .{ -1, 0 },
            Direction.down => return .{ 1, 0 },
            Direction.left => return .{ 0, -1 },
            Direction.right => return .{ 0, 1 },
        }
    }
};

/// TODO document
pub const Cursor = struct {
    /// Relative to the document, not the view.
    pos: Pos = .{.row=0, .col=0},
    target_col: usize = 0,
    selection: Selection = Selection.emptySel(.{.row=0, .col=0}),

    // fn posInsideView ?

    /// TODO: feedback when the cursor reaches the top / bottom?
    pub fn move(self: *@This(), rp_rc: RopeRc, dir: Direction) void {
        const rp: Rope = rp_rc.value.*;
        const num_rows: usize = rp.numRows();

        std.debug.assert(self.pos.row <= num_rows);
        const curr_row_length: usize = rp.rowLength(self.pos.row) catch unreachable;
        const p = self.pos;

        // New row / col positions.
        var n_row: usize = p.row;
        var n_col: usize = p.col;
        var n_target_col: usize = self.target_col;


        switch (dir) {
            .up => {
                n_row = if (p.row > 0) p.row-1 else 0;
                n_col = @min(rp.rowLength(n_row) catch unreachable, self.target_col);
            },
            .down => {
                n_row = if (p.row < num_rows) p.row+1 else p.row;
                n_col = @min(rp.rowLength(n_row) catch unreachable, self.target_col);
            },
            .left => {
                if (p.col > 0) {
                    n_col = p.col-1;
                } else if (p.row > 0) {
                    n_row -= 1;
                    n_col = rp.rowLength(n_row) catch unreachable;
                }
                n_target_col = n_col;
            },
            .right => {
                if (p.col < curr_row_length) {
                    n_col = p.col+1;
                } else if (p.row < num_rows) {
                    n_row += 1;
                    n_col = 0;
                }
                n_target_col = n_col;
            }
        }

        self.pos = .{.row = n_row, .col = n_col};
        self.target_col = n_target_col;
    }

    // fn up(self: @This()) Cursor {
    //     const row = if (p.row > 0) p.row-1 else 0;
    //     const col = @min(rp.rowLength(n_row), self.target_col);
    // }
};

/// TODO document the document
pub const Document = struct {
    // Somehow force this to always be non-empty through the type
    // system?
    history: std.ArrayList(RopeRc),

    //viewport: ViewPort, - NO, not here; it's part of the render buffer
    alloc: std.mem.Allocator,

    // More than one in the future!
    cursor: Cursor = .{},
    render_buffer: RenderBuffer,

    pub fn init(alloc: std.mem.Allocator, h: usize, w: usize, rp: RopeRc) !Document {
        const vp = ViewPort {.height = h, .width=w};
        var res: Document = .{.alloc=alloc,
                              .history=std.ArrayList(RopeRc).init(alloc),
                              .render_buffer = try RenderBuffer.init(alloc, vp)};
        var rope_v = rp;
        try res.history.append(rope_v.retain());
        return res;
    }

    pub fn deinit(self: *@This()) void {
        for (self.history.items) |itm| {
            itm.releaseWithFn(rope.Node.deinit);
        }
        self.history.deinit();
        self.render_buffer.deinit();
    }

    /// Requires self to be mut because it writes into internal buffer
    /// of the render buffer.
    pub fn getText(self: *@This()) ![]const RenderBuffer.LineSlice {
        const curr_whole = self.history.getLast();
        //const root = curr_whole.value.*;
        const vp = self.render_buffer.viewport;

        // Currently only extracting whole lines.
        const start_pos = Pos {.row = vp.start.row, .col=0};
        const start_offset = curr_whole.value.*.posToOffset(start_pos) catch return &.{};

        const end_pos = Pos {.row = start_pos.row + vp.height, .col=0};
        // Will break, TODO patch the Rope to just give me everything.
        const end_offset = curr_whole.value.*.posToOffset(end_pos) catch curr_whole.value.*.agg.num_bytes;

        const start_onwards: RopeRc = b: {
            var split = try Rope.splitAt(curr_whole, start_offset);
            split.fst.releaseWithFn(Rope.deinit);
            break :b split.snd;
        };
        defer start_onwards.releaseWithFn(Rope.deinit);

        const visible_piece = bb: {
            var split = try Rope.splitAt(start_onwards, end_offset - start_offset);
            split.snd.releaseWithFn(Rope.deinit);
            break :bb split.fst;
        };
        defer visible_piece.releaseWithFn(Rope.deinit);

        // UNSAFE (and inefficient), just for testing, TODO fix!
        var huge_buf: [1_000_000] u8 = undefined;
        var iter = visible_piece.value.*.allBytesIterator();
        var i: usize = 0;
        while (iter.next()) |c| {
            huge_buf[i] = c;
            i += 1;
        }
        return self.render_buffer.fillFromText(huge_buf[0..i]);
    }

    // /// How many lines does the current version of the doc have?
    pub fn numRows(self: @This()) usize {
        std.debug.assert(self.history.items.len > 0);
        return self.history.items[0].value.*.numLines();
        // const v_agg: rope.AggregateStats = self.history.items[0].value.*;
        // return v.agg.num_newlines + 1;
    }

    // /// Passthough to curent history.
    // pub fn rowLength(self: @This(), row: usize) !usize {
    //     std.debug.assert(self.history.items.len > 0);
    //     return self.history.items[0].value.*.rowLength(row);
    // }

    /// Too many layers? Also it makes sense to have this in Cursor
    pub fn moveCursor(self: *@This(), dir: Direction) void {
        self.cursor.move(self.history.items[0], dir);
    }

    pub fn moveView(self: *@This(), dy: isize) void {
        const vp: *ViewPort = &self.render_buffer.viewport;
        const num_rows = self.*.history.items[0].value.*.numRows();
        const curr_row: * usize = &vp.*.start.row;
        if (dy > 0) {
            // Max viewpoint start is doc size + num visible rows:
            curr_row.* = @min(curr_row.* + @abs(dy), num_rows + vp.*.height);
        } else if (dy < 0) {
            curr_row.* -= @min(@abs(dy), curr_row.*);
        }
    }
};


pub fn openAsRope(alloc: std.mem.Allocator, rel_fname: [] const u8) !RopeRc {
    //std.debug.print("AAAaaaa\n", .{});
    const dir: std.fs.Dir = std.fs.cwd();

    //var path_buf: [1000] u8 = undefined;
    //const slice = try dir.realpath(rel_fname, &path_buf);
    //std.debug.print("abs path is: {s}\n", .{slice});

    const fl: std.fs.File = try dir.openFile(rel_fname, .{});
    defer fl.close();

    const buf: []const u8 = try fl.readToEndAlloc(alloc, 20_000_000);
    defer alloc.free(buf);

    return try Rope.fromSlice(buf);
}


test "rope from this file test" {
    const rp = try openAsRope(std.testing.allocator, "src/document.zig");
    defer rp.releaseWithFn(Rope.deinit);
}


test "create doc test no leaks" {
    const rp = try openAsRope(std.testing.allocator, "src/document.zig");
    defer rp.releaseWithFn(Rope.deinit);

    var doc = try Document.init(std.testing.allocator, 10, 20, rp);
    defer doc.deinit();
}

test "get text test ocular inspection" {
    const rp = try openAsRope(std.testing.allocator, "src/document.zig");
    defer rp.releaseWithFn(Rope.deinit);

    var doc = try Document.init(std.testing.allocator, 10, 20, rp);
    defer doc.deinit();

    try doc.render_buffer.resize(.{.height = 30, .width = 30, .start = .{.row=3, .col=5}});

    const txt = try doc.getText();
    for (txt) |slc| {
        const pre: u8 = if (slc.line_ends_before_buf) '<' else ' ';
        const post: u8 = if (slc.line_continues_past_buf) '>' else ' ';
        std.debug.print("{c}{s}{c}\n", .{pre, slc.line, post});
    }
}
