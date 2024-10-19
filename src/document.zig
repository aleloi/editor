const tracy = @import("tracy");
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


/// Standard ownership semantics: caller is responsile for releasing
/// `to_insert` and `tree`.
fn insertAtOffset(tree: RopeRc, to_insert: RopeRc, offset: usize) !RopeRc {
    const splt = try Rope.splitAt(tree, offset);

    const before: RopeRc = splt.fst;
    const after: RopeRc = splt.snd;
    defer before.releaseWithFn(Rope.deinit);
    defer after.releaseWithFn(Rope.deinit);

    const before_and_insert: RopeRc = try Rope.concat(before, to_insert);
    defer before_and_insert.releaseWithFn(Rope.deinit);

    return try Rope.concat(before_and_insert, after);
}

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

/// TODO document
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

    //pub fn moveToBeginningOfLine()

    /// TODO: feedback when the cursor reaches the top / bottom?
    pub fn move(self: *@This(), rp_rc: RopeRc, dir: Direction) void {
        const zone = tracy.initZone(@src(), .{ .name = "Move cursor" });
        defer zone.deinit();
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

    /// Where does the cursor end up if we paste something with info
    /// `agg` at its position, and then move the cursor past the
    /// inserted text?
    pub fn forwardByAgg(self: *@This(), agg: rope.AggregateStats) void {
        self.pos.row += agg.num_newlines;
        if (agg.num_newlines == 0) {
            self.pos.col += agg.num_bytes;
        } else {
            self.pos.col = agg.num_bytes - agg.last_newline_pos.?;
        }
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
        const start_offset: usize = b: {
            const zone = tracy.initZone(@src(), .{ .name = "start offset" });
            defer zone.deinit();
            break :b curr_whole.value.*.posToOffset(start_pos) catch return &.{};
        };

        const end_pos = Pos {.row = start_pos.row + vp.height, .col=0};
        // Will break, TODO patch the Rope to just give me everything.
        const end_offset = curr_whole.value.*.posToOffset(end_pos) catch curr_whole.value.*.agg.num_bytes;

        const start_onwards: RopeRc = b: {
            const zone = tracy.initZone(@src(), .{ .name = "Split tree 1" });
            defer zone.deinit();
            var split = try Rope.splitAt(curr_whole, start_offset);
            split.fst.releaseWithFn(Rope.deinit);
            break :b split.snd;
        };
        defer start_onwards.releaseWithFn(Rope.deinit);

        const visible_piece = bb: {
            const zone = tracy.initZone(@src(), .{ .name = "Split tree 2" });
            defer zone.deinit();
            var split = try Rope.splitAt(start_onwards, end_offset - start_offset);
            split.snd.releaseWithFn(Rope.deinit);
            break :bb split.fst;
        };
        defer visible_piece.releaseWithFn(Rope.deinit);

        // UNSAFE (and inefficient), just for testing, TODO fix!
        var huge_buf: [1_000_000] u8 = undefined;
        var i: usize = 0;

        {
            const zone = tracy.initZone(@src(), .{ .name = "Tree read chars" });
            defer zone.deinit();
            var iter = visible_piece.value.*.allBytesIterator();
            while (iter.next()) |c| {
                huge_buf[i] = c;
                i += 1;
            }
        }

        return b: {
            const zone = tracy.initZone(@src(), .{ .name = "Filling render buffer" });
            defer zone.deinit();
            break :b self.render_buffer.fillFromText(huge_buf[0..i]);
        };

    }

    // /// How many lines does the current version of the doc have?
    pub fn numRows(self: @This()) usize {
        std.debug.assert(self.history.items.len > 0);
        return self.history.getLast().value.*.numLines();
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
        self.cursor.move(self.history.getLast(), dir);
    }

    /// todo document
    pub fn moveView(self: *@This(), dy: isize) void {
        const vp: *ViewPort = &self.render_buffer.viewport;
        const num_rows = self.history.getLast().value.*.numRows();
        const curr_row: * usize = &vp.*.start.row;
        if (dy > 0) {
            // Max viewpoint start is doc size + num visible rows:
            curr_row.* = @min(curr_row.* + @abs(dy), num_rows + vp.*.height);
        } else if (dy < 0) {
            curr_row.* -= @min(@abs(dy), curr_row.*);
        }
    }

    /// Insert `text` at current cursor position, set cursor after it
    /// and (todo implement?) set viewport so that cursor is visible.
    /// TODO make a thing that inserts a TREE (useful for pasting),
    /// and call it from here.
    pub fn insertAtCursor(self: *@This(), text: []const u8) !void {
        const pos: Pos = self.cursor.pos;
        const last: RopeRc = self.history.getLast();

        const to_insert: RopeRc = try Rope.fromSlice(text);
        defer to_insert.releaseWithFn(Rope.deinit);

        const res = try insertAtOffset(
            last, to_insert,
            last.value.*.posToOffset(pos) catch last.value.*.agg.num_bytes);
        errdefer res.releaseWithFn(Rope.deinit);

        try self.history.append(res);
        self.cursor.forwardByAgg(to_insert.value.*.agg);
    }




    pub fn pasteSelection(self: *@This()) !void {
        const last: RopeRc = self.history.getLast();
        //const pos_offset = last.value.*.posToOffset(self.cursor.pos) catch last.value.*.agg.num_bytes;


        const begin, const end = b: {
            const a: Pos = self.cursor.selection.head;
            const b: Pos = self.cursor.selection.anchor;
            const bp = if (a.lexLe(b)) a else b;
            const ep = if (a.lexLe(b)) b else a;

            break :b .{
                last.value.*.posToOffset(bp) catch last.value.*.agg.num_bytes,
                last.value.*.posToOffset(ep) catch last.value.*.agg.num_bytes
            };
        };

        std.debug.print("PASTE SELECTION; Begin: {}, end: {}\n", .{begin, end});

        const begin_onwards: RopeRc = b: {
            const splt = try Rope.splitAt(last, begin);
            splt.fst.releaseWithFn(Rope.deinit);
            break :b splt.snd;
        };
        defer begin_onwards.releaseWithFn(Rope.deinit);

        const begin_to_end: RopeRc = b: {
            const splt = try Rope.splitAt(begin_onwards, end - begin);
            splt.snd.releaseWithFn(Rope.deinit);
            break :b splt.fst;
        };
        std.debug.print("begin-to-end agg {any}\n", .{begin_to_end.value.*.agg});
        defer begin_to_end.releaseWithFn(Rope.deinit);

        const res = try insertAtOffset(
            last, begin_to_end,
            last.value.*.posToOffset(self.cursor.pos) catch last.value.*.agg.num_bytes);
        errdefer res.releaseWithFn(Rope.deinit);

        try self.history.append(res);
        self.cursor.forwardByAgg(begin_to_end.value.*.agg);
    }

    pub fn undo(self: *@This()) void {
        if (self.history.items.len > 1) {
            _ = self.history.pop();
        }
    }

    pub fn cursorPgUp(_: *@This()) void{}
    pub fn cursorPgDn(_: *@This()) void{}
    pub fn cursorHome(self: *@This()) void{
        self.cursor.pos.col = self.render_buffer.viewport.start.col;
    }
    pub fn cursorEnd(_: *@This()) void{}
};


pub fn openAsRope(alloc: std.mem.Allocator, rel_fname: [] const u8) !RopeRc {
    //std.debug.print("AAAaaaa\n", .{});
    const dir: std.fs.Dir = std.fs.cwd();

    //var path_buf: [1000] u8 = undefined;
    //const slice = try dir.realpath(rel_fname, &path_buf);
    //std.debug.print("abs path is: {s}\n", .{slice});

    const fl: std.fs.File = try dir.openFile(rel_fname, .{});
    defer fl.close();

    //fl.writer();

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
