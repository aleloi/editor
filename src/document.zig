const std = @import("std");
const rope = @import("rope");
const rb = @import("render_buffer.zig");

const Rc = rope.Rc;
const Pos = rope.Pos;
const RopeRc = rope.Node.RcSelf;
const Rope = rope.Node;
pub const RenderBuffer = rb.RenderBuffer;
pub const ViewPort = rb.ViewPort;

pub const LineSlice = RenderBuffer.LineSlice;


pub const Document = struct {
    // Somehow force this to always be non-empty though the type
    // system?
    history: std.ArrayList(RopeRc),

    //viewport: ViewPort, - NO, not here; it's part of the render buffer
    alloc: std.mem.Allocator,

    // More than one in the future!
    cursor: Pos = .{.row=0, .col=0},
    render_buffer: RenderBuffer,

    pub fn init(alloc: std.mem.Allocator, h: usize, w: usize, rp: RopeRc) !Document {
        const vp = ViewPort {.height = h, .width=w};
        var res: Document = .{.alloc=alloc,
                              .history=std.ArrayList(RopeRc).init(alloc),
                              .render_buffer = try RenderBuffer.init(alloc, vp)};
        var rope_v = rp;
        try res.history.append(rope_v.retain());
        errdefer rope_v.releaseWithFn(rope.Node.deinit);
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
};


pub fn openAsRope(alloc: std.mem.Allocator, rel_fname: [] const u8) !RopeRc {
    //std.debug.print("AAAaaaa\n", .{});
    const dir: std.fs.Dir = std.fs.cwd();

    //var path_buf: [1000] u8 = undefined;
    //const slice = try dir.realpath(rel_fname, &path_buf);
    //std.debug.print("abs path is: {s}\n", .{slice});

    const fl: std.fs.File = try dir.openFile(rel_fname, .{});
    defer fl.close();

    const buf: []const u8 = try fl.readToEndAlloc(alloc, 1_000_000);
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
