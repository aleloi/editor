const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const os = std.os;
const linux = std.os.linux;
const posix = std.posix;
const print = std.debug.print;
const panic = std.debug.panic;

const format = @import("format.zig");
const term_utils = @import("term_utils.zig");
const parse_utils = @import("parse_utils.zig");
const write_utils = @import("write_utils.zig");

const doc = @import("document.zig");
const Cursor = doc.Cursor;
const Selection = doc.Selection;
const Point = doc.Pos;


var file_content: [5 * 1024 * 1024]u8 = undefined;
var bytes_read: usize = 0;
const lines_read: usize = 100;
//var lines: [5 * 1024 * 1024][]const u8 = undefined;
/// index of first visible line
// var first_line: usize = 0;
const bottom_ui_rows: usize = 4;
/// rows reserved for permanent ui
const non_content_rows = bottom_ui_rows;
/// rows available for file content
var content_rows: usize = undefined;

/// window dimensions
var size: Size = undefined;
var tty: fs.File = undefined;

// åäö
// zig run src/mini.zig < src/parse_utils.zig &> mini.log
// zig run src/mini.zig -O ReleaseFast < src/parse_utils.zig &> mini.log
// https://ziglang.org/documentation/master/std/#std.posix.poll
// https://chatgpt.com/share/43543411-1296-4086-990d-0df98b621321

fn get_writer(buf: []u8) std.io.GenericWriter {
    var fbs = std.io.fixedBufferStream(&buf);
    return fbs.writer();
}

var parse_buf: [1000]u8 = .{128} ** 1000;
var parse_fbs = std.io.fixedBufferStream(&parse_buf);
const parse_writer = parse_fbs.writer();

/// tries to match the slice needle to a slice in haystack.
fn genericMatch(needle: []const u8, haystack: []const []const u8) bool {
    for (haystack) |straw| {
        if (std.mem.eql(u8, needle, straw)) {
            return true;
        }
    }
    return false;
}

/// tries to match a direction to the end of slice str
fn matchDirSuffix(str: []const u8) !Direction {
    for (arrows, dirs) |arrow, dir| {
        // print("'{s}' '{s}'\n", .{ str[(str.len - arrow.len)..], arrow });
        if (std.mem.eql(u8, str[(str.len - arrow.len)..], arrow)) {
            return dir;
        }
    }
    return error.Error;
}

/// return return the pair sorted by row, col
fn getSortedPoints(A: Point, B: Point) struct { Point, Point } {
    if (B.row < A.row) return .{ B, A };
    if (B.row == A.row and B.col < A.col) return .{ B, A };
    return .{ A, B };
}

/// strict cmp, bool (A < B)
fn cmpPoints(A: Point, B: Point) bool {
    if (B.row < A.row) return false;
    if (B.row == A.row and B.col <= A.col) return false;
    return true;
}

/// is A, B, C sorted?
/// specifically: is A <= B < C
fn isBetween(A: Point, B: Point, C: Point) bool {
    return ((!cmpPoints(B, A)) and cmpPoints(B, C));
}

const Direction = enum {
    up,
    down,
    left,
    right,
    fn pt(self: Direction) struct { isize, isize } {
        switch (self) {
            Direction.up => return .{ -1, 0 },
            Direction.down => return .{ 1, 0 },
            Direction.left => return .{ 0, -1 },
            Direction.right => return .{ 0, 1 },
        }
    }
};

/// quit commands
const q_eq: [3][]const u8 = .{ "ESC", "Q", "q" };
/// down commands
const j_eq: [3][]const u8 = .{ "DOWN", "J", "j" };
/// up commands
const k_eq: [3][]const u8 = .{ "UP", "K", "k" };

const dirs: [4]Direction = .{ Direction.up, Direction.down, Direction.left, Direction.right };
const arrows: [4][]const u8 = .{ "UP", "DOWN", "LEFT", "RIGHT" };
/// cursor up
const c_arrows: [4][]const u8 = .{ "CTRL+UP", "CTRL+DOWN", "CTRL+LEFT", "CTRL+RIGHT" };
/// change selection
const sc_arrows: [4][]const u8 = .{ "SHIFT+CTRL+UP", "SHIFT+CTRL+DOWN", "SHIFT+CTRL+LEFT", "SHIFT+CTRL+RIGHT" };

pub fn main() !void {
    tty = try fs.cwd().openFile("/dev/tty", .{ .mode = .read_write });
    defer tty.close();

    try term_utils.uncook(tty);
    defer term_utils.cook(tty) catch {};

    //try getInp();

    size = try getSize();

    var fds: [1]posix.pollfd = .{.{
        .fd = tty.handle,
        .events = posix.POLL.IN,
        .revents = undefined,
    }};

    // set last visible line
    // moveView(0);

    //try render(null, .{});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    const rope = try doc.openAsRope(alloc, "src/mini.zig"); // 6k
    //const rope = try doc.openAsRope(alloc, "/home/alex/Downloads/data-1717158044627.csv"); // 17M
    //const rope = try doc.openAsRope(alloc, "/home/alex/Downloads/data-1720544170329.csv"); // 100k
    // /home/alex/Downloads/data-1717158044627.csv 17M

    defer rope.releaseWithFn(@TypeOf(rope.value.*).deinit);
    var vp = doc.ViewPort {.height = size.height-2, .width=size.width-1};
    var dc = try doc.Document.init(alloc, vp.height, vp.width, rope);

    {
        const txt = try dc.getText();
        try render(&.{}, dc.cursor, txt, vp);
    }

    while (true) {
        print("\nvp: {}, dc.vp: {}\n\n", .{vp, dc.render_buffer.viewport});
        // var buffer: [1]u8 = undefined;
        // _ = try tty.read(&buffer);

        _ = try posix.poll(&fds, -1);
        var buffer: [1000]u8 = .{255} ** 1000;

        const num_read = try tty.read(&buffer);
        if (num_read == 0) continue;

        print("\nall bytes {any}\n", .{buffer[0..num_read]});
        var cmd_it = parse_utils.InputSeqIterator{ .bytes = buffer[0..num_read] };

        while (try cmd_it.next()) |cmd| {
            const txt_old = try dc.getText();
            print("\n single cmd {any}\n", .{cmd});
            parse_fbs.reset();
            try parse_utils.parseWrite(cmd, parse_writer);
            const cmd2: []const u8 = parse_fbs.getWritten();

            if (genericMatch(cmd2, &q_eq)) {
                return;
            } else if (genericMatch(cmd2, &j_eq)) {
                // next line
                // if (lines_read + 20 > size.height and first_line < lines_read - 20) {
                //     first_line += 1;
                // }
                vp = moveView(1, vp);
            } else if (genericMatch(cmd2, &k_eq)) {
                // previous line
                // if (first_line > 0) {
                //     first_line -= 1;
                // }
                vp = moveView(-1, vp);
            } else if (genericMatch(cmd2, &c_arrows)) {
                // ctrl+arrow, move cursor
                //  move cursor
                moveVCursorStep(&dc.cursor, try matchDirSuffix(cmd2),  &vp, txt_old);
                // reset selection
                dc.cursor.selection = Selection.emptySel(dc.cursor.pos);
            } else if (genericMatch(cmd2, &sc_arrows)) {
                // shift+ctrl+arrow, move cursor & selection
                // move cursor
                moveVCursorStep(&dc.cursor, try matchDirSuffix(cmd2),  &vp, txt_old);
                // update selection
                dc.cursor.selection.head = dc.cursor.pos;
            }
            print("\nAfter handling commands: vp: {}, dc.vp: {}\n\n", .{vp, dc.render_buffer.viewport});
            try dc.render_buffer.resize(vp);
            const txt = try dc.getText();
            //try render(&.{}, dc.cursor, txt, vp);
            try render(cmd, dc.cursor, txt, vp);
        }
    }
}

// fn renderLines(writer: anytype, lns: []const doc.LineSlice, bottom_bytes: []const u8, cursor: Cursor) !void {
//     // const tty_writer = tty.writer();
//     // var buf_writer = std.io.bufferedWriter(tty_writer);
//     // const writer = buf_writer.writer();

//     //try clear(writer);

//     for (lns, 0..) |line, i| {
//         try writeLine(writer, line.line, i);
//     }

//     //try render_bottom_ui(bottom_bytes, writer, cursor);
//     //try buf_writer.flush();
// }

/// render the current view
fn render(maybe_bytes: ?[]const u8, cursor: Cursor, lns: [] const doc.LineSlice, view: doc.ViewPort) !void {
    const tty_writer = tty.writer();
    var buf_writer = std.io.bufferedWriter(tty_writer);
    const writer = buf_writer.writer();

    try clear(writer);

    for (lns, 0..) |line, i| {
        try writeLine(writer, line.line, i);
    }

    //try renderLines(writer);
    try render_sel(writer, cursor, view, lns);
    try render_cursor(writer, cursor, view, lns);
    try render_bottom_ui(maybe_bytes, writer, cursor, view);
    _ = &maybe_bytes;

    try buf_writer.flush();
}

// /// render file content
// fn render_file_content(writer: anytype) !void {
//     // const last_line: usize = @min(first_line + content_rows, lines_read);
//     const first_line = view.fst;
//     const last_line = view.start.row+view.height;
//     for (first_line..last_line) |line_ind| {
//         try writeLine(writer, lines[line_ind], line_ind - first_line);
//     }
// }

/// render bottom ui
fn render_bottom_ui(maybe_bytes: ?[]const u8, arg_writer: anytype, cursor: Cursor, view: doc.ViewPort) !void {
    // var rawbuf
    var multi_writer = write_utils.multiWriter(arg_writer);
    const writer = multi_writer.writer();
    // input?
    if (maybe_bytes) |bytes| {
        // input given
        // raw input row
        try moveCursor(writer, size.height - 2, 0);
        try writer.writeAll("Raw input:       ");
        try parse_utils.rawWrite(bytes, writer);
        // parsed input row
        try moveCursor(writer, size.height - 1, 0);
        try writer.writeAll("Parsed input:    ");
        try parse_utils.parseWrite(bytes, writer);
    } else {
        // no input
        try moveCursor(writer, size.height - 2, 0);
        try writer.writeAll("No input recieved!");
    }
    // status row
    try moveCursor(writer, size.height - 3, 0);
    try writer.print("selection anchor {any: >3}   head {any: >3}", .{ cursor.selection.anchor, cursor.selection.head });
    try moveCursor(writer, size.height - 4, 0);
    try writer.print("view {any: >3}   cursor {any: >3} (move using CTRL+<arrow>)", .{ view, cursor.pos });
}

fn render_sel(writer: anytype, cursor: Cursor, view: doc.ViewPort, lns: []const doc.LineSlice) !void {
    const min_p, const max_p = getSortedPoints(cursor.selection.anchor, cursor.selection.head);
    const min_r = min_p.row;
    const max_r = max_p.row;
    // var max_c: usize = undefined;
    // for (min_r..(max_r + 1)) |row_i| max_c = @max(max_c, lines[row_i].len);
    // if (max_r <= min_r) return;
    for (min_r..(max_r + 1)) |row_i| {
        if (row_i < view.start.row or view.start.row+view.height <= row_i) continue;
        const rel_row_i = row_i - view.start.row;
        const line: doc.LineSlice = lns[rel_row_i];

        // TODO the below is only correct for col==0
        std.debug.assert(view.start.col==0);
        for (0..line.line.len, line.line) |col_i, ch| {
            if (isBetween(min_p, .{.row=row_i, .col=col_i}, max_p)) {
                try moveCursor(writer, row_i - view.start.row, col_i);
                // underline
                try writer.writeAll("\x1B[4m");
                var byte = ch;
                if (byte < 32 or byte > 126) byte = ' ';
                try writer.writeByte(byte);
                try writer.writeAll("\x1B[0m");
            }
        }
    }
}

fn render_cursor(writer: anytype, cursor: Cursor, view: doc.ViewPort, lns: []const doc.LineSlice) !void {
    const row = cursor.pos.row;
    const col = cursor.pos.col;
    // if cursor.pos
    if (view.start.row <= row and row < view.start.row+view.height) {
        try moveCursor(writer, row - view.start.row, col);
        // // white bg
        // try writer.writeAll("\x1B[47m");
        // reverse fg/bg
        try writer.writeAll("\x1B[7m");
        // blink
        try writer.writeAll("\x1B[5m");
        // try writer.writeAll(" ");

        // TODO fix so it works without the condition:
        std.debug.assert(view.start.col == 0);
        const line = lns[row-view.start.row].line;
        //if (col >= line.len)
        var byte = if (col < line.len) line[col] else 0;
        if (byte < 32 or byte > 126) byte = ' ';
        try writer.writeByte(byte);
        try writer.writeAll("\x1B[0m");
    }
}

/// write <txt> to the buffer at row y col 0, applying format.myFmtLine
fn writeLine(writer: anytype, txt: []const u8, y: usize) !void {
    try moveCursor(writer, y, 0);
    try writer.print("{}", .{format.myFmtLine(txt)});
}

/// move the cursor to row, col, using 0-indexing
fn moveCursor(writer: anytype, row: usize, col: usize) !void {
    try writer.print("\x1B[{};{}H", .{ row + 1, col + 1 });
}

/// clear the buffer
fn clear(writer: anytype) !void {
    try writer.writeAll("\x1B[2J");
}

const Size = struct { width: usize, height: usize };
/// get the window size
fn getSize() !Size {
    var win_size = mem.zeroes(linux.winsize);
    if (linux.ioctl(tty.handle, linux.T.IOCGWINSZ, @intFromPtr(&win_size)) != 0) {
        @panic("getsize failed ioctl()");
    }
    const height: usize = win_size.ws_row;
    // update number of rows available for content
    if (height < non_content_rows) unreachable;
    content_rows = win_size.ws_row - non_content_rows;
    return Size{
        .height = win_size.ws_row,
        .width = win_size.ws_col,
    };
}

// /// read file content from stdin.
// /// raise exception if longer than 5 MB.
// fn getInp() !void {
//     const stdin = std.io.getStdIn().reader();
//     bytes_read = try stdin.readAll(&file_content);
//     if (bytes_read == file_content.len) @panic("getInp file too long");
//     var split_it = std.mem.splitSequence(u8, file_content[0..bytes_read], "\n");
//     while (split_it.next()) |line| {
//         lines[lines_read] = line;
//         lines_read += 1;
//     }
// }
/// first line, last line. lst not included
// const View = struct {
//     fst: usize = 0,
//     lst: usize = 0,
// };
// var view: View = .{};
fn moveView(ind: isize, vp: doc.ViewPort) doc.ViewPort {
    var vpc = vp;
    //const lines_read: usize = 100;
    if (ind > 0) {
        vpc.start.row += @min(@abs(ind), lines_read - vpc.start.row - @min(10, lines_read));
    } else if (ind < 0) {
        // const abs: usize = ind;
        vpc.start.row -= @min(@abs(ind), vpc.start.row);
    }
    //view.start.row+view.height = @min(lines_read, view.start.row + content_rows);
    //if (view.start.row+view.height <= view.start.row or view.start.row+view.height > lines_read + 1) panic("moveView caused invalid view {any}\n", .{view});
    return vpc;
}

// // 0-indexing
// const Point = struct {
//     row: usize,
//     col: usize,
// };
fn point(row: usize, col: usize) Point {
    return .{ .row = row, .col = col };
}

// /// a point, and a selection
// const Cursor = struct {
//     pos: Point,
//     target_col: usize = 0,
//     selection: Selection = emptySel(point(1, 0)),
// };
// const def_pos = point(0, 0);
// var cursor: Cursor = .{ .pos = def_pos, .selection = emptySel(def_pos) };

/// move the cursor single step in cardinal direction
fn moveVCursorStep(cursor: *Cursor, dir: Direction, view: *doc.ViewPort, lns: [] const doc.LineSlice) void {
    // TODO breaks when cursor is outside of viewport...
    std.debug.assert(view.*.start.col==0);
    std.debug.assert(view.*.start.row <= cursor.*.pos.row and cursor.*.pos.row < view.*.start.row + view.*.height);

    // current pos
    var crow = cursor.pos.row;
    var ccol = cursor.pos.col;

    const view_start_row = view.*.start.row;
    //
    const n_row = lines_read;

    switch (dir) {
        Direction.up => {
            if (crow < 1) {
                ccol = 0;
            } else {
                crow -= 1;
                ccol = @min(lns[crow-view_start_row].line.len, cursor.target_col);
            }
        },
        Direction.down => {
            if (crow >= n_row - 1) {
                ccol = lns[crow-view_start_row].line.len;
            } else {
                crow += 1;
                ccol = @min(lns[crow-view_start_row].line.len, cursor.target_col);
            }
        },
        Direction.left => {
            if (ccol < 1) {
                if (crow > 0) {
                    crow -= 1;
                    ccol = lns[crow-view_start_row].line.len;
                } else {}
            } else {
                ccol -= 1;
            }
            cursor.target_col = ccol;
        },
        Direction.right => {
            if (ccol >= lns[crow-view_start_row].line.len) {
                if (crow < n_row - 1) {
                    crow += 1;
                    ccol = 0;
                } else {}
            } else {
                ccol += 1;
            }
            cursor.target_col = ccol;
        },
    }
    // save changes
    cursor.pos = .{.row=crow, .col=ccol};

    // make sure the cursor is visible after being moved
    if (crow < view.start.row) view.* = moveView(-@as(isize, @intCast(view.start.row - crow)), view.*);
    if (crow >= view.start.row+view.height) view.* = moveView(@intCast(view.start.row+view.height + 1 - crow), view.*);
}

// /// move the the cursor
// fn moveVCursorRel(rows: isize, cols: isize, cursor: *Cursor, view: *doc.ViewPort, lns: []const doc.LineSlice) void {
//     var crow = cursor.pos.row;
//     var ccol = cursor.pos.col;
//     const view_start_row = view.*.start.row;

//     if (rows < 0) {
//         crow -= @min(crow, @abs(rows));
//     } else if (rows > 0) {
//         crow += @min(lines_read - crow, @abs(rows));
//     }
//     const clen = lns[crow-view_start_row].len;
//     if (cols == 0) {
//         ccol = @min(clen, cursor.target_col);
//     } else {
//         if (cols > 0) {
//             ccol += @min(clen - ccol, @abs(cols));
//         } else if (cols < 0) {
//             ccol -= @min(ccol, @abs(cols));
//         }
//         cursor.target_col = ccol;
//     }
//     // save changes
//     cursor.pos = .{.row=crow, .colr=ccol};

//     // make sure the cursor is visible after being moved
//     if (crow < view.start.row) moveView(-@as(isize, @intCast(view.start.row - crow)));
//     if (crow >= view.start.row+view.height) moveView(@intCast(view.start.row+view.height + 1 - crow));
// }

// /// try to move the cursor to target point
// fn setVCursorPos(target: Point, cursor: *Cursor) void {
//     var trow = target.row;
//     var tcol = target.col;
//     if (trow > lines_read) trow = lines_read;
//     const tlen = lines[trow].len;
//     if (tcol > tlen) tcol = tlen;
//     cursor.pos = .{.row=trow, .col=tcol};
//     cursor.target_col = tcol;
// }

// /// movable head, immovable anchor.
// /// regular cursor/empty selection: head = anchor
// const Selection = struct {
//     head: Point,
//     anchor: Point,
// };
// fn emptySel(pos: Point) Selection {
//     return .{ .head = pos, .anchor = pos };
// }

// this seems to ensure all tests are run
// when placed in the root file
test {
    std.testing.refAllDecls(@This());
    // or refAllDeclsRecursive
}
