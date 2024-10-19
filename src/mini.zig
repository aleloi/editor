const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const os = std.os;
const linux = std.os.linux;
const posix = std.posix;
const print = std.debug.print;
const panic = std.debug.panic;

const doc = @import("document.zig");
const Cursor = doc.Cursor;
const Selection = doc.Selection;
const Point = doc.Pos;
const Direction = doc.Direction;

const tracy = @import("tracy");

const format = @import("format.zig");
const term_utils = @import("term_utils.zig");
const parse_utils = @import("parse_utils.zig");
const write_utils = @import("write_utils.zig");
const misc_utils = @import("misc_utils.zig");
const selection_utils = @import("selection_utils.zig");
const logging = @import("logging.zig");

pub const std_options = logging.std_options;


/// index of first visible line
// var first_line: usize = 0;
const bottom_ui_rows: usize = 4;
/// rows reserved for permanent ui
const non_content_rows = bottom_ui_rows;
const non_content_cols: usize = 5;  // TODO!

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

var parse_buf: [1000]u8 = undefined;
var parse_fbs = std.io.fixedBufferStream(&parse_buf);
const parse_writer = parse_fbs.writer();

/// tries to match the slice needle to a slice in haystack.
fn genericMatch(needle: []const u8, haystack: []const []const u8) bool {
    const zone = tracy.initZone(@src(), .{ .name = "generic match" });
    defer zone.deinit();
    for (haystack) |straw| {
        if (std.mem.eql(u8, needle, straw)) {
            return true;
        }
    }
    return false;
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

/// quit commands
const q_eq: [3][]const u8 = .{ "ESC", "Q", "q" };
/// down commands
const j_eq: [3][]const u8 = .{ "DOWN", "J", "j" };
/// up commands
const k_eq: [3][]const u8 = .{ "UP", "K", "k" };

pub const arrows: [4][]const u8 = .{ "UP", "DOWN", "LEFT", "RIGHT" };
/// move cursor
const c_arrows: [4][]const u8 = .{ "CTRL+UP", "CTRL+DOWN", "CTRL+LEFT", "CTRL+RIGHT" };
/// change selection
const sc_arrows: [4][]const u8 = .{ "SHIFT+CTRL+UP", "SHIFT+CTRL+DOWN", "SHIFT+CTRL+LEFT", "SHIFT+CTRL+RIGHT" };
/// move cursor fn+arrow
const c_fn_arrows: [4][]const u8 = .{ "CTRL+PGUP", "CTRL+PGDN", "CTRL+HOME", "CTRL+END" };
/// change selection fn+arrow
const sc_fn_arrows: [4][]const u8 = .{ "SHIFT+CTRL+PGUP", "SHIFT+CTRL+PGDN", "SHIFT+CTRL+HOME", "SHIFT+CTRL+END" };

const paste_sel: [1][]const u8 = .{ "CTRL+y" };

const insert_mode: [1][]const u8 = .{"i"};

const normal_mode: [1][]const u8 = .{"ASCII-ESC / ESC / CTRL+8"};
const undo: [1][]const u8 = .{"CTRL+u"};


const Mode = enum {
    insert,
    normal
};

var mode: Mode = .normal;

pub fn main() !void {
    tty = try fs.cwd().openFile("/dev/tty", .{ .mode = .read_write });
    defer tty.close();

    try term_utils.uncook(tty);
    defer term_utils.cook(tty) catch {};

    try logging.logger_init(null);
    defer logging.logger_deinit();


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


    const rope = try doc.openAsRope(alloc, "src/document.zig"); // 6k
    //const rope = try doc.openAsRope(alloc, "/home/alex/Downloads/data-1717158044627.csv"); // 17M
    //const rope = try doc.openAsRope(alloc, "/home/alex/Downloads/data-1720544170329.csv"); // 100k
    // /home/alex/Downloads/data-1717158044627.csv 17M

    defer rope.releaseWithFn(@TypeOf(rope.value.*).deinit);
    //const vp = doc.ViewPort {.height = size.height-2, .width=size.width-1};
    var dc = try doc.Document.init(alloc, size.height-non_content_rows, size.width-4, rope);

    {
        const txt = try dc.getText();
        try render(&.{}, dc.cursor, txt, dc.render_buffer.viewport);
    }

    while (true) {
        // var buffer: [1]u8 = undefined;
        // _ = try tty.read(&buffer);

        _ = try posix.poll(&fds, -1);
        var buffer: [1000]u8 = .{255} ** 1000;

        var cmd_it = b: {
            const zone = tracy.initZone(@src(), .{ .name = "Parsing into buffer" });
            defer zone.deinit();
            const num_read = try tty.read(&buffer);
            if (num_read == 0) continue;

            print("\nall bytes {any}\n", .{buffer[0..num_read]});
            break :b parse_utils.InputSeqIterator{ .bytes = buffer[0..num_read] };
        };



        while (try cmd_it.next()) |cmd| {
            const zone = tracy.initZone(@src(), .{ .name = "Handling command" });
            defer zone.deinit();

            //const txt_old = try dc.getText();
            print("\n single cmd {any}\n", .{cmd});
            parse_fbs.reset();
            try parse_utils.parseWrite(cmd, parse_writer);
            const cmd2: []const u8 = parse_fbs.getWritten();

            if (genericMatch(cmd2, &q_eq)) {
                return;
            } else if (genericMatch(cmd2, &j_eq)) {
                // next line
                dc.moveView(1);
            } else if (genericMatch(cmd2, &k_eq)) {
                // previous line
                dc.moveView(-1);
            } else if (genericMatch(cmd2, &c_arrows)) {
                // ctrl+arrow, move cursor
                //  move cursor
                dc.moveCursor(try selection_utils.matchDirSuffix(cmd2));
                // reset selection
                dc.cursor.selection = Selection.emptySel(dc.cursor.pos);
            } else if (genericMatch(cmd2, &sc_arrows)) {
                // shift+ctrl+arrow, move cursor & selection
                // move cursor
                dc.moveCursor(try selection_utils.matchDirSuffix(cmd2));
                // update selection
                dc.cursor.selection.head = dc.cursor.pos;
            } else if (genericMatch(cmd2, &paste_sel)) {
                try dc.pasteSelection();
            } else if (genericMatch(cmd2, &undo)) {
                dc.undo();
            } else if (genericMatch(cmd2, &c_fn_arrows)) {
                // ctrl+arrow, move cursor
                // move cursor
                const Case = enum { PGUP, PGDN, HOME, END };
                const case = std.meta.stringToEnum(Case, cmd2[5..]);
                if (case) |case_| {
                    switch (case_) {
                        .PGUP => dc.cursorPgUp(),
                        .PGDN => dc.cursorPgDn(),
                        .HOME => dc.cursorHome(),
                        .END => dc.cursorEnd(),
                    }
                }
            } else if (genericMatch(cmd2, &sc_fn_arrows)) {
                // shift+ctrl+arrow, move cursor & selection
                // move cursor
                const Case = enum { PGUP, PGDN, HOME, END };
                const case = std.meta.stringToEnum(Case, cmd2[11..]);
                if (case) |case_| {
                    switch (case_) {
                        .PGUP => dc.cursorPgUp(),
                        .PGDN => dc.cursorPgDn(),
                        .HOME => dc.cursorHome(),
                        .END => dc.cursorEnd(),
                    }
                    // update selection (TODO)!
                    dc.cursor.selection.head = dc.cursor.pos;
                }
            }
            else if (mode == .normal and genericMatch(cmd2, &insert_mode)) {
                mode = .insert;
            } else if (mode == .insert and genericMatch(cmd2, &normal_mode)) {
                mode = .normal;
            } else if (mode == .insert) {
                try dc.insertAtCursor(cmd2);
            }

            {
                const zone_print = tracy.initZone(@src(), .{ .name = "print" });
                defer zone_print.deinit();
                print("\nAfter handling commands: dc.vp: {}\n", .{ dc.render_buffer.viewport});
                print("Cursor: {}\n\n", .{dc.cursor});
            }
            //try dc.render_buffer.resize(vp);
            const txt = b: {
                const zone_txt = tracy.initZone(@src(), .{ .name = "Getting text" });
                defer zone_txt.deinit();
                break :b try dc.getText();
            };
            //try render(&.{}, dc.cursor, txt, vp);
            {
                const zone_rndr = tracy.initZone(@src(), .{ .name = "Rendering" });
                defer zone_rndr.deinit();
                try render(cmd, dc.cursor, txt, dc.render_buffer.viewport);
            }
        }
    }
}

/// render the current view
fn render(maybe_bytes: ?[]const u8, cursor: Cursor, lns: [] const doc.LineSlice, view: doc.ViewPort) !void {
    const tty_writer = tty.writer();
    var buf_writer = std.io.bufferedWriter(tty_writer);
    const writer = buf_writer.writer();

    try clear(writer);

    for (lns, 0..) |line, i| {
        try writeLine(writer, line.line, i);
    }
    try render_line_numbers(writer, view);

    //try renderLines(writer);
    try render_sel(writer, cursor, view, lns);
    try render_cursor(writer, cursor, view, lns);
    try render_bottom_ui(maybe_bytes, writer, cursor, view);
    _ = &maybe_bytes;

    try buf_writer.flush();
}

// render the line numbers
fn render_line_numbers(writer: anytype, view: doc.ViewPort) !void {
    const first_line = view.start.row;
    const last_line = view.start.row+view.height;
    //non_content_cols = 2 + misc_utils.numDigits(last_line); // TODO!!!
    for (first_line..last_line) |line_ind| {
        const pad_slice = misc_utils.spaces[0..(non_content_cols - misc_utils.numDigits(line_ind) - 1)];
        try moveCursor(writer, line_ind - first_line, 0);
        try writer.print("{s}{d}", .{ pad_slice, line_ind });
    }
    for (last_line..(view.start.row + view.height)) |line_ind| {
        const pad_slice = misc_utils.spaces[0..(non_content_cols - 2)];
        try moveCursor(writer, line_ind - first_line, 0);
        try writer.print("{s}~", .{pad_slice});
    }
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
        try writer.print("\x1B[46m", .{});
        try writer.writeAll("Raw input:       ");
        try parse_utils.rawWrite(bytes, writer);
        // parsed input row
        try moveCursor(writer, size.height - 1, 0);
        try writer.writeAll("Parsed input:    ");
        try parse_utils.parseWrite(bytes, writer);
        try writer.print("\x1B[49m", .{});
    } else {
        // no input
        try moveCursor(writer, size.height - 2, 0);
        try writer.writeAll("No input recieved!");
    }
    // status row
    try moveCursor(writer, size.height - 3, 0);
    try writer.print("\x1B[45m", .{});
    try writer.print("MODE: {s:>6}", .{@tagName(mode)});
    try writer.print("\x1B[47m", .{});
    try writer.print(" selection anchor {any: >3}   head {any: >3}", .{
        cursor.selection.anchor, cursor.selection.head });
    try moveCursor(writer, size.height - 4, 0);
    try writer.print("view {any: >3}   cursor {any: >3} (move using CTRL+<arrow>)", .{ view, cursor.pos });
    try writer.print("\x1B[49m", .{});
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
        if (rel_row_i >= lns.len) continue;
        const line: doc.LineSlice = lns[rel_row_i];

        // TODO the below is only correct for col==0
        std.debug.assert(view.start.col==0);
        for (0..line.line.len, line.line) |col_i, ch| {
            if (isBetween(min_p, .{.row=row_i, .col=col_i}, max_p)) {
                try moveCursor(writer, row_i - view.start.row, col_i+non_content_cols);
                // underline
                try writer.writeAll("\x1B[4m");
                // invert
                try writer.writeAll("\x1B[7m");
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
        if (row-view.start.row >= lns.len) return;
        try moveCursor(writer, row - view.start.row, col+non_content_cols);
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
    try moveCursor(writer, y, non_content_cols);
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
    //const content_rows = win_size.ws_row - non_content_rows;
    return Size{
        .height = win_size.ws_row,
        .width = win_size.ws_col,
    };
}

// this seems to ensure all tests are run
// when placed in the root file
test {
    std.testing.refAllDecls(@This());
    // or refAllDeclsRecursive
}
