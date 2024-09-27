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

var file_content: [5 * 1024 * 1024]u8 = undefined;
var bytes_read: usize = 0;
var lines_read: usize = 0;
var lines: [5 * 1024 * 1024][]const u8 = undefined;
/// index of first visible line
// var first_line: usize = 0;
const bottom_ui_rows: usize = 3;
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
// try writer.print("bla", .{});
// print("{any}\n", .{fbs.getWritten()[3..]});
// fbs.reset();
// print("{s}\n", .{@typeName(@TypeOf(fbs.getWritten()))});

/// tries to match the slice needle to a slice in haystack.
fn genericMatch(needle: []const u8, haystack: []const []const u8) bool {
    for (haystack) |straw| {
        if (std.mem.eql(u8, needle, straw)) {
            return true;
        }
    }
    return false;
}

/// quit commands
const q_eq: [3][]const u8 = .{ "ESC", "Q", "q" };
/// down commands
const j_eq: [3][]const u8 = .{ "DOWN", "J", "j" };
/// up commands
const k_eq: [3][]const u8 = .{ "UP", "K", "k" };

/// cursor up
const c_arrows: [4][]const u8 = .{ "CTRL+UP", "CTRL+DOWN", "CTRL+LEFT", "CTRL+RIGHT" };

pub fn main() !void {
    tty = try fs.cwd().openFile("/dev/tty", .{ .mode = .read_write });
    defer tty.close();

    try term_utils.uncook(tty);
    defer term_utils.cook(tty) catch {};

    try getInp();

    size = try getSize();

    var fds: [1]posix.pollfd = .{.{
        .fd = tty.handle,
        .events = posix.POLL.IN,
        .revents = undefined,
    }};

    // set last visible line
    moveView(0);

    try render(null);

    while (true) {
        // var buffer: [1]u8 = undefined;
        // _ = try tty.read(&buffer);

        _ = try posix.poll(&fds, -1);
        var buffer: [1000]u8 = .{255} ** 1000;

        const num_read = try tty.read(&buffer);
        if (num_read == 0) continue;

        print("\nall bytes {any}\n", .{buffer[0..num_read]});
        var cmd_it = parse_utils.InputSeqIterator{ .bytes = buffer[0..num_read] };

        while (try cmd_it.next()) |cmd| {
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
                moveView(1);
            } else if (genericMatch(cmd2, &k_eq)) {
                // previous line
                // if (first_line > 0) {
                //     first_line -= 1;
                // }
                moveView(-1);
            } else if (genericMatch(cmd2, &c_arrows)) {
                // ctrl+arrow, move cursor
                if (std.mem.eql(u8, cmd2, c_arrows[0])) {
                    // up
                    moveVCursorRel(-1, 0);
                } else if (std.mem.eql(u8, cmd2, c_arrows[1])) {
                    // down
                    moveVCursorRel(1, 0);
                } else if (std.mem.eql(u8, cmd2, c_arrows[2])) {
                    // left
                    moveVCursorRel(0, -1);
                } else if (std.mem.eql(u8, cmd2, c_arrows[3])) {
                    // right
                    moveVCursorRel(0, 1);
                }
            }
            try render(cmd);
        }
    }
}

/// render the current view
fn render(maybe_bytes: ?[]const u8) !void {
    const tty_writer = tty.writer();
    var buf_writer = std.io.bufferedWriter(tty_writer);
    const writer = buf_writer.writer();

    try clear(writer);

    try render_file_content(writer);
    try render_cursor(writer);
    try render_bottom_ui(maybe_bytes, writer);
    _ = &maybe_bytes;

    try buf_writer.flush();
}

/// render file content
fn render_file_content(writer: anytype) !void {
    // const last_line: usize = @min(first_line + content_rows, lines_read);
    const first_line = view.fst;
    const last_line = view.lst;
    for (first_line..last_line) |line_ind| {
        try writeLine(writer, lines[line_ind], line_ind - first_line);
    }
}

/// render bottom ui
fn render_bottom_ui(maybe_bytes: ?[]const u8, arg_writer: anytype) !void {
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
    try writer.print("view {any}       cursor {any} (move using CTRL+<arrow>)", .{ view, cursor.pos });
}

fn render_cursor(writer: anytype) !void {
    const row = cursor.pos.row;
    const col = cursor.pos.col;
    // if cursor.pos
    if (view.fst <= row and row < view.lst) {
        try moveCursor(writer, row - view.fst, col);
        // // white bg
        // try writer.writeAll("\x1B[47m");
        // reverse fg/bg
        try writer.writeAll("\x1B[7m");
        // blink
        try writer.writeAll("\x1B[5m");
        // try writer.writeAll(" ");
        var byte = lines[row][col];
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

/// read file content from stdin.
/// raise exception if longer than 5 MB.
fn getInp() !void {
    const stdin = std.io.getStdIn().reader();
    bytes_read = try stdin.readAll(&file_content);
    if (bytes_read == file_content.len) @panic("getInp file too long");
    var split_it = std.mem.splitSequence(u8, file_content[0..bytes_read], "\n");
    while (split_it.next()) |line| {
        lines[lines_read] = line;
        lines_read += 1;
    }
}
/// first line, last line. lst not included
const View = struct {
    fst: usize = 0,
    lst: usize = 0,
};
var view: View = .{};
fn moveView(ind: isize) void {
    if (ind > 0) {
        view.fst += @min(@abs(ind), lines_read - view.fst - @min(10, lines_read));
    } else if (ind < 0) {
        // const abs: usize = ind;
        view.fst -= @min(@abs(ind), view.fst);
    }
    view.lst = @min(lines_read, view.fst + content_rows);
    if (view.lst <= view.fst or view.lst > lines_read + 1) panic("moveView caused invalid view {any}\n", .{view});
}

// 0-indexing
const Point = struct {
    row: usize,
    col: usize,
};
fn point(row: usize, col: usize) Point {
    return .{ .row = row, .col = col };
}

/// a point, and a selection
const Cursor = struct {
    pos: Point,
    target_col: usize = 0,
    selection: ?Selection = null,
};
const def_pos = point(0, 0);
var cursor: Cursor = .{ .pos = def_pos, .selection = emptySel(def_pos) };

/// move the the cursor
fn moveVCursorRel(rows: isize, cols: isize) void {
    var crow = cursor.pos.row;
    var ccol = cursor.pos.col;
    if (rows < 0) {
        crow -= @min(crow, @abs(rows));
    } else if (rows > 0) {
        crow += @min(lines_read - crow, @abs(rows));
    }
    const clen = lines[crow].len;
    if (cols == 0) {
        ccol = @min(clen, cursor.target_col);
    } else if (cols > 0) {
        ccol += @min(clen - ccol, @abs(cols));
        cursor.target_col = ccol;
    } else if (cols < 0) {
        ccol -= @min(ccol, @abs(cols));
        cursor.target_col = ccol;
    }
    cursor.pos = point(crow, ccol);

    // make sure the cursor is visible after being moved
    if (crow < view.fst) moveView(-@as(isize, @intCast(view.fst - crow)));
    if (crow >= view.lst) moveView(@intCast(view.lst + 1 - crow));
}

/// try to move the cursor to target point
fn setVCursorPos(target: Point) void {
    var trow = target.row;
    var tcol = target.col;
    if (trow > lines_read) trow = lines_read;
    const tlen = lines[trow].len;
    if (tcol > tlen) tcol = tlen;
    cursor.pos = point(trow, tcol);
    cursor.target_col = tcol;
}

/// movable head, immovable anchor.
/// regular cursor/empty selection: head = anchor
const Selection = struct {
    head: Point,
    anchor: Point,
};
fn emptySel(pos: Point) Selection {
    return .{ .head = pos, .anchor = pos };
}

// this seems to ensure all tests are run
// when placed in the root file
test {
    std.testing.refAllDecls(@This());
    // or refAllDeclsRecursive
}
