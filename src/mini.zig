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
const misc_utils = @import("misc_utils.zig");
const selection_utils = @import("selection_utils.zig");
const logging = @import("logging.zig");

pub const std_options = logging.std_options;
pub const logger = logging.default_logger;

var file_content: [5 * 1024 * 1024]u8 = undefined;
var bytes_read: usize = 0;
pub var lines_read: usize = 0;
pub var lines: [5 * 1024 * 1024][]const u8 = undefined;
/// index of first visible line
// var first_line: usize = 0;
pub const bottom_ui_rows: usize = 4;
/// rows reserved for permanent ui
pub const non_content_rows = bottom_ui_rows;
/// rows available for file content
pub var content_rows: usize = undefined;
/// cols reserved for line number
pub var non_content_cols: usize = undefined;

/// window dimensions
var size: Size = undefined;
var tty: fs.File = undefined;

// åäö
// zig run src/mini.zig < src/parse_utils.zig &> mini.log
// zig run src/mini.zig -O ReleaseFast < src/parse_utils.zig &> mini.log
// https://ziglang.org/documentation/master/std/#std.posix.poll
// https://chatgpt.com/share/43543411-1296-4086-990d-0df98b621321

var parse_buf: [1000]u8 = undefined;
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

pub fn main() !void {
    tty = try fs.cwd().openFile("/dev/tty", .{ .mode = .read_write });
    defer tty.close();

    try term_utils.uncook(tty);
    defer term_utils.cook(tty) catch {};

    try logging.logger_init(null);
    defer logging.logger_deinit();

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
        var buffer: [1000]u8 = undefined;

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
                moveView(1);
            } else if (genericMatch(cmd2, &k_eq)) {
                // previous line
                moveView(-1);
            } else if (genericMatch(cmd2, &c_arrows)) {
                // ctrl+arrow, move cursor
                //  move cursor
                selection_utils.moveVCursorStep(try selection_utils.matchDirSuffix(cmd2));
                // reset selection
                selection_utils.cursor.selection = selection_utils.emptySel(selection_utils.cursor.pos);
            } else if (genericMatch(cmd2, &sc_arrows)) {
                // shift+ctrl+arrow, move cursor & selection
                // move cursor
                selection_utils.moveVCursorStep(try selection_utils.matchDirSuffix(cmd2));
                // update selection
                selection_utils.cursor.selection.head = selection_utils.cursor.pos;
            } else if (genericMatch(cmd2, &c_fn_arrows)) {
                // ctrl+arrow, move cursor
                // move cursor
                const Case = enum { PGUP, PGDN, HOME, END };
                const case = std.meta.stringToEnum(Case, cmd2[5..]);
                if (case) |case_| {
                    switch (case_) {
                        .PGUP => selection_utils.cursorPgUp(),
                        .PGDN => selection_utils.cursorPgDn(),
                        .HOME => selection_utils.cursorHome(),
                        .END => selection_utils.cursorEnd(),
                    }
                }
            } else if (genericMatch(cmd2, &sc_fn_arrows)) {
                // shift+ctrl+arrow, move cursor & selection
                // move cursor
                const Case = enum { PGUP, PGDN, HOME, END };
                const case = std.meta.stringToEnum(Case, cmd2[11..]);
                if (case) |case_| {
                    switch (case_) {
                        .PGUP => selection_utils.cursorPgUp(),
                        .PGDN => selection_utils.cursorPgDn(),
                        .HOME => selection_utils.cursorHome(),
                        .END => selection_utils.cursorEnd(),
                    }
                    // update selection
                    selection_utils.cursor.selection.head = selection_utils.cursor.pos;
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
    non_content_cols = 2 + misc_utils.numDigits(lines_read);

    try render_line_numbers(writer);
    try render_file_content(writer);
    try render_sel(writer);
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

// render the line numbers
fn render_line_numbers(writer: anytype) !void {
    const first_line = view.fst;
    const last_line = view.lst;
    for (first_line..last_line) |line_ind| {
        const pad_slice = misc_utils.spaces[0..(non_content_cols - misc_utils.numDigits(line_ind) - 1)];
        try moveCursor(writer, line_ind - first_line, 0);
        try writer.print("{s}{d}", .{ pad_slice, line_ind });
    }
    for (last_line..(view.fst + content_rows)) |line_ind| {
        const pad_slice = misc_utils.spaces[0..(non_content_cols - 2)];
        try moveCursor(writer, line_ind - first_line, 0);
        try writer.print("{s}~", .{pad_slice});
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
    try writer.print("{any}", .{selection_utils.cursor.selection});
    try moveCursor(writer, size.height - 4, 0);
    try writer.print("view {any}" ++ " " ** 40 ++ "cursor {any} (move using CTRL+<arrow>)", .{ view, selection_utils.cursor.pos });
}

fn render_sel(writer: anytype) !void {
    const min_p, const max_p = getSortedPoints(selection_utils.cursor.selection.anchor, selection_utils.cursor.selection.head);
    const min_r = min_p.row;
    const max_r = max_p.row;
    // var max_c: usize = undefined;
    // for (min_r..(max_r + 1)) |row_i| max_c = @max(max_c, lines[row_i].len);
    // if (max_r <= min_r) return;
    for (min_r..(max_r + 1)) |row_i| {
        if (row_i < view.fst or view.lst <= row_i) continue;
        const line = lines[row_i];
        for (0..line.len, line) |col_i, ch| {
            if (isBetween(min_p, point(row_i, col_i), max_p)) {
                try moveCursor(writer, row_i - view.fst, col_i + non_content_cols);
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

fn render_cursor(writer: anytype) !void {
    const row = selection_utils.cursor.pos.row;
    const col = selection_utils.cursor.pos.col;
    // if selection_utils.cursor.pos
    if (view.fst <= row and row < view.lst) {
        try moveCursor(writer, row - view.fst, col + non_content_cols);
        // blink
        try writer.writeAll("\x1B[5m");
        // reverse fg/bg
        try writer.writeAll("\x1B[7m");
        // if part of selection, underline
        if (isBetween(selection_utils.cursor.selection.anchor, selection_utils.cursor.pos, selection_utils.cursor.selection.head) or isBetween(selection_utils.cursor.selection.head, selection_utils.cursor.pos, selection_utils.cursor.selection.anchor)) {
            // underline
            try writer.writeAll("\x1B[4m");
        }
        var byte: u8 = ' ';
        if (col < lines[row].len) byte = lines[row][col];
        if (byte < 32 or byte > 126) byte = ' ';
        try writer.writeByte(byte);
        try writer.writeAll("\x1B[0m");
    }
}

/// write slice to the buffer at row y col 0, applying format.myFmtLine
fn writeLine(writer: anytype, txt: []const u8, y: usize) !void {
    try moveCursor(writer, y, non_content_cols);
    try writer.print("{}", .{format.myFmtLine(txt)});
}

/// move the selection_utils.cursor to row, col, using 0-indexing
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
pub const View = struct {
    fst: usize = 0,
    lst: usize = 0,
    pub fn format(
        self: *const @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("view ( {d: >4}, {d: >4} )", .{ self.fst, self.lst });
    }
};
pub var view: View = .{};
pub fn moveView(ind: isize) void {
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
pub const Point = struct {
    row: usize,
    col: usize,
    pub fn format(
        self: *const @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("( {d: >4}, {d: >4} )", .{ self.row, self.col });
    }
};
pub fn point(row: usize, col: usize) Point {
    return .{ .row = row, .col = col };
}

// this seems to ensure all tests are run
// when placed in the root file
test {
    std.testing.refAllDecls(@This());
    // or refAllDeclsRecursive
}
