const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const os = std.os;
const linux = std.os.linux;
const posix = std.posix;

const treez = @import("treez");

const format = @import("format.zig");
const term_utils = @import("term_utils.zig");
const pu = @import("parse_utils.zig");
const wu = @import("write_utils.zig");
const mu = @import("misc_utils.zig");
const su = @import("selection_utils.zig");
const logging = @import("logging.zig");

// function imports
pub const std_options = logging.std_options;
pub const logger = logging.default_logger;
pub const panic = logging.panic;
pub const panicFmt = logging.panicFmt;
const isz = mu.isz;

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

/// minimum lines visible when scrolling past end
const MIN_LINES_VISIBLE: usize = 1;

// åäö
// zig run src/mini.zig < src/parse_utils.zig &> mini.log
// zig run src/mini.zig -O ReleaseFast < src/parse_utils.zig &> mini.log
// https://ziglang.org/documentation/master/std/#std.posix.poll
// https://chatgpt.com/share/43543411-1296-4086-990d-0df98b621321

var parse_buf: [1000]u8 = undefined;
var parse_fbs = std.io.fixedBufferStream(&parse_buf);
const parse_writer = parse_fbs.writer();

/// tries to match the slice needle to a slice in haystack.
fn sliceMatch(needle: []const u8, haystack: []const []const u8) bool {
    for (haystack) |straw| {
        if (std.mem.eql(u8, needle, straw)) {
            return true;
        }
    }
    return false;
}

pub fn main() !void {
    const ziglang = try treez.Language.get("zig");

    var parser = try treez.Parser.create();
    defer parser.destroy();

    try parser.setLanguage(ziglang);
    // parser.useStandardLogger();

    const inp = @embedFile("mini.zig");
    const tree = try parser.parseString(null, inp);
    defer tree.destroy();

    tty = try fs.cwd().openFile("/dev/tty", .{ .mode = .read_write });
    defer tty.close();

    try term_utils.uncook(tty);
    defer term_utils.cook(tty) catch {};

    try logging.loggerInit(null);
    defer logging.loggerDeinit();

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
        if (num_read == buffer.len) {
            logger.err("out of memory reading at least {any} bytes from tty!", .{num_read});
            return error.OutOfMemory;
        }

        logger.debug("number of bytes read {any}", .{num_read});

        logger.debug("all bytes {any}", .{buffer[0..num_read]});
        var cmd_it = pu.InputSeqIterator{ .bytes = buffer[0..num_read] };

        while (try cmd_it.next()) |cmd| {
            logger.debug("single cmd {any}", .{cmd});
            parse_fbs.reset();
            try pu.parseWrite(cmd, parse_writer);
            const cmd2: []const u8 = parse_fbs.getWritten();
            blk: {
                var move_view = false;
                if (sliceMatch(cmd2, &su.q_eq)) {
                    logger.debug("input case q, quit", .{});
                    return;
                } else if (sliceMatch(cmd2, &su.j_eq)) {
                    logger.debug("input case j, move view", .{});
                    // next line
                    moveView(1);
                } else if (sliceMatch(cmd2, &su.k_eq)) {
                    logger.debug("input case k, move view", .{});
                    // previous line
                    moveView(-1);
                } else if (sliceMatch(cmd2, &su.c_arrows)) {
                    logger.debug("input case ctrl+arrow, move cursor", .{});
                    // ctrl+arrow, move cursor
                    move_view = true;
                    su.movePt(su.matchDirSuffix(cmd2) catch break :blk, false, false, &su.cursor);
                } else if (sliceMatch(cmd2, &su.sc_arrows)) {
                    logger.debug("input case shift+ctrl+arrow, move cursor & selection", .{});
                    // shift+ctrl+arrow, move cursor & selection
                    move_view = true;
                    su.movePt(su.matchDirSuffix(cmd2) catch break :blk, true, false, &su.cursor);
                } else if (sliceMatch(cmd2, &su.fn_arrows)) {
                    logger.debug("input case fn+arrow, move view", .{});
                    // fn+arrow, move view
                    switch (su.matchDirSuffix(cmd2) catch break :blk) {
                        su.Direction.up => moveView(isz(content_rows - 1)),
                        su.Direction.down => moveView(isz(content_rows - 1)),
                        else => {},
                    }
                } else if (sliceMatch(cmd2, &su.c_fn_arrows)) {
                    logger.debug("input case ctrl+fn+arrow, move cursor", .{});
                    // ctrl+fn+arrow, move cursor
                    move_view = true;
                    su.movePt(su.matchDirSuffix(cmd2) catch break :blk, false, true, &su.cursor);
                } else if (sliceMatch(cmd2, &su.sc_fn_arrows)) {
                    logger.debug("input case shift+ctrl+fn+arrow, move cursor & selection", .{});
                    // shift+ctrl+fn+arrow, move cursor & selection
                    move_view = true;
                    su.movePt(su.matchDirSuffix(cmd2) catch break :blk, true, true, &su.cursor);
                }
                if (move_view) {
                    const t_row = su.cursor.pos.row;
                    if (t_row >= view.lst) moveView(isz(t_row - view.lst) + 1) else if (t_row < view.fst) moveView(isz(t_row) - isz(view.fst));
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
    non_content_cols = 2 + mu.numDigits(lines_read);

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
        const pad_slice = mu.spaces[0..(non_content_cols - mu.numDigits(line_ind) - 1)];
        try moveCursor(writer, line_ind - first_line, 0);
        try writer.print("{s}{d}", .{ pad_slice, line_ind });
    }
    for (last_line..(view.fst + content_rows)) |line_ind| {
        const pad_slice = mu.spaces[0..(non_content_cols - 2)];
        try moveCursor(writer, line_ind - first_line, 0);
        try writer.print("{s}~", .{pad_slice});
    }
}

/// render bottom ui
fn render_bottom_ui(maybe_bytes: ?[]const u8, arg_writer: anytype) !void {
    // var rawbuf
    var multi_writer = wu.multiWriter(arg_writer);
    const writer = multi_writer.writer();
    // input?
    if (maybe_bytes) |bytes| {
        // input given
        // raw input row
        try moveCursor(writer, size.height - 2, 0);
        try writer.writeAll("Raw input:       ");
        try pu.rawWrite(bytes, writer);
        // parsed input row
        try moveCursor(writer, size.height - 1, 0);
        try writer.writeAll("Parsed input:    ");
        try pu.parseWrite(bytes, writer);
    } else {
        // no input
        try moveCursor(writer, size.height - 2, 0);
        try writer.writeAll("No input recieved!");
    }
    // status row
    try moveCursor(writer, size.height - 3, 0);
    try writer.print("{any}", .{su.cursor.sel});
    try moveCursor(writer, size.height - 4, 0);
    try writer.print("view {any}" ++ " " ** 40 ++ "cursor {any} (move using CTRL+<arrow>)", .{ view, su.cursor.pos });
}

fn render_sel(writer: anytype) !void {
    // logger.debug("render_sel selection {any}", .{su.cursor.sel});
    // logger.debug("render_sel view.fst {} view.lst {}", .{ view.fst, view.lst });
    var min_p, var max_p = su.getSortedPoints(su.cursor.sel.anchor, su.cursor.sel.head);
    // logger.debug("min_p {any} max_p {any}", .{ min_p, max_p });
    // limit ourselves to the visible part of the selection
    min_p = su.maxPt(min_p, su.point(view.fst, 0));
    // limit ourselves to the visible part of the selection
    max_p = su.minPt(max_p, su.point(view.lst, 0));
    // logger.debug("min_p {any} max_p {any}", .{ min_p, max_p });
    const min_r = min_p.row;
    const max_r = max_p.row;
    // var max_c: usize = undefined;
    // for (min_r..(max_r + 1)) |row_i| max_c = @max(max_c, lines[row_i].len);
    // if (max_r <= min_r) return;
    // logger.debug("min_r {any} max_r {any}", .{ min_r, max_r });
    if (min_r <= max_r) {
        for (min_r..(max_r + 1)) |row_i| {
            if (row_i < view.fst or view.lst <= row_i) continue;
            const line = lines[row_i];
            for (0..line.len, line) |col_i, ch| {
                if (su.isBetween(min_p, su.point(row_i, col_i), max_p)) {
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

    // logger.debug("rendel_sel end", .{});
}

fn render_cursor(writer: anytype) !void {
    const row = su.cursor.pos.row;
    const col = su.cursor.pos.col;
    // if su.cursor.pos
    if (view.fst <= row and row < view.lst) {
        try moveCursor(writer, row - view.fst, col + non_content_cols);
        // blink
        try writer.writeAll("\x1B[5m");
        // reverse fg/bg
        try writer.writeAll("\x1B[7m");
        // if part of selection, underline
        if (su.isBetween(su.cursor.sel.anchor, su.cursor.pos, su.cursor.sel.head) or su.isBetween(su.cursor.sel.head, su.cursor.pos, su.cursor.sel.anchor)) {
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

/// move the su.cursor to row, col, using 0-indexing
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
        panicFmt("getsize failed ioctl()", .{});
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
    if (bytes_read == file_content.len) panicFmt("getInp file too long, at least {} bytes", .{bytes_read});
    var split_it = std.mem.splitSequence(u8, file_content[0..bytes_read], "\n");
    while (split_it.next()) |line| {
        lines[lines_read] = line;
        lines_read += 1;
    }
}
/// first line, last line. lst not visible
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
/// lst is not invisible
pub var view: View = .{};
/// can be called with any valid isize
pub fn moveView(ind: isize) void {
    logger.debug("moveView(ind: {any})", .{ind});
    if (ind > 0) {
        view.fst += @min(@abs(ind), lines_read - view.fst - @min(MIN_LINES_VISIBLE, lines_read));
    } else if (ind < 0) {
        // const abs: usize = ind;
        view.fst -= @min(@abs(ind), view.fst);
    }
    view.lst = @min(lines_read, view.fst + content_rows);
    if (view.lst <= view.fst or view.lst > lines_read) panicFmt("moveView caused invalid view {any}\n", .{view});
}

// this seems to ensure all tests are run
// when placed in the root file
test {
    std.testing.refAllDecls(@This());
    // or refAllDeclsRecursive
}
