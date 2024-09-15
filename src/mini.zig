const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const os = std.os;
const linux = std.os.linux;
const posix = std.posix;

const format = @import("format.zig");
const term_utils = @import("term_utils.zig");
const parse_utils = @import("parse_utils.zig");
const write_utils = @import("write_utils.zig");

var file_content: [5 * 1024 * 1024]u8 = undefined;
var bytes_read: usize = 0;
var lines_read: usize = 0;
var lines: [5 * 1024 * 1024][]const u8 = undefined;
/// index of first visible line
var first_line: usize = 0;
const bottom_ui_rows: usize = 2;
/// rows reserved for permanent ui
const non_content_rows = bottom_ui_rows;
/// rows available for file content
var content_rows: usize = undefined;

/// window dimensions
var size: Size = undefined;
var tty: fs.File = undefined;

// åäö
// zig run src/mini.zig < src/parse_utils.zig &> mini.log
// https://ziglang.org/documentation/master/std/#std.posix.poll
// https://chatgpt.com/share/43543411-1296-4086-990d-0df98b621321

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

    try render(null);

    while (true) {
        // var buffer: [1]u8 = undefined;
        // _ = try tty.read(&buffer);

        _ = try posix.poll(&fds, -1);
        var buffer: [10]u8 = .{255} ** 10;
        const num_read = try tty.read(&buffer);
        if (num_read == 0) continue;

        if (num_read == 0) unreachable;
        // if (num_read > 1) {
        //     // TODO return error so that deferred uncooking may work
        //     //unreachable;
        //     return;
        // }
        // try parse_utils.parse(buffer[0..num_read]);

        if (buffer[0] == 'q') {
            return;
        } else if (buffer[0] == 'j') {
            // next line
            if (lines_read + 20 > size.height and first_line < lines_read - 20) {
                first_line += 1;
            }
        } else if (buffer[0] == 'k') {
            // previous line
            if (first_line > 0) {
                first_line -= 1;
            }
        }
        try render(buffer[0..num_read]);
    }
}

/// render the current view
fn render(maybe_bytes: ?[]const u8) !void {
    const tty_writer = tty.writer();
    var buf_writer = std.io.bufferedWriter(tty_writer);
    const writer = buf_writer.writer();

    try clear(writer);

    try render_file_content(writer);
    try render_bottom_ui(maybe_bytes, writer);

    try buf_writer.flush();
}

/// render file content
fn render_file_content(writer: anytype) !void {
    const last_line: usize = @min(first_line + content_rows, lines_read);
    for (first_line..last_line) |line_ind| {
        try writeLine(writer, lines[line_ind], line_ind - first_line);
    }
}

/// render bottom ui
fn render_bottom_ui(maybe_bytes: ?[]const u8, writer: anytype) !void {
    const multi_writer = write_utils.multiWriter(writer);
    if (maybe_bytes) |bytes| {
        try moveCursor(writer, size.height - 2, 0);
        try parse_utils.rawWrite(bytes, multi_writer);
        try moveCursor(writer, size.height - 1, 0);
        try parse_utils.parseWrite(bytes, multi_writer);
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
