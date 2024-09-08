const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const os = std.os;
const linux = std.os.linux;

const format = @import("format.zig");
const term_utils = @import("term_utils.zig");

var file_content: [5 * 1024 * 1024]u8 = undefined;
var bytes_read: usize = 0;
var lines_read: usize = 0;
var lines: [5 * 1024 * 1024][]const u8 = undefined;
/// index of first visible line
var first_line: usize = 0;

/// window dimensions
var size: Size = undefined;
var tty: fs.File = undefined;

// https://ziglang.org/documentation/master/std/#std.posix.poll
// https://chatgpt.com/share/43543411-1296-4086-990d-0df98b621321

pub fn main() !void {
    tty = try fs.cwd().openFile("/dev/tty", .{ .mode = .read_write });
    defer tty.close();

    try term_utils.uncook(tty);
    defer term_utils.cook(tty) catch {};

    try getInp();

    size = try getSize();

    try render();

    while (true) {
        var buffer: [1]u8 = undefined;
        _ = try tty.read(&buffer);

        if (buffer[0] == 'q') {
            return;
        } else if (buffer[0] == 'j') {
            // next line
            if (lines_read + 20 > size.height and first_line < lines_read - 20) {
                first_line += 1;
                try render();
            }
        } else if (buffer[0] == 'k') {
            // previous line
            if (first_line > 0) {
                first_line -= 1;
                try render();
            }
        } else std.time.sleep(100); // nanoseconds!!
    }
}

/// render the current view
fn render() !void {
    const writer = tty.writer();
    try clear(writer);
    const last_line: usize = @min(first_line + size.height, lines_read);
    for (first_line..last_line) |lineInd| {
        try writeLine(writer, lines[lineInd], lineInd - first_line);
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
