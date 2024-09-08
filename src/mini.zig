// test swedish: åäö unicode 🔥🐍
const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const os = std.os;
const linux = std.os.linux;
const posix = std.posix;

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
var cooked_termios: linux.termios = undefined;
var raw: linux.termios = undefined;
var tty: fs.File = undefined;

// https://ziglang.org/documentation/master/std/#std.posix.poll
// https://chatgpt.com/share/43543411-1296-4086-990d-0df98b621321
const Mode = enum {
    INSERT,
    NORMAL
};

// var cursorLine: usize = 0;
// var cursorCol: usize = 0;


var mode: Mode = Mode.NORMAL;

pub fn main() !void {
    tty = try fs.cwd().openFile("/dev/tty", .{ .mode = .read_write });
    defer tty.close();

    var fds: [1]posix.pollfd = .{
        .{
            .fd = tty.handle,
            .events = posix.POLL.IN,
            .revents = undefined,
        }
    };

    try uncook();
    defer cook() catch unreachable;

    try getInp();

    size = try getSize();

    try render();

    while (true) {
        _ = try posix.poll(&fds, -1);
        var buffer: [10]u8 = .{255}**10;
        const num_read = try tty.read(&buffer);
        std.debug.print("Read {} chars; buffer is: {s}\n", .{num_read, buffer[0..num_read]});
        if (num_read == 0) unreachable;
        if (num_read > 1) {
            // TODO return error so that deferred uncooking may work
            //unreachable;
            continue;
        }

        if (mode == Mode.NORMAL) {
            switch (buffer[0]) {
                'q' => {return; },
                'j' => {
                    std.debug.print("[j] lines_read: {}, first_line: {}, size.height: {}\n", .{lines_read, first_line, size.height});
                    if (lines_read + 20 > size.height and first_line < lines_read - 20) {
                        first_line += 1;
                        try render();
                    }
                },
                'k' => {
                    std.debug.print("[k] lines_read: {}, first_line: {}, size.height: {}\n", .{lines_read, first_line, size.height});
                    // previous line
                    if (first_line > 0) {
                        first_line -= 1;
                        try render();
                    }
                },
                'i' => {
                    mode = Mode.INSERT;
                },
                255 => unreachable,
                else => {
                    std.debug.print("buffer[0] is: {}\n", .{buffer[0]});

                }
            }
        }

        else if (mode == Mode.INSERT) {
            switch (buffer[0]) {
                // printable lower half ASCII chars, SPACE in 32 and TILDE is 126
                ' ' ... '~' => {
                    //insertIntoBufferAtCursorPosition(buffer[0])
                    try insertToTTY(&buffer);
                },
                '\x1B' => {
                    mode = Mode.NORMAL;
                },
                else => {
                    std.debug.print("Wants to insert: {}\n", .{buffer[0]});

                }
            }
        }
    }
}

fn insertToTTY(writableBytes: []const u8) !void {
    const writer = tty.writer();
    const num_written = try writer.write(writableBytes);
    std.debug.assert(num_written == writableBytes.len);
}

fn render() !void {
    const writer = tty.writer();
    try clear(writer);
    const last_line: usize = @min(first_line + size.height, lines_read);
    for (first_line..last_line) |line_ind| {
        try writeLine(writer, lines[line_ind], line_ind - first_line);
    }
}

fn writeLine(writer: anytype, txt: []const u8, y: usize) !void {
    try moveCursor(writer, y, 0);
    try writer.print("{}", .{format.myFmtLine(txt)});
}

fn uncook() !void {
    const writer = tty.writer();
    if (linux.tcgetattr(tty.handle, &cooked_termios) != 0) {
        @panic("failed tcgetattr()");
    }
    errdefer cook() catch unreachable;

    raw = cooked_termios;
    raw.lflag.ECHO = false;
    raw.lflag.ICANON = false;
    raw.lflag.ISIG = false;
    raw.lflag.IEXTEN = false;
    raw.iflag.IXON = false;
    raw.iflag.ICRNL = false;
    raw.iflag.BRKINT = false;
    raw.iflag.INPCK = false;
    raw.iflag.ISTRIP = false;
    raw.oflag.OPOST = false;
    raw.cflag.CSIZE = .CS8;
    raw.cc[@intFromEnum(linux.V.MIN)] = 0;
    raw.cc[@intFromEnum(linux.V.TIME)] = 0;
    if (linux.tcsetattr(tty.handle, .FLUSH, &raw) != 0) {
        @panic("uncook failed tcsetattr()");
    }
    //try hideCursor(writer);
    try enterAlt(writer);
    try clear(writer);
}

fn cook() !void {
    const writer = tty.writer();
    try clear(writer);
    try leaveAlt(writer);
    try showCursor(writer);
    try attributeReset(writer);
    if (linux.tcsetattr(tty.handle, .FLUSH, &cooked_termios) != 0) {
        @panic("cook failed tcsetattr()");
    }
}

fn moveCursor(writer: anytype, row: usize, col: usize) !void {
    _ = try writer.print("\x1B[{};{}H", .{ row + 1, col + 1 });
}

fn enterAlt(writer: anytype) !void {
    try writer.writeAll("\x1B[s"); // Save cursor position.
    try writer.writeAll("\x1B[?47h"); // Save screen.
    try writer.writeAll("\x1B[?1049h"); // Enable alternative buffer.
}

fn leaveAlt(writer: anytype) !void {
    try writer.writeAll("\x1B[?1049l"); // Disable alternative buffer.
    try writer.writeAll("\x1B[?47l"); // Restore screen.
    try writer.writeAll("\x1B[u"); // Restore cursor position.
}

fn hideCursor(writer: anytype) !void {
    try writer.writeAll("\x1B[?25l");
}

fn showCursor(writer: anytype) !void {
    try writer.writeAll("\x1B[?25h");
}

fn attributeReset(writer: anytype) !void {
    try writer.writeAll("\x1B[0m");
}

fn clear(writer: anytype) !void {
    try writer.writeAll("\x1B[2J");
}

const Size = struct { width: usize, height: usize };

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

fn getInp() !void {
    const stdin = std.io.getStdIn().reader();
    while (true) {
        if (stdin.readUntilDelimiter(file_content[bytes_read..], '\n')) |line| {
            bytes_read += line.len;
            file_content[bytes_read] = '\n';
            bytes_read += 1;
            lines[lines_read] = line;
            lines_read += 1;
        } else |err| switch (err) {
            error.EndOfStream => return,
            else => return err,
        }
    }
}
