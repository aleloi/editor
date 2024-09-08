const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const os = std.os;
const linux = std.os.linux;

const format = @import("format.zig");

var fileContent: [5 * 1024 * 1024]u8 = undefined;
var bytesRead: usize = 0;
var linesRead: usize = 0;
var lines: [5 * 1024 * 1024][]u8 = undefined;
var firstLine: usize = 0;

var size: Size = undefined;
var cooked_termios: linux.termios = undefined;
var raw: linux.termios = undefined;
var tty: fs.File = undefined;

pub fn main() !void {
    tty = try fs.cwd().openFile("/dev/tty", .{ .mode = .read_write });
    defer tty.close();

    try uncook();
    defer cook() catch {};

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
            if (linesRead + 20 > size.height and firstLine < linesRead - 20) {
                firstLine += 1;
                try render();
            }
        } else if (buffer[0] == 'k') {
            // previous line
            if (firstLine > 0) {
                firstLine -= 1;
                try render();
            }
        }
    }
}

fn render() !void {
    const writer = tty.writer();
    try clear(writer);
    const lastLine: usize = @min(firstLine + size.height, linesRead);
    for (firstLine..lastLine) |lineInd| {
        try writeLine(writer, lines[lineInd], lineInd - firstLine);
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
    errdefer cook() catch {};

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
    try hideCursor(writer);
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
        if (stdin.readUntilDelimiter(fileContent[bytesRead..], '\n')) |line| {
            bytesRead += line.len;
            fileContent[bytesRead] = '\n';
            bytesRead += 1;
            lines[linesRead] = line;
            linesRead += 1;
        } else |err| switch (err) {
            error.EndOfStream => return,
            else => return err,
        }
    }
}
