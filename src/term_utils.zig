//! terminal utils, mainly entering/exiting alternative buffer and mode
const std = @import("std");
const linux = std.os.linux;

const logging = @import("logging.zig");

/// Global I/O context, set by main() from init.io.
/// Used for all file operations (open, read, write, close).
pub var app_io: std.Io = undefined;

/// the original settings
var orig_termios: linux.termios = undefined;
/// our alternative settings
var alt_termios: linux.termios = undefined;

pub var tty: std.Io.File = undefined;
pub var tty_writer: std.Io.File.Writer = undefined;
var tty_writer_buf: [4096]u8 = undefined;

/// enter alternative buffer, saving current state
pub fn uncook() !void {
    tty = try std.Io.Dir.cwd().openFile(app_io, "/dev/tty", .{ .mode = .read_write });
    tty_writer = std.Io.File.Writer.init(tty, app_io, &tty_writer_buf);

    const writer = &tty_writer.interface;
    if (linux.tcgetattr(tty.handle, &orig_termios) != 0) {
        @panic("failed tcgetattr()");
    }
    errdefer cook() catch {};

    alt_termios = orig_termios;
    alt_termios.lflag.ECHO = false;
    alt_termios.lflag.ICANON = false;
    alt_termios.lflag.ISIG = false;
    alt_termios.lflag.IEXTEN = false;
    alt_termios.iflag.IXON = false;
    alt_termios.iflag.ICRNL = false;
    alt_termios.iflag.BRKINT = false;
    alt_termios.iflag.INPCK = false;
    alt_termios.iflag.ISTRIP = false;
    alt_termios.oflag.OPOST = false;
    alt_termios.cflag.CSIZE = .CS8;
    alt_termios.cc[@intFromEnum(linux.V.MIN)] = 0;
    alt_termios.cc[@intFromEnum(linux.V.TIME)] = 0;
    if (linux.tcsetattr(tty.handle, .FLUSH, &alt_termios) != 0) {
        @panic("uncook failed tcsetattr()");
    }
    try writer.writeAll("\x1B[?25l"); // Hide cursor.
    try writer.writeAll("\x1B[s"); // Save cursor position.
    try writer.writeAll("\x1B[?47h"); // Save screen.
    try writer.writeAll("\x1B[?1049h"); // Enable alternative buffer.
    try writer.writeAll("\x1B[2J"); // Clear buffer.
    try writer.writeAll("\x1B[0m"); // Attribute reset.
    try writer.flush();
}

/// exit alternative buffer, restore previous state
pub fn cook() !void {
    const writer = &tty_writer.interface;
    try writer.writeAll("\x1B[2J"); // Clear buffer.
    try writer.writeAll("\x1B[?1049l"); // Disable alternative buffer.
    try writer.writeAll("\x1B[?47l"); // Restore screen.
    try writer.writeAll("\x1B[u"); // Restore cursor position.
    try writer.writeAll("\x1B[?25h"); // Show cursor.
    try writer.writeAll("\x1B[0m"); // Attribute reset.
    try writer.flush();
    if (linux.tcsetattr(tty.handle, .FLUSH, &orig_termios) != 0) {
        @panic("cook failed tcsetattr()");
    }
    tty.close(app_io);
}

pub const Size = struct { width: usize, height: usize };
/// get the window size
pub fn getSize() !Size {
    var win_size = std.mem.zeroes(std.posix.winsize);
    if (linux.ioctl(tty.handle, linux.T.IOCGWINSZ, @intFromPtr(&win_size)) != 0) {
        logging.panicFmt("getsize failed ioctl()", .{});
    }

    return Size{
        .height = win_size.row,
        .width = win_size.col,
    };
}
