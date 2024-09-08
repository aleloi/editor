//! terminal utils, mainly entering/exiting alternative buffer and mode
const std = @import("std");
const fs = std.fs;
const linux = std.os.linux;

/// the original settings
var orig_termios: linux.termios = undefined;
/// our alternative settings
var alt_termios: linux.termios = undefined;

/// enter alternative buffer, saving current state
pub fn uncook(tty: fs.File) !void {
    const writer = tty.writer();
    if (linux.tcgetattr(tty.handle, &orig_termios) != 0) {
        @panic("failed tcgetattr()");
    }
    errdefer cook(tty) catch {};

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
}

/// exit alternative buffer, restore previous state
pub fn cook(tty: fs.File) !void {
    const writer = tty.writer();
    try writer.writeAll("\x1B[2J"); // Clear buffer.
    try writer.writeAll("\x1B[?1049l"); // Disable alternative buffer.
    try writer.writeAll("\x1B[?47l"); // Restore screen.
    try writer.writeAll("\x1B[u"); // Restore cursor position.
    try writer.writeAll("\x1B[?25h"); // Show cursor.
    try writer.writeAll("\x1B[0m"); // Attribute reset.
    if (linux.tcsetattr(tty.handle, .FLUSH, &orig_termios) != 0) {
        @panic("cook failed tcsetattr()");
    }
}
