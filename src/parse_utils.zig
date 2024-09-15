//! parse_utils, parsing terminal input sequences
const std = @import("std");
// const fs = std.fs;
// const linux = std.os.linux;

/// something went wrong while parsing
const ParseError = error{ ParseOther, C0MatchError, GenericMatchError } || ParseModiferError;
/// something went wrong while parsing the modifier keys
const ParseModiferError = error{ ParseModifierOther, NoModifiers, ValueError };

const CSI: [2]u8 = .{ '\x1B', '[' };

/// tries to match the string in bytes to a string in codes.
/// prints using writer if successful,
/// otherwise returns ParseError
fn genericMatchCode(bytes: []const u8, codes: []const []const u8, keys: []const []const u8, writer: anytype) !void {
    for (codes, keys) |code, key| {
        if (std.mem.eql(u8, bytes, code)) {
            try writer.writeAll(key);
            return;
        }
    }
    return ParseError.GenericMatchError;
}

/// tries to map ascii -> ctrl+[key]
fn matchC0(byte: u8, writer: anytype) !void {
    const codes: [27][]const u8 = .{ "\x11", "\x17", "\x05", "\x12", "\x14", "\x19", "\x15", "\x0F", "\x10", "\x01", "\x13", "\x04", "\x06", "\x07", "\x0B", "\x0C", "\x1A", "\x18", "\x03", "\x16", "\x02", "\x0E", "\x00", "\x1E", "\x1F", "\x1C", "\x1D" };
    const keys: [27][]const u8 = .{ "CTRL+q", "CTRL+w", "CTRL+e", "CTRL+r", "CTRL+t", "CTRL+y", "CTRL+u", "CTRL+o", "CTRL+p", "CTRL+a", "CTRL+s", "CTRL+d", "CTRL+f", "CTRL+g", "CTRL+k", "CTRL+l", "CTRL+z", "CTRL+x", "CTRL+c", "CTRL+v", "CTRL+b", "CTRL+n", "CTRL+' / CTRL+´ / CTRL+2 / CTRL+SPACE", "CTRL+^", "CTRL+-", "CTRL+< / CTRL+7", "CTRL+9 / CTRL+0" };
    _ = genericMatchCode(&[1]u8{byte}, &codes, &keys, writer) catch {
        return ParseError.C0MatchError;
    };
}

/// interprets a modifier byte
fn interpretModifier(byte: u8, writer: anytype) !void {
    if (byte < 1 or byte > 16) return ParseModiferError.ValueError;

    const bmap: u8 = byte - 1;
    // modifier bitmap
    if (bmap & 1 != 0) try writer.writeAll("SHIFT+");
    if (bmap & 2 != 0) try writer.writeAll("ALT+");
    if (bmap & 4 != 0) try writer.writeAll("CTRL+");
    if (bmap & 8 != 0) try writer.writeAll("META+");
    // function shouldnt be called if no modifiers are active
    if (bmap == 0) return ParseModiferError.NoModifiers;
}

/// incomplete mapping of double byte xterm keycodes for special keys.
fn xtermKeycodeDouble(bytes: []const u8, writer: anytype) !void {
    const codes: [4][]const u8 = .{ "1P", "1Q", "1R", "1S" };
    const keys: [4][]const u8 = .{ "F1", "F2", "F3", "F4" };
    for (codes, keys) |code, key| {
        if (std.mem.eql(u8, bytes, code)) {
            try writer.writeAll(key);
            break;
        }
    } else {
        try writer.writeAll("<xcode-other>");
        return ParseError.ParseOther;
    }
}

/// incomplete mapping of single byte xterm keycodes for special keys.
fn xtermKeycodeSingle(byte: u8, writer: anytype) !void {
    switch (byte) {
        'A' => try writer.writeAll("UP"),
        'B' => try writer.writeAll("DOWN"),
        'C' => try writer.writeAll("RIGHT"),
        'D' => try writer.writeAll("LEFT"),
        'F' => try writer.writeAll("END"),
        'H' => try writer.writeAll("HOME"),
        else => {
            try writer.writeAll("<xcode-other>");
            return ParseError.ParseOther;
        },
    }
}

fn parseBytesXterm(bytes: []const u8, writer: anytype) !void {
    switch (bytes.len) {
        1 => try xtermKeycodeSingle(bytes[0], writer),
        2 => try xtermKeycodeDouble(bytes[0..2], writer),
        else => try writer.writeAll("<xterm-not-implemented>"),
    }
}

fn parseBytesVtOld(bytes: []const u8, writer: anytype) !void {
    const codes: [5][]const u8 = .{ "[A", "[B", "[C", "[D", "[E" };
    const keys: [5][]const u8 = .{ "F1", "F2", "F3", "F4", "F5" };

    _ = genericMatchCode(bytes, &codes, &keys, writer) catch {
        try writer.writeAll("<vt-old-not-implemented>");
        return ParseError.ParseOther;
    };
}

fn vtKeycode(bytes: []const u8, writer: anytype) !void {
    const codes: [20][]const u8 = .{ "1", "2", "3", "4", "5", "6", "7", "8", "11", "12", "13", "14", "15", "17", "18", "19", "20", "21", "23", "24" };
    const keys: [20][]const u8 = .{ "HOME", "INSERT", "DELETE", "END", "PGUP", "PGDN", "HOME", "END", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12" };
    for (codes, keys) |code, key| {
        if (std.mem.eql(u8, bytes, code)) {
            try writer.writeAll(key);
            break;
        }
    } else {
        try writer.writeAll("<vt-other>");
        return ParseError.ParseOther;
    }
}

/// <esc> '[' (<keycode>) (';'<modifier>) '~'
/// only (<keycode>) (';'<modifier>) remains
fn parseBytesVt(bytes: []const u8, writer: anytype) !void {
    var maybe_keycode: ?[]const u8 = null;
    var maybe_modifier: ?[]const u8 = null;
    var split_it = std.mem.splitSequence(u8, bytes, ";");
    maybe_keycode = split_it.next();
    maybe_modifier = split_it.next();
    if (maybe_modifier) |modifier| {
        if (modifier.len != 1) {
            return ParseError.ParseOther;
        } else {
            try interpretModifier(modifier[0], writer);
        }
    }
    if (maybe_keycode) |keycode| {
        try vtKeycode(keycode, writer);
    } else return ParseError.ParseOther;
}

/// try to parse terminal input sequence
/// https://en.wikipedia.org/wiki/ANSI_escape_code#Terminal_input_sequences
fn parseInputBytes(bytes: []const u8, writer: anytype) !void {
    std.debug.print("\nparseInputBytes called with bytes: ", .{});
    for (bytes) |byte| {
        std.debug.print(" \\x{X:0>2} ", .{byte});
    }
    std.debug.print("\n", .{});
    switch (bytes.len) {
        0 => {
            return ParseError.ParseOther;
        },
        1 => {
            switch (bytes[0]) {
                '\x1B' => try writer.writeAll("ESC / CTRL+8"),
                // regular ascii
                32...126 => |byte| try writer.writeByte(byte),
                // low ascii + 127
                0...26, 28...31, 127 => |byte| {
                    matchC0(byte, writer) catch |err| {
                        switch (err) {
                            ParseError.C0MatchError => try writer.print("{s}", .{myFmtBytes(&[1]u8{byte})}),
                            else => unreachable,
                        }
                    };
                },
                else => {
                    return ParseError.ParseOther;
                },
            }
        },
        2 => {
            if (bytes[0] != '\x1B') return ParseError.ParseOther;
            try writer.writeAll("ALT+\"");
            try parseInputBytes(bytes[1..], writer);
            try writer.writeAll("\"");
        },
        else => {
            // we should not have recieved so many bytes
            // if they are not an escape code
            // so the first bytes should be <esc>[
            if (!std.mem.eql(u8, bytes[0..2], &CSI)) return ParseError.ParseOther;
            if (bytes[0] != '\x1B' or bytes[1] != '[') {
                return ParseError.ParseOther;
            }
            if (bytes[bytes.len - 1] == '~') {
                // vt sequence
                // <esc> '[' (<keycode>) (';'<modifier>) '~'
                try parseBytesVt(bytes[2..(bytes.len - 1)], writer);
            } else {
                if (bytes[2] == '[') {
                    // older vt sequence
                    try parseBytesVtOld(bytes[2..], writer);
                } else {
                    // xterm sequence
                    // <esc> '[' (<modifier>) <char>
                    try parseBytesXterm(bytes[2..], writer);
                }
            }
        },
    }
}

fn formatFn(
    bytes: []const u8,
    comptime f: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = options;
    _ = f;
    for (bytes) |byte| {
        try writer.writeAll("\"");
        defer writer.writeAll("\" ") catch {};
        switch (byte) {
            0...31 => {
                // C0, ASCII control characters
                switch (byte) {
                    '\x1B' => try writer.print("\\x{X:0>2}", .{byte}),
                    '\n' => try writer.writeAll("LF / CTRL+j"),
                    '\r' => try writer.writeAll("CR / ENTER / CTRL+m"),
                    '\x08' => try writer.writeAll("BS / CTRL+BACKSPACE / CTRL+h"),
                    '\x09' => try writer.writeAll("HT(TAB) / CTRL+i"),
                    else => try writer.print("C0/other \\x{X:0>2}", .{byte}),
                    // else => unreachable,
                }
            },
            32...126 => try writer.writeByte(byte),
            127 => try writer.writeAll("DEL / BACKSPACE / CTRL++"),
            else => try writer.print("non-ASCII \\x{X:0>2}", .{byte}),
        }
    }
}

/// format printable chars unchanged
/// otherwise try to print human readable version
fn myFmtBytes(bytes: []const u8) std.fmt.Formatter(formatFn) {
    return .{ .data = bytes };
}

pub fn parseWrite(raw: []const u8, writer: anytype) !void {
    // std.debug.print("Input sequence parse attempt: ", .{});
    if (raw.len > 0) {
        parseInputBytes(raw, writer) catch {
            try writer.print("Readable bytes: {s}", .{myFmtBytes(raw)});
        };
    } else {
        try writer.print("Readable bytes: {s}", .{myFmtBytes(raw)});
    }
    // std.debug.print("\n", .{});
}

pub fn rawWrite(bytes: []const u8, writer: anytype) !void {
    for (bytes) |byte| {
        switch (byte) {
            32...126 => try writer.print(" \" {c} \"", .{byte}),
            else => try writer.print(" \" \\x{X:0>2} \"", .{byte}),
        }
    }
}

test "test parse bytes 0..128" {
    // zig test src/parse_utils.zig
    const write_utils = @import("write_utils.zig");

    for (0..128) |i| {
        try parseWrite(&[1]u8{@as(u8, @truncate(i))}, write_utils.DebugWriter);
    }
}

test "test parse bytes 27(ALT/ESC/...) + 0..128" {
    // zig test src/parse_utils.zig
    const write_utils = @import("write_utils.zig");

    for (0..128) |i| {
        try parseWrite(&[2]u8{ 27, @as(u8, @truncate(i)) }, write_utils.DebugWriter);
    }
}
