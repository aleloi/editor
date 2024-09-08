//! formatting printable/unprintable chars
const std = @import("std");

/// format writables unchanged
/// otherwise space with red background
/// if bytes are longer than WIDTH,
/// truncate and mark with space
/// with green background
const WIDTH: usize = 100;

fn formatFn(
    bytes: []const u8,
    comptime f: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = options;
    _ = f;

    for (bytes[0..@min(bytes.len, WIDTH - 1)]) |byte| {
        if (byte >= 32 and byte <= 126) {
            try writer.writeByte(byte);
        } else {
            try writer.writeAll("\x1B[41m");
            try writer.writeAll(" ");
            try writer.writeAll("\x1B[0m");
        }
    }
    if (bytes.len >= WIDTH) {
        try writer.writeAll("\x1B[42m");
        try writer.writeAll(" ");
        try writer.writeAll("\x1B[0m");
    }
}

/// format printable chars unchanged
/// otherwise space with red background
/// if bytes is longer than WIDTH,
/// truncate and mark with space
/// with green background
pub fn myFmtLine(bytes: []const u8) std.fmt.Formatter(formatFn) {
    return .{ .data = bytes };
}

test "test dots" {
    const str: []const u8 = "my_test_string";
    std.debug.print("testing .. : {s}\n", .{str[0..4]});
    std.debug.print("testing ... : ", .{});
    for (0..16) |i| {
        switch (i) {
            0...4 => std.debug.print("{c}", .{str[i]}),
            else => continue,
        }
    }
    std.debug.print("\n", .{});
}

test "test format" {
    var my_buf: [256]u8 = .{0} ** 256;
    for (0..256) |i| my_buf[i] = @as(u8, @truncate(i));
    for (0..16) |i| {
        std.debug.print("{}\n", .{myFmtLine(my_buf[(i * 16)..][0..16])});
    }
    std.debug.print("{}\n", .{myFmtLine(&my_buf)});
}
