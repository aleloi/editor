//! formatting printable/unprintable chars
const std = @import("std");
const Writer = std.Io.Writer;

const WIDTH: usize = 100;

pub const MyFmtLine = struct {
    data: []const u8,
    pub fn format(s: @This(), writer: *Writer) Writer.Error!void {
        for (s.data[0..@min(s.data.len, WIDTH - 1)]) |byte| {
            if (byte >= 32 and byte <= 126) {
                try writer.writeByte(byte);
            } else {
                try writer.writeAll("\x1B[41m");
                try writer.writeAll(" ");
                try writer.writeAll("\x1B[0m");
            }
        }
        if (s.data.len >= WIDTH) {
            try writer.writeAll("\x1B[42m");
            try writer.writeAll(" ");
            try writer.writeAll("\x1B[0m");
        }
    }
};

/// format printable chars unchanged
/// otherwise space with red background
/// if bytes is longer than WIDTH,
/// truncate and mark with space
/// with green background
pub fn myFmtLine(bytes: []const u8) MyFmtLine {
    return .{ .data = bytes };
}

pub const MyFmtBytes = struct {
    data: []const u8,
    pub fn format(s: @This(), writer: *Writer) Writer.Error!void {
        for (s.data) |byte| {
            try writer.writeAll("\"");
            defer writer.writeAll("\" ") catch {};
            switch (byte) {
                // unprintable ascii
                0...31, 127 => try writer.print("\\x{X:0>2}", .{byte}),
                // printable ascii
                32...126 => try writer.writeByte(byte),
                // non-ascii (including 128...255)
                else => try writer.print("non-ASCII \\x{X:0>2}", .{byte}),
            }
        }
    }
};

/// format printable chars unchanged
/// otherwise print hex
pub fn myFmtBytes(bytes: []const u8) MyFmtBytes {
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

// test "test panic" {
//     @panic("Test to ensure all tests are run.");
// }
