//! parse_utils, parsing input bytes to human-readable
const std = @import("std");
// const fs = std.fs;
// const linux = std.os.linux;

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
                    '\x1B' => try writer.print("\\x{X}", .{byte}),
                    '\n' => try writer.writeAll("LF"),
                    '\r' => try writer.writeAll("CR"),
                    '\x08' => try writer.writeAll("BS"),
                    '\x09' => try writer.writeAll("HT(TAB)"),
                    else => try writer.print("C0/other \\x{X}", .{byte}),
                }
            },
            32...126 => try writer.writeByte(byte),
            127 => try writer.writeAll("DEL"),
            else => try writer.print("non-ASCII \\x{X}", .{byte}),
        }
    }
}

/// format printable chars unchanged
/// otherwise try to print human readable version
fn myFmtBytes(bytes: []const u8) std.fmt.Formatter(formatFn) {
    return .{ .data = bytes };
}

/// interpret raw input bytes
pub fn parse(raw: []const u8) !void {
    const num_read = raw.len;
    std.debug.print("Read {} bytes\n", .{num_read});
    std.debug.print("Read bytes: {s} \n", .{raw});
    std.debug.print("Readable bytes: {s} \n", .{myFmtBytes(raw)});
}
