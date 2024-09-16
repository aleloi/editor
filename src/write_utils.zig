//! write utils

const std = @import("std");

const stderr = std.io.getStdErr();
/// writes to stderr
pub const stderr_writer = stderr.writer();

/// writes to std.debug as well as the supplied writer
pub fn MultiWriter(comptime WriterType: type) type {
    return struct {
        wrapped_writer: WriterType,

        const Self = @This();
        pub const Error = WriterType.Error;
        pub const Writer = std.io.Writer(*Self, Error, write);

        pub fn write(self: *Self, bytes: []const u8) Error!usize {
            // std.debug.print("{s}", .{bytes});
            _ = stderr_writer.write(bytes) catch {};
            return try self.wrapped_writer.write(bytes);
        }

        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }
    };
}

/// get a MultiWriter, that writes to std.debug as well as the supplied writer
pub fn multiWriter(writer: anytype) MultiWriter(@TypeOf(writer)) {
    return .{ .wrapped_writer = writer };
}

test "test stderr_writer format" {
    // zig test src/parse_utils.zig

    for (0..128) |i| {
        _ = stderr_writer.print("\\x{X:0>2}", .{i}) catch {
            unreachable;
        };
    }
}
