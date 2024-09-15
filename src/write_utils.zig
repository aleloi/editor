//! write utils

const std = @import("std");

/// implements writer.writeByte, .writeAll, .print
/// using std.debug.print
pub const DebugWriter = struct {
    pub fn writeByte(byte: u8) !void {
        std.debug.print("{c}", .{byte});
    }
    pub fn writeAll(bytes: []const u8) !void {
        std.debug.print("{s}", .{bytes});
    }
    pub fn print(comptime fmt: []const u8, args: anytype) !void {
        std.debug.print(fmt, args);
    }
};

// pub fn BufferedWriter(comptime buffer_size: usize, comptime WriterType: type) type {
//     return struct {
//         unbuffered_writer: WriterType,
//         buf: [buffer_size]u8 = undefined,
//         end: usize = 0,

// /// Takes a tuple of streams, and constructs a new stream that writes to all of them
// pub fn MultiWriter(comptime Writers: type) type {
//     comptime var ErrSet = error{};
//     inline for (@typeInfo(Writers).Struct.fields) |field| {
//         const StreamType = field.type;
//         ErrSet = ErrSet || StreamType.Error;
//     }

//     return struct {
//         const Self = @This();

//         streams: Writers,

//         pub const Error = ErrSet;
//         pub const Writer = io.Writer(*Self, Error, write);

//         pub fn writer(self: *Self) Writer {
//             return .{ .context = self };
//         }

//         pub fn write(self: *Self, bytes: []const u8) Error!usize {
//             inline for (self.streams) |stream|
//                 try stream.writeAll(bytes);
//             return bytes.len;
//         }
//     };
// }

// pub fn multiWriter(streams: anytype) MultiWriter(@TypeOf(streams)) {
//     return .{ .streams = streams };
// }

/// writes to std.debug as well as the supplied writer
pub fn MultiWriter(comptime WriterType: type) type {
    return struct {
        wrapped_writer: WriterType,

        const Self = @This();
        pub const Error = WriterType.Error;
        pub const Writer = std.io.Writer(*Self, Error, write);

        pub fn write(self: *Self, bytes: []const u8) Error!usize {
            std.debug.print("{s}", .{bytes});
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
