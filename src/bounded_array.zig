//! Vendored BoundedArray from Zig 0.13 stdlib.
//! std.BoundedArray was removed in Zig 0.16.
const std = @import("std");

pub fn BoundedArray(comptime T: type, comptime capacity: usize) type {
    return struct {
        buffer: [capacity]T = undefined,
        len: usize = 0,

        pub const Self = @This();

        pub const Error = error{Overflow};

        pub fn slice(self: anytype) @TypeOf(self.buffer[0..self.len]) {
            return self.buffer[0..self.len];
        }

        pub fn append(self: *Self, value: T) Error!void {
            if (self.len >= capacity) return error.Overflow;
            self.buffer[self.len] = value;
            self.len += 1;
        }

        pub fn pop(self: *Self) T {
            if (self.len == 0) unreachable;
            self.len -= 1;
            return self.buffer[self.len];
        }

        pub fn get(self: *const Self, index: usize) T {
            return self.buffer[index];
        }

        pub fn set(self: *Self, index: usize, value: T) void {
            self.buffer[index] = value;
        }

        pub fn fromSlice(source: []const T) Error!Self {
            if (source.len > capacity) return error.Overflow;
            var result: Self = .{};
            @memcpy(result.buffer[0..source.len], source);
            result.len = source.len;
            return result;
        }
    };
}

test "BoundedArray basic" {
    var arr: BoundedArray(u8, 4) = .{};
    try arr.append(1);
    try arr.append(2);
    try arr.append(3);
    try std.testing.expect(arr.len == 3);
    try std.testing.expect(arr.slice()[0] == 1);
    _ = arr.pop();
    try std.testing.expect(arr.len == 2);
    try std.testing.expect(arr.get(0) == 1);
}
