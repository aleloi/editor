const std = @import("std");

pub fn Rc(comptime T: type) type {
    return struct {
        value: *T,
        pub fn init(alloc: std.mem.Allocator, t: T) !@This() {
            var res: @This() = .{ .value=undefined };
            res.value = try alloc.create(T);
            res.value.* = t;
            return res;
        }

        pub fn releaseWithFn(self: @This(), f: anytype) void {
            _ = f;
            _ = self;
        }

        pub fn retain(self: @This()) @This() {
            return self;
        }
    };
}

