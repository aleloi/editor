//! definition of key-related types
const std = @import("std");

pub const Arrow = struct {
    dir: Direction,
    fn_: bool,
};

pub const Key = union(enum) {
    ascii: u7,
    fkey: u5,
    arrow: Arrow,
    Insert,
    Delete,
};

pub const KeyPress = struct {
    key: Key,
    shift: bool,
    alt: bool,
    ctrl: bool,
    // meta: bool,
};

pub const arrows: [4]Arrow = .{
    .{ .dir = dirs[0], .fn_ = false },
    .{ .dir = dirs[1], .fn_ = false },
    .{ .dir = dirs[2], .fn_ = false },
    .{ .dir = dirs[3], .fn_ = false },
};

pub const fn_arrows: [4]Arrow = .{
    .{ .dir = dirs[0], .fn_ = true },
    .{ .dir = dirs[1], .fn_ = true },
    .{ .dir = dirs[2], .fn_ = true },
    .{ .dir = dirs[3], .fn_ = true },
};

pub const dirs: [4]Direction = .{ Direction.up, Direction.down, Direction.left, Direction.right };

pub const Direction = enum {
    up,
    down,
    left,
    right,
    fn pt(self: Direction) struct { isize, isize } {
        switch (self) {
            Direction.up => return .{ -1, 0 },
            Direction.down => return .{ 1, 0 },
            Direction.left => return .{ 0, -1 },
            Direction.right => return .{ 0, 1 },
        }
    }
    pub fn format(
        self: *const @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.writeAll(switch (self.*) {
            Direction.up => "up",
            Direction.down => "down",
            Direction.left => "left",
            Direction.right => "right",
        });
    }
};
