const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const os = std.os;
const linux = std.os.linux;
const posix = std.posix;
const print = std.debug.print;
const panic = std.debug.panic;
const write_utils = @import("write_utils.zig");
const main = @import("mini.zig");
const document = @import("document.zig");

const Point = main.Point;
const point = main.point;
const Direction = document.Direction;

const logger = main.logger;

/// a point, and a selection
pub const Cursor = struct {
    pos: Point,
    target_col: usize = 0,
    selection: Selection = emptySel(point(1, 0)),
    pub fn format(
        self: *const @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("cursor ( {any}, t_col {d}, {any} )", .{ self.pos, self.target_col, self.selection });
    }
};
pub const def_pos = point(0, 0);
pub var cursor: Cursor = .{ .pos = def_pos, .selection = emptySel(def_pos) };

/// tries to match a direction to the end of slice str
pub fn matchDirSuffix(str: []const u8) !Direction {
    for (main.arrows, dirs) |arrow, dir| {
        // print("'{s}' '{s}'\n", .{ str[(str.len - arrow.len)..], arrow });
        if (std.mem.eql(u8, str[(str.len - arrow.len)..], arrow)) {
            return dir;
        }
    }
    return error.Error;
}

const dirs: [4]Direction = .{ Direction.up, Direction.down, Direction.left, Direction.right };

// pub const Direction = enum {
//     up,
//     down,
//     left,
//     right,
//     fn pt(self: Direction) struct { isize, isize } {
//         switch (self) {
//             Direction.up => return .{ -1, 0 },
//             Direction.down => return .{ 1, 0 },
//             Direction.left => return .{ 0, -1 },
//             Direction.right => return .{ 0, 1 },
//         }
//     }
// };

/// move the cursor single step in cardinal direction
pub fn moveVCursorStep(dir: Direction) void {
    // current pos
    var crow = cursor.pos.row;
    var ccol = cursor.pos.col;
    //
    const n_row = main.lines_read;

    switch (dir) {
        Direction.up => {
            if (crow < 1) {
                ccol = 0;
            } else {
                crow -= 1;
                ccol = @min(main.lines[crow].len, cursor.target_col);
            }
        },
        Direction.down => {
            if (crow >= n_row - 1) {
                ccol = main.lines[crow].len;
            } else {
                crow += 1;
                ccol = @min(main.lines[crow].len, cursor.target_col);
            }
        },
        Direction.left => {
            if (ccol < 1) {
                if (crow > 0) {
                    crow -= 1;
                    ccol = main.lines[crow].len;
                } else {}
            } else {
                ccol -= 1;
            }
            cursor.target_col = ccol;
        },
        Direction.right => {
            if (ccol >= main.lines[crow].len) {
                if (crow < n_row - 1) {
                    crow += 1;
                    ccol = 0;
                } else {}
            } else {
                ccol += 1;
            }
            cursor.target_col = ccol;
        },
    }
    // save changes
    cursor.pos = point(crow, ccol);

    // make sure the cursor is visible after being moved
    if (crow < main.view.fst) main.moveView(-@as(isize, @intCast(main.view.fst - crow)));
    logger.debug("moveView {} {}", .{ crow, main.view.lst });

    if (crow >= main.view.lst) main.moveView(@as(isize, @intCast(crow + 1 - main.view.lst)));
}

// /// move the the cursor
// pub fn moveVCursorRel(rows: isize, cols: isize) void {
//     var crow = cursor.pos.row;
//     var ccol = cursor.pos.col;
//     if (rows < 0) {
//         crow -= @min(crow, @abs(rows));
//     } else if (rows > 0) {
//         crow += @min(main.lines_read - crow, @abs(rows));
//     }
//     const clen = main.lines[crow].len;
//     if (cols == 0) {
//         ccol = @min(clen, cursor.target_col);
//     } else {
//         if (cols > 0) {
//             ccol += @min(clen - ccol, @abs(cols));
//         } else if (cols < 0) {
//             ccol -= @min(ccol, @abs(cols));
//         }
//         cursor.target_col = ccol;
//     }
//     // save changes
//     cursor.pos = point(crow, ccol);

//     // make sure the cursor is visible after being moved
//     if (crow < main.view.fst) main.moveView(-@as(isize, @intCast(main.view.fst - crow)));
//     if (crow >= main.view.lst) main.moveView(@intCast(main.view.lst + 1 - crow));
// }

/// try to move the cursor to target point
pub fn setVCursorPos(target: Point) void {
    var trow = target.row;
    var tcol = target.col;
    if (trow > main.lines_read) trow = main.lines_read;
    const tlen = main.lines[trow].len;
    if (tcol > tlen) tcol = tlen;
    cursor.pos = point(trow, tcol);
    cursor.target_col = tcol;
}

pub fn cursorHome() void {
    logger.debug("cursorHome called!", .{});
    cursor.pos = point(cursor.pos.row, 0);
    cursor.target_col = 0;
}

pub fn cursorEnd() void {
    logger.debug("cursorEnd called!", .{});
    const crow = cursor.pos.row;
    const clen = main.lines[crow].len;
    cursor.pos = point(crow, clen);
    cursor.target_col = clen;
}

pub fn cursorPgUp() void {
    logger.debug("cursorPgUp called!", .{});
    if (cursor.pos.row > main.content_rows - 1) {
        cursor.pos.row -= (main.content_rows - 1);
    } else {
        if (cursor.pos.row == 0) cursor.target_col = 0;
        cursor.pos.row = 0;
    }

    // make sure the cursor is visible after being moved
    const crow = cursor.pos.row;
    if (crow < main.view.fst) main.moveView(-@as(isize, @intCast(main.view.fst - crow)));
    if (crow >= main.view.lst) main.moveView(@intCast(main.view.lst + 1 - crow));
}

pub fn cursorPgDn() void {
    logger.debug("cursorPgDn called!", .{});
    if (cursor.pos.row + main.content_rows - 1 < main.lines_read) {
        cursor.pos.row += (main.content_rows - 1);
    } else {
        if (cursor.pos.row == main.lines_read - 1) cursor.target_col = main.lines[main.lines_read - 1].len;
        cursor.pos.row = main.lines_read - 1;
    }

    // make sure the cursor is visible after being moved
    const crow = cursor.pos.row;
    if (crow < main.view.fst) main.moveView(-@as(isize, @intCast(main.view.fst - crow)));
    logger.debug("moveView {} {}", .{ crow, main.view.lst });
    if (crow >= main.view.lst) main.moveView(@as(isize, @intCast(crow + 1 - main.view.lst)));
}

/// movable head, immovable anchor.
/// regular cursor/empty selection: head = anchor
pub const Selection = struct {
    head: Point,
    anchor: Point,
    pub fn format(
        self: *const @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("selection ( anchor {any}, head {any} )", .{ self.anchor, self.head });
    }
};
pub fn emptySel(pos: Point) Selection {
    return .{ .head = pos, .anchor = pos };
}
