const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const os = std.os;
const linux = std.os.linux;
const posix = std.posix;

const write_utils = @import("write_utils.zig");
const main = @import("mini.zig");
const mu = @import("misc_utils.zig");
const du = @import("dll_utils.zig");
const doc = @import("document.zig");
const ku = @import("key_utils.zig");

// 0-indexing
pub const Point = struct {
    row: usize,
    col: usize,
    pub fn format(
        self: *const @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("({d: >3}, {d: >3})", .{ self.row, self.col });
    }
};
pub fn point(row: usize, col: usize) Point {
    return .{ .row = row, .col = col };
}

/// return return the pair sorted by row, col (min first)
pub fn getSortedPoints(A: Point, B: Point) struct { Point, Point } {
    if (B.row < A.row) return .{ B, A };
    if (B.row == A.row and B.col < A.col) return .{ B, A };
    return .{ A, B };
}

pub fn minPt(A: Point, B: Point) Point {
    if (B.row < A.row) return B;
    if (B.row == A.row and B.col < A.col) return B;
    return A;
}

pub fn maxPt(A: Point, B: Point) Point {
    if (B.row < A.row) return A;
    if (B.row == A.row and B.col < A.col) return A;
    return B;
}

/// strict cmp, bool (A < B)
pub fn cmpPoints(A: Point, B: Point) bool {
    if (B.row < A.row) return false;
    if (B.row == A.row and B.col <= A.col) return false;
    return true;
}

/// A == B ?
pub fn eqPts(A: Point, B: Point) bool {
    return A.row == B.row and A.col == B.col;
}

/// is A, B, C sorted?
/// specifically: is A <= B < C
pub fn isBetween(A: Point, B: Point, C: Point) bool {
    return ((!cmpPoints(B, A)) and cmpPoints(B, C));
}

/// how many bytes from A to B ? needs A <= B
/// closed or half-open interval?
pub fn byteDiff(A: Point, B: Point) !usize {
    // check if A <= B
    if (!eqPts(A, minPt(A, B))) return error.Other;
    var ret: usize = 0;
    const fst = try doc.root.getNode(A.row + 1);
    const lst = try doc.root.getNode(B.row + 1);
    var curr = fst;
    for (A.row..(B.row + 1)) |_| {
        ret += (try curr.getSlice()).len;
        curr = try curr.getNode(1);
    }
    ret -= A.col;
    ret -= ((try lst.getSlice()).len - B.col); // TODO think about this
    return ret;
}

pub const logger = main.logger;
pub const std_options = main.std_options;

/// quit commands
pub const q_eq: [3][]const u8 = .{ "ESC", "Q", "q" };
/// down commands
pub const j_eq: [3][]const u8 = .{ "DOWN", "J", "j" };
/// up commands
pub const k_eq: [3][]const u8 = .{ "UP", "K", "k" };

/// move view
pub const arrows: [4][]const u8 = .{ "UP", "DOWN", "LEFT", "RIGHT" };
/// move cursor
pub const c_arrows: [4][]const u8 = .{ "CTRL+UP", "CTRL+DOWN", "CTRL+LEFT", "CTRL+RIGHT" };
/// change selection
pub const sc_arrows: [4][]const u8 = .{ "SHIFT+CTRL+UP", "SHIFT+CTRL+DOWN", "SHIFT+CTRL+LEFT", "SHIFT+CTRL+RIGHT" };
/// move view fn+arrow
pub const fn_arrows: [4][]const u8 = .{ "PGUP", "PGDN", "HOME", "END" };
/// move cursor fn+arrow
pub const c_fn_arrows: [4][]const u8 = .{ "CTRL+PGUP", "CTRL+PGDN", "CTRL+HOME", "CTRL+END" };
/// change selection fn+arrow
pub const sc_fn_arrows: [4][]const u8 = .{ "SHIFT+CTRL+PGUP", "SHIFT+CTRL+PGDN", "SHIFT+CTRL+HOME", "SHIFT+CTRL+END" };

/// a point, and a selection
pub const Cursor = struct {
    pos: Point,
    t_col: usize = 0,
    sel: Selection = emptySel(point(1, 0)),
    pub fn format(
        self: *const @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("cur ({any}, t_col {d}, {any})", .{ self.pos, self.t_col, self.sel });
    }
};
pub const def_pos = point(0, 0);
pub var cursor: Cursor = .{ .pos = def_pos, .sel = emptySel(def_pos) };

/// tries to match a direction to the end of slice str
pub fn matchDirSuffix(str: []const u8) !Direction {
    logger.debug("matchDirSuffix {s}", .{str});
    for (arrows, dirs) |arrow, dir| {
        // print("'{s}' '{s}'\n", .{ str[(str.len - arrow.len)..], arrow });
        if (std.mem.eql(u8, str[(str.len - arrow.len)..], arrow)) {
            logger.debug("match {any}", .{dir});
            return dir;
        }
    }
    for (fn_arrows, dirs) |arrow, dir| {
        // print("'{s}' '{s}'\n", .{ str[(str.len - arrow.len)..], arrow });
        if (std.mem.eql(u8, str[(str.len - arrow.len)..], arrow)) {
            logger.debug("match {any}", .{dir});
            return dir;
        }
    }
    logger.warn("no match!", .{});
    return error.Error;
}

const dirs = ku.dirs;

pub const Direction = ku.Direction;

// /// unpack Point
// pub fn ptUp(pt: Point) struct { usize, usize } {
//     return .{ pt.row, pt.col };
// }

pub fn movePt(dir: Direction, move_sel: bool, fn_: bool, cursor_: *Cursor) void {
    logger.debug("movePt(dir: {any}, move_sel: {any}, fn_: {any}, cursor_: {any})", .{ dir, move_sel, fn_, cursor_.* });
    if (move_sel) {
        const pt: Point = cursor_.sel.head;
        cursor_.pos, cursor_.t_col = if (fn_) movePtFn(dir, pt, cursor_.t_col) else moveSingleStep(dir, pt, cursor_.t_col);
        cursor_.sel.head = cursor_.pos;
    } else {
        const pt: Point = switch (dir) {
            Direction.up, Direction.left => minPt(cursor_.sel.anchor, cursor_.sel.head),
            Direction.down, Direction.right => maxPt(cursor_.sel.anchor, cursor_.sel.head),
        };
        const t_col = if (cursor_.sel.isEmpty()) cursor_.t_col else pt.col;
        cursor_.pos, cursor_.t_col = if (fn_) movePtFn(dir, pt, t_col) else moveSingleStep(dir, pt, t_col);
        cursor_.sel = emptySel(cursor_.pos);
    }
    logger.debug("movePt cursor {any}", .{cursor_.*});
}

/// dir, pt, target_col
///
/// return new {pt, tcol}
pub fn moveSingleStep(dir: Direction, pt: Point, tc: usize) struct { Point, usize } {
    var crow = pt.row;
    var ccol = pt.col;
    var tcol: usize = tc;
    const n_rows = main.lines_read;
    switch (dir) {
        Direction.up => {
            if (crow < 1) {
                crow = 0;
                ccol = 0;
            } else {
                crow -= 1;
                ccol = @min(main.lines[crow].len, tc);
            }
        },
        Direction.down => {
            if (crow >= n_rows - 1) {
                crow = n_rows - 1;
                ccol = main.lines[crow].len;
            } else {
                crow += 1;
                ccol = @min(main.lines[crow].len, tc);
            }
        },
        Direction.left => {
            if (ccol < 1) {
                if (crow > 0) {
                    crow -= 1;
                    ccol = main.lines[crow].len;
                } else {
                    ccol = 0;
                }
            } else {
                ccol -= 1;
            }
            tcol = ccol;
        },
        Direction.right => {
            if (ccol >= main.lines[crow].len) {
                if (crow < n_rows - 1) {
                    crow += 1;
                    ccol = 0;
                } else {
                    ccol = main.lines[crow].len;
                }
            } else {
                ccol += 1;
            }
            tcol = ccol;
        },
    }

    // // if tcol unchanged, return null
    // if (tcol == pt.col) tcol = null;

    return .{ point(crow, ccol), tcol };
}

/// dir, pt, target_col
///
/// return new {pt, tcol}
pub fn movePtFn(dir: Direction, pt: Point, tc: usize) struct { Point, usize } {
    var crow = pt.row;
    var ccol = pt.col;
    var tcol: usize = tc;
    const n_rows = main.lines_read;
    switch (dir) {
        Direction.up => {
            if (crow < 1) {
                ccol = 0;
            } else {
                crow -= @min(crow, main.content_rows - 1);
                ccol = @min(main.lines[crow].len, tc);
            }
        },
        Direction.down => {
            if (crow >= n_rows - 1) {
                ccol = main.lines[crow].len;
            } else {
                crow += @min(n_rows - 1 - main.content_rows - 1, main.content_rows - 1);
                ccol = @min(main.lines[crow].len, tc);
            }
        },
        Direction.left => {
            ccol = 0;
            tcol = ccol;
        },
        Direction.right => {
            ccol = main.lines[crow].len;
            tcol = ccol;
        },
    }

    // // if tcol unchanged, return null
    // if (tcol == pt.col) tcol = null;

    return .{ point(crow, ccol), tcol };
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

        try writer.print("sel (an {any}, he {any})", .{ self.anchor, self.head });
    }
    /// is the current selection empty? that is, is `head == anchor`?
    pub fn isEmpty(self: *const @This()) bool {
        return self.anchor.row == self.head.row and self.anchor.col == self.head.col;
    }
};
pub fn emptySel(pos: Point) Selection {
    return .{ .head = pos, .anchor = pos };
}
