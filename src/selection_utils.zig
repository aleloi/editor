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

// 0-indexing
pub const Point = struct {
    row: usize,
    col: usize,
    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
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

/// is A, B, C sorted?
/// specifically: is A <= B < C
pub fn isBetween(A: Point, B: Point, C: Point) bool {
    return ((!cmpPoints(B, A)) and cmpPoints(B, C));
}

pub const logger = main.logger;
pub const std_options = main.std_options;

/// a point, and a selection
pub const Cursor = struct {
    pos: Point,
    t_col: usize = 0,
    sel: Selection = emptySel(point(1, 0)),
    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("cur ({f}, t_col {d}, {f})", .{ self.pos, self.t_col, self.sel });
    }
};
pub const def_pos = point(0, 0);
pub var cursor: Cursor = .{ .pos = def_pos, .sel = emptySel(def_pos) };

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
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.writeAll(switch (self) {
            Direction.up => "up",
            Direction.down => "down",
            Direction.left => "left",
            Direction.right => "right",
        });
    }
};

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
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("sel (an {f}, he {f})", .{ self.anchor, self.head });
    }
    /// is the current selection empty? that is, is `head == anchor`?
    pub fn isEmpty(self: *const @This()) bool {
        return self.anchor.row == self.head.row and self.anchor.col == self.head.col;
    }
};
pub fn emptySel(pos: Point) Selection {
    return .{ .head = pos, .anchor = pos };
}
