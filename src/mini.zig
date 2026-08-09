const std = @import("std");

const doc = @import("document.zig");
const Cursor = doc.Cursor;
const Selection = doc.Selection;
const Point = doc.Pos;
const Direction = doc.Direction;

const tracy = @import("tracy");
const treez = @import("treez");
const vaxis = @import("vaxis");

const format = @import("format.zig");
const write_utils = @import("write_utils.zig");
const misc_utils = @import("misc_utils.zig");
const selection_utils = @import("selection_utils.zig");
const logging = @import("logging.zig");

pub const std_options = logging.std_options;
pub const logger = logging.default_logger;
pub const panic = std.debug.FullPanic(logging.panic);
pub const panicFmt = logging.panicFmt;


/// index of first visible line
// var first_line: usize = 0;
const bottom_ui_rows: usize = 4;
/// rows reserved for permanent ui
const non_content_rows = bottom_ui_rows;
const non_content_cols: usize = 5;  // TODO!

/// window dimensions
var size: Size = undefined;
var tty: vaxis.Tty = undefined;
var vx: vaxis.Vaxis = undefined;

/// vaxis event union. vaxis uses @hasField to only deliver declared fields.
const Event = union(enum) {
    key_press: vaxis.Key,
    key_release: vaxis.Key,
    winsize: vaxis.Winsize,
    focus_in,
    focus_out,
    paste: []const u8,
};

/// return return the pair sorted by row, col
fn getSortedPoints(A: Point, B: Point) struct { Point, Point } {
    if (B.row < A.row) return .{ B, A };
    if (B.row == A.row and B.col < A.col) return .{ B, A };
    return .{ A, B };
}

/// strict cmp, bool (A < B)
fn cmpPoints(A: Point, B: Point) bool {
    if (B.row < A.row) return false;
    if (B.row == A.row and B.col <= A.col) return false;
    return true;
}

/// is A, B, C sorted?
/// specifically: is A <= B < C
fn isBetween(A: Point, B: Point, C: Point) bool {
    return ((!cmpPoints(B, A)) and cmpPoints(B, C));
}

const Mode = enum {
    insert,
    normal
};

var mode: Mode = .normal;

pub fn main(init: std.process.Init) !void {
    // tree-sitter init (parses this file, does not integrate with Document yet)
    const ziglang = try treez.Language.get("zig");
    var parser = try treez.Parser.create();
    defer parser.destroy();
    try parser.setLanguage(ziglang);
    const inp = @embedFile("mini.zig");
    const tree = try parser.parseString(null, inp);
    defer tree.destroy();

    var tty_buf: [1024]u8 = undefined;
    tty = try vaxis.Tty.init(init.io, &tty_buf);
    defer tty.deinit();

    var gpa = std.heap.DebugAllocator(.{}){};
    const alloc = gpa.allocator();

    vx = try vaxis.init(init.io, alloc, init.environ_map, .{
        .system_clipboard_allocator = alloc,
    });
    defer vx.deinit(alloc, tty.writer());

    try vx.enterAltScreen(tty.writer());

    try logging.loggerInit(null);
    defer logging.loggerDeinit();

    size = try getSize();


    const rope = try doc.openAsRope(alloc, "src/document.zig", init.io); // 6k
    //const rope = try doc.openAsRope(alloc, "/home/alex/Downloads/data-1717158044627.csv"); // 17M
    //const rope = try doc.openAsRope(alloc, "/home/alex/Downloads/data-1720544170329.csv"); // 100k
    // /home/alex/Downloads/data-1717158044627.csv 17M

    defer rope.releaseWithFn(@TypeOf(rope.value.*).deinit);
    //const vp = doc.ViewPort {.height = size.height-2, .width=size.width-1};
    var dc = try doc.Document.init(alloc, size.height-non_content_rows, size.width-4, rope);

    {
        const txt = try dc.getText();
        try render(null, dc.cursor, txt, dc.render_buffer.viewport);
    }

    // CRITICAL: start the loop BEFORE queryTerminal — queryTerminal blocks on
    // a futex that is only woken by the loop's reader thread processing the
    // terminal's capability response.
    var loop: vaxis.Loop(Event) = .init(init.io, &tty, &vx);
    try loop.start();
    defer loop.stop();

    try vx.queryTerminal(tty.writer(), .fromSeconds(1));

    while (true) {
        const event = try loop.nextEvent();
        var maybe_key: ?vaxis.Key = null;
        switch (event) {
            .key_press => |key| {
                maybe_key = key;
                const zone = tracy.initZone(@src(), .{ .name = "Handling command" });
                defer zone.deinit();

                // ESC: exit insert mode; do nothing in normal mode
                if (key.codepoint == vaxis.Key.escape) {
                    if (mode == .insert) mode = .normal;
                }
                // quit: q/Q in normal mode (no ctrl/alt)
                else if (mode == .normal and
                    (key.codepoint == 'q' or key.codepoint == 'Q') and
                    !key.mods.ctrl and !key.mods.alt)
                {
                    return;
                }
                // enter insert mode: i (normal mode only)
                else if (mode == .normal and key.codepoint == 'i' and
                    !key.mods.ctrl and !key.mods.alt)
                {
                    mode = .insert;
                }
                // scroll down: j/J/down (no ctrl/alt)
                else if ((key.codepoint == 'j' or key.codepoint == 'J' or
                    key.codepoint == vaxis.Key.down) and !key.mods.ctrl and !key.mods.alt)
                {
                    dc.moveView(1);
                }
                // scroll up: k/K/up (no ctrl/alt)
                else if ((key.codepoint == 'k' or key.codepoint == 'K' or
                    key.codepoint == vaxis.Key.up) and !key.mods.ctrl and !key.mods.alt)
                {
                    dc.moveView(-1);
                }
                // ctrl+arrows (no shift): move cursor + reset selection
                else if (key.mods.ctrl and !key.mods.shift and isArrow(key.codepoint)) {
                    dc.moveCursor(dirFromKey(key.codepoint));
                    dc.cursor.selection = Selection.emptySel(dc.cursor.pos);
                }
                // shift+ctrl+arrows: move cursor + update selection
                else if (key.mods.ctrl and key.mods.shift and isArrow(key.codepoint)) {
                    dc.moveCursor(dirFromKey(key.codepoint));
                    dc.cursor.selection.head = dc.cursor.pos;
                }
                // paste: ctrl+y
                else if (key.matches('y', .{ .ctrl = true })) {
                    try dc.pasteSelection();
                }
                // undo: ctrl+u
                else if (key.matches('u', .{ .ctrl = true })) {
                    dc.undo();
                }
                // page up (plain or ctrl, no shift)
                else if (key.codepoint == vaxis.Key.page_up and !key.mods.shift) {
                    dc.cursorPgUp();
                }
                // shift+ctrl+page_up
                else if (key.codepoint == vaxis.Key.page_up and key.mods.ctrl and key.mods.shift) {
                    dc.cursorPgUp();
                    dc.cursor.selection.head = dc.cursor.pos;
                }
                // page down (plain or ctrl, no shift)
                else if (key.codepoint == vaxis.Key.page_down and !key.mods.shift) {
                    dc.cursorPgDn();
                }
                // shift+ctrl+page_down
                else if (key.codepoint == vaxis.Key.page_down and key.mods.ctrl and key.mods.shift) {
                    dc.cursorPgDn();
                    dc.cursor.selection.head = dc.cursor.pos;
                }
                // ctrl+home (no shift)
                else if (key.codepoint == vaxis.Key.home and key.mods.ctrl and !key.mods.shift) {
                    dc.cursorHome();
                }
                // shift+ctrl+home
                else if (key.codepoint == vaxis.Key.home and key.mods.ctrl and key.mods.shift) {
                    dc.cursorHome();
                    dc.cursor.selection.head = dc.cursor.pos;
                }
                // ctrl+end (no shift)
                else if (key.codepoint == vaxis.Key.end and key.mods.ctrl and !key.mods.shift) {
                    dc.cursorEnd();
                }
                // shift+ctrl+end
                else if (key.codepoint == vaxis.Key.end and key.mods.ctrl and key.mods.shift) {
                    dc.cursorEnd();
                    dc.cursor.selection.head = dc.cursor.pos;
                }
                // insert text (insert mode)
                else if (mode == .insert and key.text != null) {
                    try dc.insertAtCursor(key.text.?);
                }
            },
            .winsize => |ws| {
                size = .{ .width = ws.cols, .height = ws.rows };
            },
            .paste => |text| {
                defer alloc.free(text);
                if (mode == .insert) {
                    try dc.insertAtCursor(text);
                }
            },
            else => {},
        }

        const txt = b: {
            const zone_txt = tracy.initZone(@src(), .{ .name = "Getting text" });
            defer zone_txt.deinit();
            break :b try dc.getText();
        };
        {
            const zone_rndr = tracy.initZone(@src(), .{ .name = "Rendering" });
            defer zone_rndr.deinit();
            try render(maybe_key, dc.cursor, txt, dc.render_buffer.viewport);
        }
    }
}

/// true if codepoint is one of the four arrow keys
fn isArrow(cp: u21) bool {
    return cp == vaxis.Key.up or cp == vaxis.Key.down or
        cp == vaxis.Key.left or cp == vaxis.Key.right;
}

/// map an arrow key codepoint to a Direction
fn dirFromKey(cp: u21) Direction {
    return switch (cp) {
        vaxis.Key.up => .up,
        vaxis.Key.down => .down,
        vaxis.Key.left => .left,
        vaxis.Key.right => .right,
        else => unreachable,
    };
}

/// render the current view
fn render(maybe_key: ?vaxis.Key, cursor: Cursor, lns: [] const doc.LineSlice, view: doc.ViewPort) !void {
    const writer = tty.writer();

    try clear(writer);

    for (lns, 0..) |line, i| {
        try writeLine(writer, line.line, i);
    }
    try render_line_numbers(writer, view);

    //try renderLines(writer);
    try render_sel(writer, cursor, view, lns);
    try render_cursor(writer, cursor, view, lns);
    try render_bottom_ui(maybe_key, writer, cursor, view);

    try writer.flush();
}

// render the line numbers
fn render_line_numbers(writer: anytype, view: doc.ViewPort) !void {
    const first_line = view.start.row;
    const last_line = view.start.row+view.height;
    //non_content_cols = 2 + misc_utils.numDigits(last_line); // TODO!!!
    for (first_line..last_line) |line_ind| {
        const pad_slice = misc_utils.spaces[0..(non_content_cols - misc_utils.numDigits(line_ind) - 1)];
        try moveCursor(writer, line_ind - first_line, 0);
        try writer.print("{s}{d}", .{ pad_slice, line_ind });
    }
    for (last_line..(view.start.row + view.height)) |line_ind| {
        const pad_slice = misc_utils.spaces[0..(non_content_cols - 2)];
        try moveCursor(writer, line_ind - first_line, 0);
        try writer.print("{s}~", .{pad_slice});
    }
}

// /// render file content
// fn render_file_content(writer: anytype) !void {
//     // const last_line: usize = @min(first_line + content_rows, lines_read);
//     const first_line = view.fst;
//     const last_line = view.start.row+view.height;
//     for (first_line..last_line) |line_ind| {
//         try writeLine(writer, lines[line_ind], line_ind - first_line);
//     }
// }

/// render bottom ui
fn render_bottom_ui(maybe_key: ?vaxis.Key, writer: anytype, cursor: Cursor, view: doc.ViewPort) !void {
    // caps indicator line
    try moveCursor(writer, size.height - 2, 0);
    try writer.print("\x1B[46m", .{});
    try writer.print("Protocol: kitty: {s} | rgb: {s} | unicode: {s} | sgr_px: {s}", .{
        if (vx.caps.kitty_keyboard) "yes" else "no",
        if (vx.caps.rgb) "yes" else "no",
        @tagName(vx.caps.unicode),
        if (vx.caps.sgr_pixels) "yes" else "no",
    });
    // key event line
    try moveCursor(writer, size.height - 1, 0);
    if (maybe_key) |key| {
        try writer.print("Key: codepoint={d} mods={any} text={?s}", .{ key.codepoint, key.mods, key.text });
    } else {
        try writer.writeAll("Key: (none)");
    }
    try writer.print("\x1B[49m", .{});
    // status row
    try moveCursor(writer, size.height - 3, 0);
    try writer.print("\x1B[45m", .{});
    try writer.print("MODE: {s:>6}", .{@tagName(mode)});
    try writer.print("\x1B[47m", .{});
    try writer.print(" selection anchor {any}   head {any}", .{
        cursor.selection.anchor, cursor.selection.head });
    try moveCursor(writer, size.height - 4, 0);
    try writer.print("view {any}   cursor {f} (move using CTRL+<arrow>)", .{ view, cursor.pos });
    try writer.print("\x1B[49m", .{});
}

fn render_sel(writer: anytype, cursor: Cursor, view: doc.ViewPort, lns: []const doc.LineSlice) !void {
    const min_p, const max_p = getSortedPoints(cursor.selection.anchor, cursor.selection.head);
    const min_r = min_p.row;
    const max_r = max_p.row;
    // var max_c: usize = undefined;
    // for (min_r..(max_r + 1)) |row_i| max_c = @max(max_c, lines[row_i].len);
    // if (max_r <= min_r) return;
    for (min_r..(max_r + 1)) |row_i| {
        if (row_i < view.start.row or view.start.row+view.height <= row_i) continue;
        const rel_row_i = row_i - view.start.row;
        if (rel_row_i >= lns.len) continue;
        const line: doc.LineSlice = lns[rel_row_i];

        // TODO the below is only correct for col==0
        std.debug.assert(view.start.col==0);
        for (0..line.line.len, line.line) |col_i, ch| {
            if (isBetween(min_p, .{.row=row_i, .col=col_i}, max_p)) {
                try moveCursor(writer, row_i - view.start.row, col_i+non_content_cols);
                // underline
                try writer.writeAll("\x1B[4m");
                // invert
                try writer.writeAll("\x1B[7m");
                var byte = ch;
                if (byte < 32 or byte > 126) byte = ' ';
                try writer.writeByte(byte);
                try writer.writeAll("\x1B[0m");
            }
        }
    }
}

fn render_cursor(writer: anytype, cursor: Cursor, view: doc.ViewPort, lns: []const doc.LineSlice) !void {
    const row = cursor.pos.row;
    const col = cursor.pos.col;
    // if cursor.pos
    if (view.start.row <= row and row < view.start.row+view.height) {
        if (row-view.start.row >= lns.len) return;
        try moveCursor(writer, row - view.start.row, col+non_content_cols);
        // // white bg
        // try writer.writeAll("\x1B[47m");
        // reverse fg/bg
        try writer.writeAll("\x1B[7m");
        // blink
        try writer.writeAll("\x1B[5m");
        // try writer.writeAll(" ");

        // TODO fix so it works without the condition:
        std.debug.assert(view.start.col == 0);
        const line = lns[row-view.start.row].line;
        //if (col >= line.len)
        var byte = if (col < line.len) line[col] else 0;
        if (byte < 32 or byte > 126) byte = ' ';
        try writer.writeByte(byte);
        try writer.writeAll("\x1B[0m");
    }
}

/// write <txt> to the buffer at row y col 0, applying format.myFmtLine
fn writeLine(writer: anytype, txt: []const u8, y: usize) !void {
    try moveCursor(writer, y, non_content_cols);
    try writer.print("{f}", .{format.myFmtLine(txt)});
}

/// move the cursor to row, col, using 0-indexing
fn moveCursor(writer: anytype, row: usize, col: usize) !void {
    try writer.print("\x1B[{};{}H", .{ row + 1, col + 1 });
}

/// clear the buffer
fn clear(writer: anytype) !void {
    try writer.writeAll("\x1B[2J");
}

const Size = struct { width: usize, height: usize };
/// get the window size
fn getSize() !Size {
    const ws = try tty.getWinsize();
    const height: usize = ws.rows;
    // update number of rows available for content
    if (height < non_content_rows) unreachable;
    return Size{
        .height = ws.rows,
        .width = ws.cols,
    };
}

// this seems to ensure all tests are run
// when placed in the root file
test {
    std.testing.refAllDecls(@This());
    // or refAllDeclsRecursive
}

// --- vaxis.Parser.parse() unit tests (kitty + legacy encodings) ---

test "vaxis parser: legacy up arrow" {
    var parser: vaxis.Parser = .{};
    const result = try parser.parse("\x1b[A", std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 3), result.n);
    const key = result.event.?.key_press;
    try std.testing.expectEqual(vaxis.Key.up, key.codepoint);
    try std.testing.expect(key.mods.eql(.{}));
}

test "vaxis parser: legacy ctrl+up arrow" {
    var parser: vaxis.Parser = .{};
    const result = try parser.parse("\x1b[1;5A", std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 6), result.n);
    const key = result.event.?.key_press;
    try std.testing.expectEqual(vaxis.Key.up, key.codepoint);
    try std.testing.expect(key.mods.ctrl);
    try std.testing.expect(!key.mods.shift);
}

test "vaxis parser: legacy shift+ctrl+up arrow" {
    var parser: vaxis.Parser = .{};
    const result = try parser.parse("\x1b[1;6A", std.testing.allocator);
    const key = result.event.?.key_press;
    try std.testing.expectEqual(vaxis.Key.up, key.codepoint);
    try std.testing.expect(key.mods.ctrl);
    try std.testing.expect(key.mods.shift);
}

test "vaxis parser: legacy plain key 'q'" {
    var parser: vaxis.Parser = .{};
    const result = try parser.parse("q", std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 1), result.n);
    const key = result.event.?.key_press;
    try std.testing.expectEqual(@as(u21, 'q'), key.codepoint);
    try std.testing.expect(key.mods.eql(.{}));
}

test "vaxis parser: legacy ctrl+a (0x01)" {
    var parser: vaxis.Parser = .{};
    const result = try parser.parse("\x01", std.testing.allocator);
    const key = result.event.?.key_press;
    try std.testing.expectEqual(@as(u21, 'a'), key.codepoint);
    try std.testing.expect(key.mods.ctrl);
}

test "vaxis parser: legacy escape" {
    var parser: vaxis.Parser = .{};
    const result = try parser.parse("\x1b", std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 1), result.n);
    const key = result.event.?.key_press;
    try std.testing.expectEqual(vaxis.Key.escape, key.codepoint);
}

test "vaxis parser: kitty CSI u plain 'q'" {
    var parser: vaxis.Parser = .{};
    const result = try parser.parse("\x1b[113u", std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 6), result.n);
    const key = result.event.?.key_press;
    try std.testing.expectEqual(@as(u21, 'q'), key.codepoint);
    try std.testing.expect(key.mods.eql(.{}));
}

test "vaxis parser: kitty CSI u ctrl+a" {
    var parser: vaxis.Parser = .{};
    const result = try parser.parse("\x1b[97;5u", std.testing.allocator);
    const key = result.event.?.key_press;
    try std.testing.expectEqual(@as(u21, 'a'), key.codepoint);
    try std.testing.expect(key.mods.ctrl);
}

test "vaxis parser: kitty CSI u special key (down)" {
    var parser: vaxis.Parser = .{};
    const result = try parser.parse("\x1b[57353u", std.testing.allocator);
    const key = result.event.?.key_press;
    try std.testing.expectEqual(vaxis.Key.down, key.codepoint);
}

test "vaxis parser: partial CSI sequence needs more bytes" {
    var parser: vaxis.Parser = .{};
    const result = try parser.parse("\x1b[", std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 0), result.n);
    try std.testing.expect(result.event == null);
}
