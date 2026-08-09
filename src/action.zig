const std = @import("std");
const vaxis = @import("vaxis");
const doc = @import("document.zig");

pub const Mode = enum {
    insert,
    normal,
};

pub const Action = union(enum) {
    no_op,
    quit,
    change_mode: Mode,
    move_cursor: struct { dir: doc.Direction, extend_selection: bool },
    scroll_view: isize,
    page_up: struct { extend_selection: bool },
    page_down: struct { extend_selection: bool },
    cursor_home: struct { extend_selection: bool },
    cursor_end: struct { extend_selection: bool },
    insert_text: []const u8,
    paste_selection,
    undo,
};

/// Map a key event + current mode to an Action. PURE — no I/O, no side effects.
/// Mirrors the if/else chain that lived in mini.zig (lines 137-222).
pub fn interpretKeypress(key: vaxis.Key, m: Mode) Action {
    // ESC: exit insert mode; do nothing in normal mode
    if (key.codepoint == vaxis.Key.escape) {
        if (m == .insert) return .{ .change_mode = .normal };
        return .no_op;
    }
    // quit: q/Q in normal mode (no ctrl/alt)
    if (m == .normal and
        (key.codepoint == 'q' or key.codepoint == 'Q') and
        !key.mods.ctrl and !key.mods.alt)
    {
        return .quit;
    }
    // enter insert mode: i (normal mode only)
    if (m == .normal and key.codepoint == 'i' and
        !key.mods.ctrl and !key.mods.alt)
    {
        return .{ .change_mode = .insert };
    }
    // scroll down: j/J/down (no ctrl/alt)
    if ((key.codepoint == 'j' or key.codepoint == 'J' or
        key.codepoint == vaxis.Key.down) and !key.mods.ctrl and !key.mods.alt)
    {
        return .{ .scroll_view = 1 };
    }
    // scroll up: k/K/up (no ctrl/alt)
    if ((key.codepoint == 'k' or key.codepoint == 'K' or
        key.codepoint == vaxis.Key.up) and !key.mods.ctrl and !key.mods.alt)
    {
        return .{ .scroll_view = -1 };
    }
    // ctrl+arrows (no shift): move cursor + reset selection
    if (key.mods.ctrl and !key.mods.shift and isArrow(key.codepoint)) {
        return .{ .move_cursor = .{ .dir = dirFromKey(key.codepoint), .extend_selection = false } };
    }
    // shift+ctrl+arrows: move cursor + update selection
    if (key.mods.ctrl and key.mods.shift and isArrow(key.codepoint)) {
        return .{ .move_cursor = .{ .dir = dirFromKey(key.codepoint), .extend_selection = true } };
    }
    // paste: ctrl+y
    if (key.matches('y', .{ .ctrl = true })) {
        return .paste_selection;
    }
    // undo: ctrl+u
    if (key.matches('u', .{ .ctrl = true })) {
        return .undo;
    }
    // page up (plain or ctrl, no shift)
    if (key.codepoint == vaxis.Key.page_up and !key.mods.shift) {
        return .{ .page_up = .{ .extend_selection = false } };
    }
    // shift+ctrl+page_up
    if (key.codepoint == vaxis.Key.page_up and key.mods.ctrl and key.mods.shift) {
        return .{ .page_up = .{ .extend_selection = true } };
    }
    // page down (plain or ctrl, no shift)
    if (key.codepoint == vaxis.Key.page_down and !key.mods.shift) {
        return .{ .page_down = .{ .extend_selection = false } };
    }
    // shift+ctrl+page_down
    if (key.codepoint == vaxis.Key.page_down and key.mods.ctrl and key.mods.shift) {
        return .{ .page_down = .{ .extend_selection = true } };
    }
    // ctrl+home (no shift)
    if (key.codepoint == vaxis.Key.home and key.mods.ctrl and !key.mods.shift) {
        return .{ .cursor_home = .{ .extend_selection = false } };
    }
    // shift+ctrl+home
    if (key.codepoint == vaxis.Key.home and key.mods.ctrl and key.mods.shift) {
        return .{ .cursor_home = .{ .extend_selection = true } };
    }
    // ctrl+end (no shift)
    if (key.codepoint == vaxis.Key.end and key.mods.ctrl and !key.mods.shift) {
        return .{ .cursor_end = .{ .extend_selection = false } };
    }
    // shift+ctrl+end
    if (key.codepoint == vaxis.Key.end and key.mods.ctrl and key.mods.shift) {
        return .{ .cursor_end = .{ .extend_selection = true } };
    }
    // insert text (insert mode)
    if (m == .insert and key.text != null) {
        return .{ .insert_text = key.text.? };
    }
    return .no_op;
}

/// Dispatch an Action to the Document. Returns `true` when the editor should quit.
pub fn applyAction(dc: *doc.Document, action: Action, mode: *Mode) !bool {
    switch (action) {
        .no_op => return false,
        .quit => return true,
        .change_mode => |new_mode| {
            mode.* = new_mode;
            return false;
        },
        .move_cursor => |mc| {
            dc.moveCursor(mc.dir);
            if (mc.extend_selection) {
                dc.cursor.selection.head = dc.cursor.pos;
            } else {
                dc.cursor.selection = doc.Selection.emptySel(dc.cursor.pos);
            }
            return false;
        },
        .scroll_view => |dy| {
            dc.moveView(dy);
            return false;
        },
        .page_up => |pu| {
            dc.cursorPgUp();
            if (pu.extend_selection) dc.cursor.selection.head = dc.cursor.pos;
            return false;
        },
        .page_down => |pd| {
            dc.cursorPgDn();
            if (pd.extend_selection) dc.cursor.selection.head = dc.cursor.pos;
            return false;
        },
        .cursor_home => |ch| {
            dc.cursorHome();
            if (ch.extend_selection) dc.cursor.selection.head = dc.cursor.pos;
            return false;
        },
        .cursor_end => |ce| {
            dc.cursorEnd();
            if (ce.extend_selection) dc.cursor.selection.head = dc.cursor.pos;
            return false;
        },
        .insert_text => |text| {
            try dc.insertAtCursor(text);
            return false;
        },
        .paste_selection => {
            try dc.pasteSelection();
            return false;
        },
        .undo => {
            dc.undo();
            return false;
        },
    }
}

/// true if codepoint is one of the four arrow keys
fn isArrow(cp: u21) bool {
    return cp == vaxis.Key.up or cp == vaxis.Key.down or
        cp == vaxis.Key.left or cp == vaxis.Key.right;
}

/// map an arrow key codepoint to a Direction
fn dirFromKey(cp: u21) doc.Direction {
    return switch (cp) {
        vaxis.Key.up => .up,
        vaxis.Key.down => .down,
        vaxis.Key.left => .left,
        vaxis.Key.right => .right,
        else => unreachable,
    };
}
