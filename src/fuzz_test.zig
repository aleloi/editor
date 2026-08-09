const std = @import("std");
const action = @import("action.zig");
const doc = @import("document.zig");
const rope = @import("rope");

/// Generate a random `action.Action` from a `std.testing.Smith`.
///
/// `smith.value(T)` cannot produce slices (`[]const u8`) or `isize`/`usize`,
/// so `insert_text` and `scroll_view` are constructed manually.
fn smithAction(s: *std.testing.Smith, text_buf: []u8) action.Action {
    const tag = s.value(std.meta.Tag(action.Action));
    return switch (tag) {
        .no_op => .no_op,
        .quit => .quit,
        .change_mode => .{ .change_mode = s.value(action.Mode) },
        .move_cursor => .{ .move_cursor = .{
            .dir = s.value(doc.Direction),
            .extend_selection = s.value(bool),
        } },
        .scroll_view => .{ .scroll_view = @intCast(s.value(i64)) },
        .page_up => .{ .page_up = .{ .extend_selection = s.value(bool) } },
        .page_down => .{ .page_down = .{ .extend_selection = s.value(bool) } },
        .cursor_home => .{ .cursor_home = .{ .extend_selection = s.value(bool) } },
        .cursor_end => .{ .cursor_end = .{ .extend_selection = s.value(bool) } },
        .paste_selection => .paste_selection,
        .undo => .undo,
        .insert_text => blk: {
            const len = s.slice(text_buf);
            break :blk .{ .insert_text = text_buf[0..len] };
        },
    };
}

fn fuzzOne(_: void, smith: *std.testing.Smith) !void {
    const alloc = std.testing.allocator;

    const rp = try rope.Node.fromSlice("hello\nworld\n");
    defer rp.releaseWithFn(rope.Node.deinit);

    var dc = try doc.Document.init(alloc, 10, 20, rp);
    defer dc.deinit();

    var mode: action.Mode = .normal;
    var text_buf: [256]u8 = undefined;

    while (!smith.eos()) {
        const act = smithAction(smith, &text_buf);
        _ = try action.applyAction(&dc, act, &mode);
    }

    _ = try dc.getText();
}

test "fuzz: document actions" {
    try std.testing.fuzz({}, fuzzOne, .{});
}
