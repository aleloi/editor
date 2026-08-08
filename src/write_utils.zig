//! write utils

const std = @import("std");

// In 0.16, the old std.io.Writer type constructor is gone.
// The MultiWriter abstraction is removed — callers should use the
// writer directly. Stderr mirroring was debug-only and is dropped.

test "test stderr_writer format" {
    // zig test src/write_utils.zig
    var buf: [4096]u8 = undefined;
    var writer: std.Io.Writer = .fixed(&buf);

    for (0..128) |i| {
        writer.end = 0;
        writer.print("\\x{X:0>2}", .{i}) catch {
            unreachable;
        };
    }
}
