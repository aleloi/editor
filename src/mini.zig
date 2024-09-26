const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const os = std.os;
const linux = std.os.linux;
const posix = std.posix;
const print = std.debug.print;

const format = @import("format.zig");
const term_utils = @import("term_utils.zig");
const parse_utils = @import("parse_utils.zig");
const write_utils = @import("write_utils.zig");

var file_content: [5 * 1024 * 1024]u8 = undefined;
var bytes_read: usize = 0;
var lines_read: usize = 0;
var lines: [5 * 1024 * 1024][]const u8 = undefined;
/// index of first visible line
var first_line: usize = 0;
const bottom_ui_rows: usize = 2;
/// rows reserved for permanent ui
const non_content_rows = bottom_ui_rows;
/// rows available for file content
var content_rows: usize = undefined;

/// window dimensions
var size: Size = undefined;
var tty: fs.File = undefined;

// åäö
// zig run src/mini.zig < src/parse_utils.zig &> mini.log
// https://ziglang.org/documentation/master/std/#std.posix.poll
// https://chatgpt.com/share/43543411-1296-4086-990d-0df98b621321

fn get_writer(buf: []u8) std.io.GenericWriter {
    var fbs = std.io.fixedBufferStream(&buf);
    return fbs.writer();
}

var parse_buf: [1000]u8 = .{128} ** 1000;
var parse_fbs = std.io.fixedBufferStream(&parse_buf);
const parse_writer = parse_fbs.writer();
// try writer.print("bla", .{});
// print("{any}\n", .{fbs.getWritten()[3..]});
// fbs.reset();
// print("{s}\n", .{@typeName(@TypeOf(fbs.getWritten()))});

/// tries to match the slice needle to a slice in haystack.
fn genericMatch(needle: []const u8, haystack: []const []const u8) bool {
    for (haystack) |straw| {
        if (std.mem.eql(u8, needle, straw)) {
            return true;
        }
    }
    return false;
}

/// quit commands
const q_eq: [3][]const u8 = .{ "ESC", "Q", "q" };
/// down commands
const j_eq: [3][]const u8 = .{ "DOWN", "J", "j" };
/// up commands
const k_eq: [3][]const u8 = .{ "UP", "K", "k" };

pub fn main() !void {
    tty = try fs.cwd().openFile("/dev/tty", .{ .mode = .read_write });
    defer tty.close();

    try term_utils.uncook(tty);
    defer term_utils.cook(tty) catch {};

    try getInp();

    size = try getSize();

    var fds: [1]posix.pollfd = .{.{
        .fd = tty.handle,
        .events = posix.POLL.IN,
        .revents = undefined,
    }};

    try render(null);

    while (true) {
        // var buffer: [1]u8 = undefined;
        // _ = try tty.read(&buffer);

        _ = try posix.poll(&fds, -1);
        var buffer: [1000]u8 = .{255} ** 1000;

        const num_read = try tty.read(&buffer);
        if (num_read == 0) continue;

        print("\nall bytes {any}\n", .{buffer[0..num_read]});
        var cmd_it = parse_utils.InputSeqIterator{ .bytes = buffer[0..num_read] };

        while (try cmd_it.next()) |cmd| {
            print("\n single cmd {any}\n", .{cmd});
            parse_fbs.reset();
            try parse_utils.parseWrite(cmd, parse_writer);
            const cmd2: []const u8 = parse_fbs.getWritten();

            if (genericMatch(cmd2, &q_eq)) {
                return;
            } else if (genericMatch(cmd2, &j_eq)) {
                // next line
                if (lines_read + 20 > size.height and first_line < lines_read - 20) {
                    first_line += 1;
                }
            } else if (genericMatch(cmd2, &k_eq)) {
                // previous line
                if (first_line > 0) {
                    first_line -= 1;
                }
            }
            try render(cmd);
        }
    }
}

/// render the current view
fn render(maybe_bytes: ?[]const u8) !void {
    const tty_writer = tty.writer();
    var buf_writer = std.io.bufferedWriter(tty_writer);
    const writer = buf_writer.writer();

    try clear(writer);

    try render_file_content(writer);
    try render_bottom_ui(maybe_bytes, writer);
    _ = &maybe_bytes;

    try buf_writer.flush();
}

/// render file content
fn render_file_content(writer: anytype) !void {
    const last_line: usize = @min(first_line + content_rows, lines_read);
    for (first_line..last_line) |line_ind| {
        try writeLine(writer, lines[line_ind], line_ind - first_line);
    }
}

/// render bottom ui
fn render_bottom_ui(maybe_bytes: ?[]const u8, arg_writer: anytype) !void {
    // var rawbuf
    var multi_writer = write_utils.multiWriter(arg_writer);
    const writer = multi_writer.writer();
    if (maybe_bytes) |bytes| {
        try moveCursor(writer, size.height - 2, 0);
        try parse_utils.rawWrite(bytes, writer);
        try moveCursor(writer, size.height - 1, 0);
        try parse_utils.parseWrite(bytes, writer);
    }
}

/// write <txt> to the buffer at row y col 0, applying format.myFmtLine
fn writeLine(writer: anytype, txt: []const u8, y: usize) !void {
    try moveCursor(writer, y, 0);
    try writer.print("{}", .{format.myFmtLine(txt)});
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
    var win_size = mem.zeroes(linux.winsize);
    if (linux.ioctl(tty.handle, linux.T.IOCGWINSZ, @intFromPtr(&win_size)) != 0) {
        @panic("getsize failed ioctl()");
    }
    const height: usize = win_size.ws_row;
    // update number of rows available for content
    if (height < non_content_rows) unreachable;
    content_rows = win_size.ws_row - non_content_rows;
    return Size{
        .height = win_size.ws_row,
        .width = win_size.ws_col,
    };
}

/// read file content from stdin.
/// raise exception if longer than 5 MB.
fn getInp() !void {
    const stdin = std.io.getStdIn().reader();
    bytes_read = try stdin.readAll(&file_content);
    if (bytes_read == file_content.len) @panic("getInp file too long");
    var split_it = std.mem.splitSequence(u8, file_content[0..bytes_read], "\n");
    while (split_it.next()) |line| {
        lines[lines_read] = line;
        lines_read += 1;
    }
}

// this seems to ensure all tests are run
// when placed in the root file
test {
    std.testing.refAllDecls(@This());
    // or refAllDeclsRecursive
}
