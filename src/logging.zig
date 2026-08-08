//! logging
const std = @import("std");
const builtin = @import("builtin");
const expect = std.testing.expect;

const time_utils = @import("time_utils.zig");

const mu = @import("misc_utils.zig");

var log_buf: [5000]u8 = undefined;
var log_writer: std.Io.Writer = .fixed(&log_buf);

pub const std_options: std.Options = .{
    // Set the log level to info
    .log_level = .debug,
    // Define logFn to override the std implementation
    .logFn = myLogFn,
};

// root file needs:
// const logging = @import("logging.zig");
// const logger = logging.default_logger;
// pub const std_options = logging.std_options;
// IMPORTANT, otherwise uses std log fn

// 2024-10-07 02:16:28 [debug    ]
// YYYY-MM-DD hh:mm:ss

/// default logger, loggerInit must be called before use
///
/// IMPORTANT: needs ```pub const std_options = logging.std_options;``` in root file to work as intended
///
pub const default_logger = std.log;

// # ANSI escape codes for colors
const ANSI_BLUE: [5]u8 = .{ '\x1B', '[', '3', '4', 'm' };
const ANSI_GREEN: [5]u8 = .{ '\x1B', '[', '3', '2', 'm' };
const ANSI_YELLOW: [5]u8 = .{ '\x1B', '[', '3', '3', 'm' };
const ANSI_RED: [5]u8 = .{ '\x1B', '[', '3', '1', 'm' };
const ANSI_MAGENTA: [5]u8 = .{ '\x1B', '[', '3', '5', 'm' };
const ANSI_RESET: [4]u8 = .{ '\x1B', '[', '0', 'm' };
const ANSI_BOLD: [4]u8 = .{ '\x1B', '[', '1', 'm' };

fn levelAsText(comptime level: std.log.Level) []const u8 {
    return switch (level) {
        .err => ANSI_RED ++ ANSI_BOLD ++ "error    " ++ ANSI_RESET,
        .warn => ANSI_YELLOW ++ ANSI_BOLD ++ "warning  " ++ ANSI_RESET,
        .info => ANSI_GREEN ++ ANSI_BOLD ++ "info     " ++ ANSI_RESET,
        .debug => ANSI_BLUE ++ ANSI_BOLD ++ "debug    " ++ ANSI_RESET,
    };
}

var log_fd: std.posix.fd_t = -1;

fn getFileHandle(name: []const u8) !std.posix.fd_t {
    return try std.posix.openat(std.posix.AT.FDCWD, name, .{
        .ACCMODE = .WRONLY,
        .CREAT = true,
        .APPEND = true,
    }, 0o664);
}

fn writeToFile(data: []const u8) void {
    if (log_fd < 0) return;
    _ = std.os.linux.write(log_fd, data.ptr, data.len);
}

pub fn myLogFn(
    comptime level: std.log.Level,
    comptime scope: @EnumLiteral(),
    comptime format: []const u8,
    args: anytype,
) void {
    // Ignore all non-error logging from sources other than
    // .my_project, .nice_library and the default
    // const scope_prefix = "(" ++ switch (scope) {
    //     .my_project, .nice_library, std.log.default_log_scope => @tagName(scope),
    //     else => if (@intFromEnum(level) <= @intFromEnum(std.log.Level.err))
    //         @tagName(scope)
    //     else
    //         return,
    // } ++ "): ";
    _ = scope;
    const scope_prefix = "";
    const prefix = " [" ++ comptime levelAsText(level) ++ "] " ++ scope_prefix;
    const new_format = prefix ++ format ++ "\n";
    // write to fbs
    log_writer.end = 0;
    time_utils.writeTimestamp(time_utils.now(), &log_writer) catch return;
    log_writer.print(new_format, args) catch return;
    // first print to file
    writeToFile(log_writer.buffered());

    // blk: {
    //     time_utils.writeTimestampNewline(time_utils.now(), file_writer orelse break :blk) catch break :blk;
    //     (file_writer orelse break :blk).print(new_format, args) catch break :blk;
    // }
    // Print the message to stderr, silently ignoring any errors
    var stderr_buf: [64]u8 = undefined;
    const stderr = std.debug.lockStderr(&stderr_buf);
    defer std.debug.unlockStderr();
    stderr.file_writer.interface.writeAll(log_writer.buffered()) catch {};
}

pub fn main() void {
    // Using the default scope:
    std.log.debug("A borderline useless debug log message", .{}); // Won't be printed as log_level is .info
    std.log.info("Flux capacitor is starting to overheat", .{});
    // Using scoped logging:
    const my_project_log = std.log.scoped(.my_project);
    const nice_library_log = std.log.scoped(.nice_library);
    const verbose_lib_log = std.log.scoped(.verbose_lib);
    my_project_log.debug("Starting up", .{}); // Won't be printed as log_level is .info
    nice_library_log.warn("Something went very wrong, sorry", .{});
    verbose_lib_log.warn("Added 1 + 1: {}", .{1 + 1}); // Won't be printed as it gets filtered out by our log function
}

/// init and return default_logger
pub fn getLogger(file: ?[]const u8) !@TypeOf(std.log) {
    // pub fn get_logger(comptime scope: ?@EnumLiteral(), file: ?[]const u8) !@TypeOf(std.log.scoped(scope orelse std.log.default_log_scope)) {
    log_fd = getFileHandle(file orelse "app.log") catch unreachable;
    defer std.os.linux.close(log_fd);
    default_logger.debug("logger initialized!", .{});
    return default_logger;
    // return std.log.scoped(scope orelse std.log.default_log_scope);
}

/// open file handle
pub fn loggerInit(file: ?[]const u8) !void {
    log_fd = getFileHandle(file orelse "app.log") catch unreachable;
    default_logger.debug("logger initialized!", .{});
    // return std.log.scoped(scope orelse std.log.default_log_scope);
}

/// close file handle
pub fn loggerDeinit() void {
    _ = std.os.linux.close(log_fd);
}

pub fn logErrorFmt(
    er: anyerror,
    comptime format: []const u8,
    args: anytype,
) @TypeOf(er) {
    const size = 0x1000;
    const trunc_msg = "(msg truncated)";
    var buf: [size + trunc_msg.len]u8 = undefined;
    // a minor annoyance with this is that it will result in the NoSpaceLeft
    // error being part of the @panic stack trace (but that error should
    // only happen rarely)
    const msg = std.fmt.bufPrint(buf[0..size], format, args) catch |err| switch (err) {
        error.NoSpaceLeft => blk: {
            @memcpy(buf[size..], trunc_msg);
            break :blk &buf;
        },
    };
    return logError(er, msg, @errorReturnTrace(), @returnAddress());
}

pub fn logError(er: anyerror, msg: []const u8, error_return_trace: ?*std.builtin.StackTrace, ret_addr: ?usize) @TypeOf(er) {
    _ = error_return_trace;
    var buf: [5000]u8 = undefined;
    var writer: std.Io.Writer = .fixed(&buf);

    blk: {
        writer.print("error: {s} {s}\n", .{ @errorName(er), msg }) catch break :blk;
        const terminal: std.Io.Terminal = .{ .writer = &writer, .mode = .escape_codes };
        if (builtin.strip_debug_info) {
            writer.print("Unable to dump stack trace: debug info stripped\n", .{}) catch break :blk;
            break :blk;
        }
        std.debug.writeCurrentStackTrace(.{
            .first_address = ret_addr orelse @returnAddress(),
        }, terminal) catch |err| {
            writer.print("Unable to dump stack trace: {s}\n", .{@errorName(err)}) catch break :blk;
            break :blk;
        };
    }

    default_logger.err("{s}", .{writer.buffered()});

    return er;
}

pub fn panicFmt(
    comptime format: []const u8,
    args: anytype,
) noreturn {
    const size = 0x1000;
    const trunc_msg = "(msg truncated)";
    var buf: [size + trunc_msg.len]u8 = undefined;
    // a minor annoyance with this is that it will result in the NoSpaceLeft
    // error being part of the @panic stack trace (but that error should
    // only happen rarely)
    const msg = std.fmt.bufPrint(buf[0..size], format, args) catch |err| switch (err) {
        error.NoSpaceLeft => blk: {
            @memcpy(buf[size..], trunc_msg);
            break :blk &buf;
        },
    };
    panic(msg, @returnAddress());
}

pub fn panic(msg: []const u8, first_trace_addr: ?usize) noreturn {
    var buf: [5000]u8 = undefined;
    var writer: std.Io.Writer = .fixed(&buf);

    blk: {
        writer.print("panic: {s}\n", .{msg}) catch break :blk;
        const terminal: std.Io.Terminal = .{ .writer = &writer, .mode = .escape_codes };
        if (builtin.strip_debug_info) {
            writer.print("Unable to dump stack trace: debug info stripped\n", .{}) catch break :blk;
            break :blk;
        }
        std.debug.writeCurrentStackTrace(.{
            .first_address = first_trace_addr orelse @returnAddress(),
        }, terminal) catch |err| {
            writer.print("Unable to dump stack trace: {s}\n", .{@errorName(err)}) catch break :blk;
            break :blk;
        };
    }

    default_logger.err("{s}", .{writer.buffered()});
    // _ = error_return_trace;
    // std.posix.exit(1);
    std.debug.defaultPanic(msg, first_trace_addr);
}
