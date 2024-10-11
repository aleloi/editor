//! logging
const std = @import("std");
const expect = std.testing.expect;
const print = std.debug.print;

const time_utils = @import("time_utils.zig");

pub const std_options = .{
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

fn getFileHandle(name: []const u8) !std.fs.File {
    const dir: std.fs.Dir = std.fs.cwd();

    return try dir.createFile(name, .{ .truncate = false });
}
var handle: std.fs.File = undefined;
var file_writer: ?@TypeOf(handle.writer()) = null;

pub fn myLogFn(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.EnumLiteral),
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
    // Print the message to stderr, silently ignoring any errors
    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();
    const stderr = std.io.getStdErr().writer();
    time_utils.writeTimestamp(time_utils.now(), stderr) catch return;
    stderr.print(prefix ++ format ++ "\n", args) catch return;
    time_utils.writeTimestamp(time_utils.now(), file_writer orelse return) catch return;
    (file_writer orelse return).print(prefix ++ format ++ "\n", args) catch return;
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

/// default logger
///
/// IMPORTANT: needs ```pub const std_options = logging.std_options;``` in root file to work as intended
///
pub const default_logger = std.log;

// pub fn get_logger(comptime scope: ?@TypeOf(.EnumLiteral), file: ?[]const u8) !@TypeOf(std.log.scoped(scope orelse std.log.default_log_scope)) {
//     handle = getFileHandle(file orelse "app.log") catch unreachable;
//     defer handle.close();
//     try handle.seekFromEnd(0);
//     file_writer = handle.writer();

//     // return std.log.scoped(scope orelse std.log.default_log_scope);
// }

pub fn logger_init(file: ?[]const u8) !void {
    handle = getFileHandle(file orelse "app.log") catch unreachable;
    // defer handle.close();
    try handle.seekFromEnd(0);
    file_writer = handle.writer();

    default_logger.debug("logger initialized!", .{});
    // return std.log.scoped(scope orelse std.log.default_log_scope);
}

pub fn logger_deinit() void {
    // file_writer
    // handle = getFileHandle(file orelse "app.log") catch unreachable;

    defer handle.close();
    // try handle.seekFromEnd(0);
    // file_writer = handle.writer();

    // return std.log.scoped(scope orelse std.log.default_log_scope);
}
