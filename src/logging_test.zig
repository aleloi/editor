// const std = @import("std");
const logging = @import("logging.zig");
const logger = logging.default_logger;
pub const std_options = logging.std_options;
pub const panic = logging.panic;

fn f1() void {
    logger.debug("f1 called!", .{});
    f2() catch |err| logging.logErrorFmt(err, "whoopsie {s}", .{"woop"}) catch {};
}

fn f2() !void {
    logger.debug("f2 called!", .{});
    return error.TestError;
}

fn f3() !void {
    logger.debug("f3 called!", .{});
    f2() catch |err| return logging.logErrorFmt(err, "(unsuppressed) whoopsie {s}", .{"woop"});
}

pub fn main() !void {
    try logging.loggerInit(null);
    logger.debug("test debug messsage", .{});
    logger.info("test info messsage", .{});
    logger.warn("test warning messsage", .{});
    logger.err("test error messsage", .{});

    f1();
    // try f3();
    logging.panicFmt("panic msg {any}", .{&".{ 555, 33 }"});
}
