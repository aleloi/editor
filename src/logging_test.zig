const logging = @import("logging.zig");
const logger = logging.default_logger;
pub const std_options = logging.std_options;

pub fn main() !void {
    logger.debug("debug messsage", .{});
    logger.info("info messsage", .{});
    logger.warn("warning messsage", .{});
    logger.err("error messsage", .{});
}
