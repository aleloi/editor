//! decoding epoch time
const std = @import("std");
const testing = std.testing;
const time = std.time;
const epoch = time.epoch;

const print = std.debug.print;

/// {year, month, day, hours, minutes, seconds}, in UTC
pub fn getTuple(secs: u64) struct { u16, u4, u5, u5, u6, u6 } {
    const epoch_seconds = epoch.EpochSeconds{ .secs = secs };
    const epoch_day = epoch_seconds.getEpochDay();
    const day_seconds = epoch_seconds.getDaySeconds();
    const year_day = epoch_day.calculateYearDay();
    const month_day = year_day.calculateMonthDay();
    const year = year_day.year;
    const month = month_day.month.numeric();
    const day: u5 = month_day.day_index + 1;
    const hours = day_seconds.getHoursIntoDay();
    const minutes = day_seconds.getMinutesIntoHour();
    const seconds = day_seconds.getSecondsIntoMinute();
    return .{ year, month, day, hours, minutes, seconds };
}

/// YYYY-MM-DD hh:mm:ss, in UTC
pub fn writeTimestamp(secs: u64, writer: anytype) !void {
    try writer.print(
        "{any:0>4}-{any:0>2}-{any:0>2} {any:0>2}:{any:0>2}:{any:0>2}",
        getTuple(secs),
    );
}

/// write timestamp to buf
pub fn bufPrintTimestamp(secs: u64, buf: []u8) !void {
    var fbs = std.io.fixedBufferStream(buf);
    const writer = fbs.writer();
    try writeTimestamp(secs, writer);
}

/// write timestamp to stderr
pub fn printTimestamp(secs: u64) !void {
    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();
    const stderr = std.io.getStdErr().writer();
    try writeTimestamp(secs, stderr);
    try stderr.print("\n", .{});
}

/// get current epoch time in seconds. wrapper around @abs(std.time.timestamp())
pub fn now() u64 {
    return @abs(std.time.timestamp());
}

// pub fn main() !void {
//     try printTimestamp(now());

//     const test_tup_1: struct { u64, *const [19:0]u8 } = .{ 0, "1970-01-01 00:00:00" };
//     const test_tup_2: struct { u64, *const [19:0]u8 } = .{ 31535999, "1970-12-31 23:59:59" };
//     const test_tup_3: struct { u64, *const [19:0]u8 } = .{ 1622924906, "2021-06-05 20:28:26" };
//     const test_tup_4: struct { u64, *const [19:0]u8 } = .{ 1625159473, "2021-07-01 17:11:13" };

//     try testDecode(test_tup_1);
//     try testDecode(test_tup_2);
//     try testDecode(test_tup_3);
//     try testDecode(test_tup_4);
// }

fn testDecode(raw_and_expected: struct { u64, *const [19:0]u8 }) !void {
    print("\ntest decode...\n", .{});
    var ret_buf: [19]u8 = undefined;
    try bufPrintTimestamp(raw_and_expected[0], &ret_buf);
    try expectEqlPrint(u8, raw_and_expected[1][0..], &ret_buf);
}

fn expectEqlPrint(comptime T: type, a: []const T, b: []const T) !void {
    print("expected: {s}\nactual  : {s}\n", .{ a, b });
    if (std.mem.eql(T, a, b) == false) unreachable;
    print("passed!\n", .{});
}

fn expectPrint(expected: anytype, actual: anytype) !void {
    print("expected: {any}\nactual  : {any}\n", .{ expected, actual });
    testing.expectEqual(expected, actual) catch unreachable;
    print("passed!\n", .{});
}

test "epoch decoding" {
    const test_tup_1: struct { u64, *const [19:0]u8 } = .{ 0, "1970-01-01 00:00:00" };
    const test_tup_2: struct { u64, *const [19:0]u8 } = .{ 31535999, "1970-12-31 23:59:59" };
    const test_tup_3: struct { u64, *const [19:0]u8 } = .{ 1622924906, "2021-06-05 20:28:26" };
    const test_tup_4: struct { u64, *const [19:0]u8 } = .{ 1625159473, "2021-07-01 17:11:13" };

    try testDecode(test_tup_1);
    try testDecode(test_tup_2);
    try testDecode(test_tup_3);
    try testDecode(test_tup_4);
}
