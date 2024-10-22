//! miscellaneous utils

/// unpack and return error on null
pub fn up(maybe: anytype) !@TypeOf(maybe orelse unreachable) {
    return maybe orelse error.Null;
}

/// cast usize to isize
pub fn isz(arg: usize) isize {
    return @intCast(arg);
}

// number of digits in decimal representation of arg
pub fn numDigits(arg: anytype) usize {
    if (arg == 0) return 1;

    var count: usize = 0;
    var num = arg;

    while (num != 0) {
        num /= 10;
        count += 1;
    }

    return count;
}

// https://ziglang.org/documentation/master/std/#std.math.maxInt
// https://ziglang.org/documentation/master/std/#std.math.minInt
/// maximum usize
pub const max_usize: usize = @subWithOverflow(@as(usize, 0), @as(usize, 1))[0];
/// the number of digits in the decimal representation of the maximum usize
const max_usize_digits: usize = numDigits(max_usize);
/// array with enough spaces to pad to the width of a usize
pub const spaces: [max_usize_digits + 2]u8 = .{' '} ** (max_usize_digits + 2);
