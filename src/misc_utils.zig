//! miscellaneous utils

// number of digits in decimal representation
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

/// the number of digits in the decimal representation of the maximum usize
const max_usize_digits: usize = numDigits(@subWithOverflow(@as(usize, 0), @as(usize, 1))[0]);
/// array with enough spaces to pad to the width of a usize
pub const spaces: [max_usize_digits + 2]u8 = .{' '} ** (max_usize_digits + 2);
