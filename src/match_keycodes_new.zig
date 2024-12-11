//! contains lists of keycode strings
//! and the public functions
//!
//! match_ascii, matchXterm, matchVt, matchVtOldScheme

const std = @import("std");
const ku = @import("key_utils.zig");

pub const MatchError = error{
    GenericMatchError,
    ASCIIMatchError,
    XtermMatchError,
    VtMatchError,
    VtOldMatchError,
    Ss3MatchError,
};

/// tries to match the string in bytes to a string in codes.
/// prints using key_press if successful,
/// otherwise returns MatchError
fn genericMatchCode(bytes: []const u8, codes: []const []const u8, keys: []const []const u8, key_press: *ku.KeyPress) !void {
    for (codes, keys) |code, key| {
        if (std.mem.eql(u8, bytes, code)) {
            key_press.key = key;
            // try key_press.writeAll(key);
            return;
        }
    }
    return MatchError.GenericMatchError;
}

/// tries to map unprintable ascii -> [key]
///
/// unprintable := 0...32, 127, we don't recognize 128...255
pub fn match_ascii(byte: u8, key_press: *ku.KeyPress) !void {
    const codes: [33][]const u8 = .{
        "\x00",
        "\x01",
        "\x02",
        "\x03",
        "\x04",
        "\x05",
        "\x06",
        "\x07",
        "\x08",
        "\x09",
        "\x0A",
        "\x0B",
        "\x0C",
        "\x0D",
        "\x0E",
        "\x0F",
        "\x10",
        "\x11",
        "\x12",
        "\x13",
        "\x14",
        "\x15",
        "\x16",
        "\x17",
        "\x18",
        "\x19",
        "\x1A",
        "\x1B",
        "\x1C",
        "\x1D",
        "\x1E",
        "\x1F",
        "\x7f",
    };
    const keys: [33]ku.Key = .{
        .{ .ascii = 0 },
        .{ .ascii = 1 },
        .{ .ascii = 2 },
        .{ .ascii = 3 },
        .{ .ascii = 4 },
        .{ .ascii = 5 },
        .{ .ascii = 6 },
        .{ .ascii = 7 },
        .{ .ascii = 8 },
        .{ .ascii = 9 },
        .{ .ascii = 10 },
        .{ .ascii = 11 },
        .{ .ascii = 12 },
        .{ .ascii = 13 },
        .{ .ascii = 14 },
        .{ .ascii = 15 },
        .{ .ascii = 16 },
        .{ .ascii = 17 },
        .{ .ascii = 18 },
        .{ .ascii = 19 },
        .{ .ascii = 20 },
        .{ .ascii = 21 },
        .{ .ascii = 22 },
        .{ .ascii = 23 },
        .{ .ascii = 24 },
        .{ .ascii = 25 },
        .{ .ascii = 26 },
        .{ .ascii = 27 },
        .{ .ascii = 28 },
        .{ .ascii = 29 },
        .{ .ascii = 30 },
        .{ .ascii = 31 },
        .{ .ascii = 127 },
    };
    // const keys_text: [33][]const u8 = .{
    //     "CTRL+' / CTRL+` / CTRL+2 / CTRL+SPACE",
    //     "CTRL+a",
    //     "CTRL+b",
    //     "CTRL+c",
    //     "CTRL+d",
    //     "CTRL+e",
    //     "CTRL+f",
    //     "CTRL+g",
    //     "ASCII-BS / CTRL+BACKSPACE / CTRL+h",
    //     "ASCII-HT / TAB / CTRL+i",
    //     "ASCII-LF / CTRL+j",
    //     "CTRL+k",
    //     "CTRL+l",
    //     "ASCII-CR / ENTER / CTRL+m",
    //     "CTRL+n",
    //     "CTRL+o",
    //     "CTRL+p",
    //     "CTRL+q",
    //     "CTRL+r",
    //     "CTRL+s",
    //     "CTRL+t",
    //     "CTRL+u",
    //     "CTRL+v",
    //     "CTRL+w",
    //     "CTRL+x",
    //     "CTRL+y",
    //     "CTRL+z",
    //     "ASCII-ESC / ESC / CTRL+8",
    //     "CTRL+< / CTRL+7",
    //     "CTRL+9 / CTRL+0",
    //     "CTRL+^",
    //     "CTRL+-",
    //     "ASCII-DEL / BACKSPACE / CTRL++",
    // };
    genericMatchCode(&[1]u8{byte}, &codes, &keys, key_press) catch {
        return MatchError.ASCIIMatchError;
    };
}

/// match xterm keycode
pub fn matchXterm(bytes: []const u8, key_press: *ku.KeyPress) !void {
    const codes: [14][]const u8 = .{
        "P",
        "Q",
        "R",
        "S",
        "1P",
        "1Q",
        "1R",
        "1S",
        "A",
        "B",
        "C",
        "D",
        "F",
        "H",
    };
    const keys: [14]ku.Key = .{
        .{ .fkey = 0 },
        .{ .fkey = 1 },
        .{ .fkey = 2 },
        .{ .fkey = 3 },
        .{ .fkey = 0 },
        .{ .fkey = 1 },
        .{ .fkey = 2 },
        .{ .fkey = 3 },
        ku.arrows[0],
        ku.arrows[1],
        ku.arrows[2],
        ku.arrows[3],
        ku.fn_arrows[2],
        ku.fn_arrows[3],
    };
    // const keys_text: [14][]const u8 = .{
    //     "F1",
    //     "F2",
    //     "F3",
    //     "F4",
    //     "F1",
    //     "F2",
    //     "F3",
    //     "F4",
    //     "UP",
    //     "DOWN",
    //     "RIGHT",
    //     "LEFT",
    //     "END",
    //     "HOME",
    // };
    genericMatchCode(bytes, &codes, &keys, key_press) catch {
        return MatchError.XtermMatchError;
    };
}

/// match vt keycode. that is, the keycode part of
///
/// [esc] '[' ([keycode]) (';'[modifier]) '~'
pub fn matchVt(bytes: []const u8, key_press: *ku.KeyPress) !void {
    const codes: [20][]const u8 = .{
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "11",
        "12",
        "13",
        "14",
        "15",
        "17",
        "18",
        "19",
        "20",
        "21",
        "23",
        "24",
    };
    const keys: [20]ku.Key = .{
        ku.fn_arrows[2],
        .Insert,
        .Delete,
        ku.fn_arrows[3],
        ku.fn_arrows[0],
        ku.fn_arrows[1],
        ku.fn_arrows[2],
        ku.fn_arrows[3],
        .{ .fkey = 1 },
        .{ .fkey = 2 },
        .{ .fkey = 3 },
        .{ .fkey = 4 },
        .{ .fkey = 5 },
        .{ .fkey = 6 },
        .{ .fkey = 7 },
        .{ .fkey = 8 },
        .{ .fkey = 9 },
        .{ .fkey = 10 },
        .{ .fkey = 11 },
        .{ .fkey = 12 },
    };
    // const keys_text: [20][]const u8 = .{
    //     "HOME",
    //     "INSERT",
    //     "DELETE",
    //     "END",
    //     "PGUP",
    //     "PGDN",
    //     "HOME",
    //     "END",
    //     "F1",
    //     "F2",
    //     "F3",
    //     "F4",
    //     "F5",
    //     "F6",
    //     "F7",
    //     "F8",
    //     "F9",
    //     "F10",
    //     "F11",
    //     "F12",
    // };
    genericMatchCode(bytes, &codes, &keys, key_press) catch {
        return MatchError.VtMatchError;
    };
}

/// match older vt keycode
pub fn matchOldSchemeVt(bytes: []const u8, key_press: *ku.KeyPress) !void {
    const codes: [5][]const u8 = .{
        "[A",
        "[B",
        "[C",
        "[D",
        "[E",
    };
    const keys: [5]ku.Key = .{
        .{ .fkey = 0 },
        .{ .fkey = 1 },
        .{ .fkey = 2 },
        .{ .fkey = 3 },
        .{ .fkey = 4 },
    };
    // const keys_text: [5][]const u8 = .{
    //     "F1",
    //     "F2",
    //     "F3",
    //     "F4",
    //     "F5",
    // };
    genericMatchCode(bytes, &codes, &keys, key_press) catch {
        return MatchError.VtOldMatchError;
    };
}

pub fn match_Ss3(bytes: []const u8, key_press: *ku.KeyPress) !void {
    const codes: [8][]const u8 = .{
        "P",
        "Q",
        "R",
        "S",
        "A",
        "B",
        "C",
        "D",
    };
    const keys: [8]ku.Key = .{
        .{ .fkey = 0 },
        .{ .fkey = 1 },
        .{ .fkey = 2 },
        .{ .fkey = 3 },
        ku.arrows[0],
        ku.arrows[1],
        ku.arrows[2],
        ku.arrows[3],
    };
    // const keys_text: [8][]const u8 = .{
    //     "F1",
    //     "F2",
    //     "F3",
    //     "F4",
    //     "UP",
    //     "DOWN",
    //     "RIGHT",
    //     "LEFT",
    // };
    genericMatchCode(bytes, &codes, &keys, key_press) catch {
        return MatchError.Ss3MatchError;
    };
}

// TODO add tests
