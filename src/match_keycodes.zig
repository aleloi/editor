//! contains lists of keycode strings
//! and the public functions
//!
//! match_ascii, matchXterm, matchVt, matchVtOldScheme

const std = @import("std");

pub const MatchError = error{
    GenericMatchError,
    ASCIIMatchError,
    XtermMatchError,
    VtMatchError,
    VtOldMatchError,
    Ss3MatchError,
};

/// tries to match the string in bytes to a string in codes.
/// prints using writer if successful,
/// otherwise returns MatchError
fn genericMatchCode(bytes: []const u8, codes: []const []const u8, keys: []const []const u8, writer: anytype) !void {
    for (codes, keys) |code, key| {
        if (std.mem.eql(u8, bytes, code)) {
            try writer.writeAll(key);
            return;
        }
    }
    return MatchError.GenericMatchError;
}

/// tries to map unprintable ascii -> [key]
///
/// unprintable := 0...32, 127, we don't recognize 128...255
pub fn match_ascii(byte: u8, writer: anytype) !void {
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
    const keys: [33][]const u8 = .{
        "CTRL+' / CTRL+` / CTRL+2 / CTRL+SPACE",
        "CTRL+a",
        "CTRL+b",
        "CTRL+c",
        "CTRL+d",
        "CTRL+e",
        "CTRL+f",
        "CTRL+g",
        "ASCII-BS / CTRL+BACKSPACE / CTRL+h",
        "ASCII-HT / TAB / CTRL+i",
        "ASCII-LF / CTRL+j",
        "CTRL+k",
        "CTRL+l",
        "ASCII-CR / ENTER / CTRL+m",
        "CTRL+n",
        "CTRL+o",
        "CTRL+p",
        "CTRL+q",
        "CTRL+r",
        "CTRL+s",
        "CTRL+t",
        "CTRL+u",
        "CTRL+v",
        "CTRL+w",
        "CTRL+x",
        "CTRL+y",
        "CTRL+z",
        "ASCII-ESC / ESC / CTRL+8",
        "CTRL+< / CTRL+7",
        "CTRL+9 / CTRL+0",
        "CTRL+^",
        "CTRL+-",
        "ASCII-DEL / BACKSPACE / CTRL++",
    };
    genericMatchCode(&[1]u8{byte}, &codes, &keys, writer) catch {
        return MatchError.ASCIIMatchError;
    };
}

/// match xterm keycode
pub fn matchXterm(bytes: []const u8, writer: anytype) !void {
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
    const keys: [14][]const u8 = .{
        "F1",
        "F2",
        "F3",
        "F4",
        "F1",
        "F2",
        "F3",
        "F4",
        "UP",
        "DOWN",
        "RIGHT",
        "LEFT",
        "END",
        "HOME",
    };
    genericMatchCode(bytes, &codes, &keys, writer) catch {
        return MatchError.XtermMatchError;
    };
}

/// match vt keycode. that is, the keycode part of
///
/// [esc] '[' ([keycode]) (';'[modifier]) '~'
pub fn matchVt(bytes: []const u8, writer: anytype) !void {
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
    const keys: [20][]const u8 = .{
        "HOME",
        "INSERT",
        "DELETE",
        "END",
        "PGUP",
        "PGDN",
        "HOME",
        "END",
        "F1",
        "F2",
        "F3",
        "F4",
        "F5",
        "F6",
        "F7",
        "F8",
        "F9",
        "F10",
        "F11",
        "F12",
    };
    genericMatchCode(bytes, &codes, &keys, writer) catch {
        return MatchError.VtMatchError;
    };
}

/// match older vt keycode
pub fn matchOldSchemeVt(bytes: []const u8, writer: anytype) !void {
    const codes: [5][]const u8 = .{
        "[A",
        "[B",
        "[C",
        "[D",
        "[E",
    };
    const keys: [5][]const u8 = .{
        "F1",
        "F2",
        "F3",
        "F4",
        "F5",
    };
    genericMatchCode(bytes, &codes, &keys, writer) catch {
        return MatchError.VtOldMatchError;
    };
}

pub fn match_Ss3(bytes: []const u8, writer: anytype) !void {
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
    const keys: [8][]const u8 = .{
        "F1",
        "F2",
        "F3",
        "F4",
        "UP",
        "DOWN",
        "RIGHT",
        "LEFT",
    };
    genericMatchCode(bytes, &codes, &keys, writer) catch {
        return MatchError.Ss3MatchError;
    };
}

// TODO add tests
