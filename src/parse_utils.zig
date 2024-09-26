//! parse_utils, parsing terminal input sequences
const std = @import("std");
const print = std.debug.print;
const panic = std.debug.panic;

// the functions that match keycodes to keys
const match_keycodes = @import("match_keycodes.zig");

// import byte format function
const format = @import("format.zig");
const myFmtBytes = format.myFmtBytes;

/// something went wrong while parsing
const ParseError = error{
    ParseBase,
} || ParseModiferError || MatchError;
/// something went wrong while parsing the modifier keys
const ParseModiferError = error{ ParseModifierOther, NoModifiers, ValueError };
/// match failure
const MatchError = match_keycodes.MatchError;

/// Control Sequence Introducer string
const CSI: [2]u8 = .{ '\x1B', '[' };

// const SliceIter = struct {
//     slices: []const []u8,
//     slice: usize = 0,
//     pos: usize = 0,
//     fn next(self: *@This()) ?usize {
//         if (self.slice < self.slices.len) {
//             if (self.pos < self.slices[self.slice].len) {
//                 self.pos += 1;
//                 return self.slices[self.slice][self.pos - 1];
//             }
//             self.slice += 1;
//             self.pos = 0;
//             return self.next();
//         }
//     }
// };

/// given sequence CSI [seq1] CSI [seq2] CSI [seq3] ...
///
/// yield CSI [seq1], CSI [seq2], ...
const CsiIterator = struct {
    data: []const u8,
    fn next(self: *@This()) ?[]const u8 {
        if (self.data.len == 0) return null;
        var idx: usize = 0;
        var ret: ?[]const u8 = null;
        while (true) {
            idx += 1;
            if (idx > 0 and idx + 2 <= self.data.len) {
                if (std.mem.eql(u8, self.data[idx..(idx + 2)], &CSI)) {
                    ret = self.data[0..idx];
                    self.data = self.data[idx..];
                    return ret;
                }
            }
            if (idx + 2 >= self.data.len) {
                ret = self.data[0..];
                self.data = self.data[self.data.len..];
                return ret;
            }
        }
    }
};

/// interprets a modifier byte
fn interpretModifier(byte: u8, writer: anytype) !void {
    // print("interpretModifier {any}\n", .{byte});
    if (byte < 1 or byte > 16) {
        if (byte > 48) { //    subtract '0'
            return interpretModifier(byte - 48, writer);
        } else return ParseModiferError.ValueError;
    }
    // modifier bitmap
    const bmap: u8 = byte - 1;
    if (bmap & 1 != 0) try writer.writeAll("SHIFT+");
    if (bmap & 2 != 0) try writer.writeAll("ALT+");
    if (bmap & 4 != 0) try writer.writeAll("CTRL+");
    if (bmap & 8 != 0) try writer.writeAll("META+");
    // function shouldnt be called if no modifiers are active
    if (bmap == 0) return ParseModiferError.NoModifiers;
}

/// [esc] '[' ([keycode]) (';'[modifier]) '~'.
///
/// only ([keycode]) (';'[modifier]) remains
/// split and process the two parts
fn processVtSequence(bytes: []const u8, writer: anytype) !void {
    // print("processVtSequence {any}\n", .{bytes});
    var maybe_keycode: ?[]const u8 = null;
    var maybe_modifier: ?[]const u8 = null;
    var split_it = std.mem.splitSequence(u8, bytes, ";");
    maybe_keycode = split_it.next();
    maybe_modifier = split_it.next();
    // print("maybe_keycode {any}\n", .{maybe_keycode});
    // print("maybe_modifier {any}\n", .{maybe_modifier});
    if (maybe_modifier) |modifier| {
        if (modifier.len != 1) {
            return ParseError.ParseBase;
        } else {
            try interpretModifier(modifier[0], writer);
        }
    }
    if (maybe_keycode) |keycode| {
        try match_keycodes.matchVt(keycode, writer);
    } else return ParseError.ParseBase;
}

/// splits the slice into its constituent input sequences
pub const InputSeqIterator = struct {
    bytes: []const u8,
    pub fn next(self: *@This()) !?[]const u8 {
        // if no bytes, finished iterating
        if (self.bytes.len == 0) return null;
        var ret: []const u8 = undefined;
        // if 1 bytes, return after updating bytes
        if (self.bytes.len == 1) {
            ret = self.bytes;
            self.bytes = self.bytes[1..];
            // print("\nyield 1 byte {any}\n", .{ret});
            return ret;
        }
        if (self.bytes[0] != '\x1B') panic("expected bytes {any} to start with 33\n", .{self.bytes});
        if (self.bytes.len == 2) {
            ret = self.bytes;
            self.bytes = self.bytes[2..];
            // print("\nyield 2 bytes {any}\n", .{ret});
            return ret;
        }
        // if 3+ bytes, should start with CSI, or is SS3 seq
        if (self.bytes[1] == 'O') {
            // SS3, length 3 bytes
            ret = self.bytes[0..3];
            self.bytes = self.bytes[3..];
            // print("\nyield 3 bytes {any}\n", .{ret});
            return ret;
        }
        if (self.bytes[1] == '[') {
            // CSI, variable length
            var index: usize = 2;
            while (index < self.bytes.len) {
                if (64 <= self.bytes[index] and self.bytes[index] <= 126) {
                    ret = self.bytes[0..(index + 1)];
                    self.bytes = self.bytes[(index + 1)..];
                    // print("\nyield {} bytes {any}\n", .{ index + 1, ret });
                    return ret;
                }
                index += 1;
            }
            panic("CSI sequence {any} not terminated\n", .{self.bytes});
        }
        panic("3+ bytes {any} but not CSI or SS3\n", .{self.bytes});
    }
};

/// try to parse terminal input sequence
/// https://en.wikipedia.org/wiki/ANSI_escape_code#Terminal_input_sequences
fn parseInputBytes(bytes: []const u8, writer: anytype) !void {
    // std.debug.print("\nparseInputBytes called with bytes: ", .{});
    // for (bytes) |byte| {
    //     std.debug.print(" \\x{X:0>2} ", .{byte});
    // }
    // std.debug.print("\n", .{});
    switch (bytes.len) {
        0 => {
            return ParseError.ParseBase;
        },
        1 => {
            switch (bytes[0]) {
                // printable ascii
                32...126 => |byte| try writer.writeByte(byte),
                // low ascii + 127
                0...31, 127 => |byte| {
                    match_keycodes.match_ascii(byte, writer) catch |err| {
                        switch (@TypeOf(err)) {
                            MatchError => try writer.print("{s}", .{myFmtBytes(&[1]u8{byte})}),
                            else => unreachable,
                        }
                    };
                },
                else => {
                    return ParseError.ParseBase;
                },
            }
        },
        2 => {
            if (bytes[0] != '\x1B') return ParseError.ParseBase;
            try writer.writeAll("ALT+"); //\"");
            try parseInputBytes(bytes[1..], writer);
            // try writer.writeAll("\"");
        },
        else => {
            if (std.mem.eql(u8, bytes[0..2], &.{ '\x1B', 'O' })) {
                // SS3 (Single Shift 3) sequences '\x1B O <keycode>'
                return match_keycodes.match_Ss3(bytes[2..], writer);
            }
            if (bytes.len >= 6 and std.mem.eql(u8, bytes[0..4], &.{ '\x1B', '[', '1', ';' })) {
                // CSI 1 ; <modifier> <keycode>
                // xterm sequence with modifier
                try interpretModifier(bytes[4], writer);
                return match_keycodes.matchXterm(bytes[5..], writer);
            }
            // we should not have recieved so many bytes
            // if they are not an escape code
            // so the first bytes should be [esc] '['
            if (!std.mem.eql(u8, bytes[0..2], &CSI)) return ParseError.ParseBase;
            if (bytes[0] != '\x1B' or bytes[1] != '[') {
                return ParseError.ParseBase;
            }
            if (bytes[bytes.len - 1] == '~') {
                // vt sequence
                // [esc] '[' ([keycode]) (';'[modifier]) '~'
                try processVtSequence(bytes[2..(bytes.len - 1)], writer);
            } else {
                if (bytes[2] == '[') {
                    // older vt sequence
                    try match_keycodes.matchOldSchemeVt(bytes[2..], writer);
                } else {
                    // xterm sequence
                    // [esc] '[' ([modifier]) [char]
                    // the terminal seems to ignore modifiers,
                    //  and so we ignore them
                    try match_keycodes.matchXterm(bytes[2..], writer);
                }
            }
        },
    }
}

pub fn parseWrite(raw: []const u8, writer: anytype) !void {
    // std.debug.print("Input sequence parse attempt: ", .{});
    if (raw.len > 0) {
        parseInputBytes(raw, writer) catch {
            try writer.print("Readable bytes: {s}", .{myFmtBytes(raw)});
        };
    } else {
        try writer.print("Readable bytes: {s}", .{myFmtBytes(raw)});
    }
    // std.debug.print("\n", .{});
}

pub fn rawWrite(bytes: []const u8, writer: anytype) !void {
    for (bytes) |byte| {
        switch (byte) {
            32...126 => try writer.print(" \" {c} \"", .{byte}),
            else => try writer.print(" \" \\x{X:0>2} \"", .{byte}),
        }
    }
}

test "test parse bytes 0..128" {
    // zig test src/parse_utils.zig
    const write_utils = @import("write_utils.zig");
    const stderr_writer = write_utils.stderr_writer;

    for (0..128) |i| {
        try parseWrite(&[1]u8{@as(u8, @truncate(i))}, stderr_writer);
    }
}

test "test parse bytes 27(ALT/ESC/...) + 0..128" {
    // zig test src/parse_utils.zig
    const write_utils = @import("write_utils.zig");
    const stderr_writer = write_utils.stderr_writer;

    for (0..128) |i| {
        try parseWrite(&[2]u8{ 27, @as(u8, @truncate(i)) }, stderr_writer);
    }
}
