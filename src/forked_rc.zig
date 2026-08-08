const std = @import("std");
const builtin = @import("builtin");

// I think this works as long as gpa has static lifetime.
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const gpa_alloc = gpa.allocator();

/// LEAKS memory, user of this  module must deinit.
var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
const arena_alloc = arena.allocator();

// Doesn't work, can't have references to 'comptime var' in a comptime
// value. I think this means that the resulting value refers to a var
// which doesn't have static lifetime.

// fn comptime_gpa() std.mem.Allocator {
//     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     return gpa.allocator();
// }



/// A single threaded, strong reference to a reference-counted value.
pub fn Rc(comptime T: type, arg_alloc: std.mem.Allocator) type {
    return struct {
        //const alloc: std.mem.Allocator = comptime_alloc;
        value: *T,

        const Self = @This();

        /// Unsafe: you are only allowed to use one type of
        /// allocator in all places that link wtih this.
        const alloc: std.mem.Allocator = arg_alloc;

        const Inner = struct {
            strong: usize,
            value: T,
        };

        /// Creates a new reference-counted value.
        pub fn init(t: T) std.mem.Allocator.Error!Self {
            //init_alloc.vtable.alloc
            //std.debug.assert(Self.alloc == null or Self.alloc.?.vtable.alloc == init_alloc.vtable.alloc );
            //Self.alloc = init_alloc;
            const inner = try alloc.create(Inner);
            inner.* = .{.value=t, .strong=1};
            return Self {.value = &inner.*.value};
        }

        /// Gets the number of strong references to this value.
        pub fn strongCount(self: *const Self) usize {
            return self.innerPtr().strong;
        }

        /// Increments the strong count.
        pub fn retain(self: *Self) Self {
            self.innerPtr().strong += 1;
            return self.*;
        }

        /// Decrements the reference count, deallocating if the weak count reaches zero.
        /// The continued use of the pointer after calling `release` is undefined behaviour.
        fn release(self: Self) void {
            const ptr = self.innerPtr();

            ptr.strong -= 1;
            if (ptr.strong == 0) {
                alloc.destroy(ptr);
            }
        }

        /// Decrements the reference count, deallocating the weak count reaches zero,
        /// and executing `f` if the strong count reaches zero.
        /// The continued use of the pointer after calling `release` is undefined behaviour.
        pub fn releaseWithFn(self: Self, comptime f: fn (T) void) void {
            const ptr = self.innerPtr();

            ptr.strong -= 1;
            if (ptr.strong == 0) {
                f(self.value.*);

                alloc.destroy(ptr);
            }
        }

        inline fn innerPtr(self: *const Self) *Inner {
            return @alignCast(@fieldParentPtr("value", self.value));
        }

    };
}

test "create with test alloc" {
    const rc = try Rc(u8, std.testing.allocator).init(0);
    defer rc.release();
}

test "create with page alloc" {
    const rc = try Rc(u8, std.heap.page_allocator).init(0);
    defer rc.release();
}

test "create with gpa alloc" {
    const rc = try Rc(u8, gpa_alloc).init(0);
    defer rc.release();
}

test "create with arena alloc" {
    const rc = try Rc(u8, arena_alloc).init(0);
    defer rc.release();
}

