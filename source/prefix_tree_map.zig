const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const assert = std.debug.assert;
const expect = std.testing.expect;

pub fn PrefixTreeMapUnmanaged(comptime K: type, comptime V: type, comptime cmpFn: fn (lhs: K, rhs: K) std.math.Order) type {
    return struct {
        const Self = @This();
        // https://github.com/ziglang/zig/issues/4562
        const S = struct { s: ?*Self };

        parts: ArrayListUnmanaged(K) = ArrayListUnmanaged(K){},
        values: ArrayListUnmanaged(V) = ArrayListUnmanaged(V){},
        child_nodes: ArrayListUnmanaged(S) = ArrayListUnmanaged(S){},

        pub fn init(allocator: *Allocator) !*Self {
            const result = try allocator.create(Self);
            errdefer allocator.destroy(result);
            result.* = Self{};
            return result;
        }

        fn clear(self: *Self, allocator: *Allocator) void {
            self.parts.deinit(allocator);
            self.values.deinit(allocator);
            self.child_nodes.deinit(allocator);
        }

        // FIXME recursive
        pub fn deinit(self: *Self, allocator: *Allocator) void {
            for (self.child_nodes.items) |child_node| {
                if (child_node.s) |node| node.deinit(allocator);
            }
            self.clear(allocator);
            allocator.destroy(self);
        }

        pub fn value(self: Self, index: usize) V {
            return self.values.items[index];
        }

        pub fn valuePtr(self: Self, index: usize) *V {
            return &self.values.items[index];
        }

        pub fn child(self: Self, index: usize) ?*Self {
            return self.child_nodes.items[index].s;
        }

        pub fn numChildren(self: Self) usize {
            return self.child_nodes.items.len;
        }

        pub fn hasPart(self: Self, part: K) bool {
            return self.indexOf(part) != null;
        }

        pub const FindResult = struct {
            parent: *const Self,
            index: usize,
            remaining: usize,

            pub fn wasFound(self: @This()) bool {
                return self.remaining == 0;
            }

            pub fn getFromFind(self: @This()) ?V {
                return if (self.wasFound())
                    self.parent.value(self.index)
                else
                    null;
            }

            pub fn getPtrFromFind(self: @This()) ?*V {
                return if (self.wasFound())
                    self.parent.valuePtr(self.index)
                else
                    null;
            }
        };

        pub fn find(self: *const Self, key: []const K) FindResult {
            assert(key.len > 0);
            var result = FindResult{
                .parent = undefined,
                .index = undefined,
                .remaining = key.len,
            };

            var next = self;
            for (key) |part| {
                const child_index = next.indexOf(part) orelse break;
                result = FindResult{
                    .parent = next,
                    .index = child_index,
                    .remaining = result.remaining - 1,
                };
                next = next.child(child_index) orelse break;
            }

            return result;
        }

        pub fn get(self: *const Self, key: []const K) ?V {
            return self.find(key).getFromFind();
        }

        pub fn exists(self: *const Self, key: []const K) bool {
            return self.find(key).wasFound();
        }

        /// Inserts a new key-value pair into the tree, filling any newly created intermediate
        /// key-value pairs with `filler`. If there was already a value associated with the key,
        /// it is returned.
        pub fn insert(self: *Self, allocator: *Allocator, key: []const K, val: V, filler: V) !?V {
            return insertWithFind(self, allocator, key, val, filler, self.find(key));
        }

        /// Inserts a new key-value pair into the tree, filling any newly created intermediate
        /// key-value pairs with `filler`. If there was already a value associated with the key,
        /// it is returned. A `FindResult` is used to skip traversing some of the tree.
        pub fn insertWithFind(self: *Self, allocator: *Allocator, key: []const K, val: V, filler: V, find_result: FindResult) !?V {
            if (find_result.wasFound()) {
                const ptr = find_result.parent.valuePtr(find_result.index);
                const old = ptr.*;
                ptr.* = val;
                return old;
            } else if (find_result.remaining == key.len) {
                try insertBranch(self, allocator, key, val, filler);
                return null;
            } else {
                try insertMaybeCreateChildNode(allocator, key, val, filler, find_result);
                return null;
            }
        }

        fn insertMaybeCreateChildNode(allocator: *Allocator, key: []const K, val: V, filler: V, find_result: FindResult) !void {
            assert(find_result.remaining != key.len);
            const child_node_ptr = &find_result.parent.child_nodes.items[find_result.index].s;
            if (child_node_ptr.*) |child_node| {
                try insertBranch(child_node, allocator, key[key.len - find_result.remaining ..], val, filler);
            } else {
                const child_node = try init(allocator);
                errdefer allocator.destroy(child_node);
                try insertBranch(child_node, allocator, key[key.len - find_result.remaining ..], val, filler);
                child_node_ptr.* = child_node;
            }
        }

        fn insertBranch(parent: *Self, allocator: *Allocator, key: []const K, val: V, filler: V) !void {
            if (key.len == 1) {
                return attachBranch(parent, allocator, key[0], val, null);
            } else {
                const branch = try newBranch(allocator, key[1..], val, filler);
                errdefer branch.deinit(allocator);
                return attachBranch(parent, allocator, key[0], filler, branch);
            }
        }

        fn attachBranch(parent: *Self, allocator: *Allocator, part: K, val: V, branch: ?*Self) !void {
            const index = try parent.newEdge(allocator, part, val);
            parent.child_nodes.items[index].s = branch;
        }

        fn indexOf(self: Self, part: K) ?usize {
            const cmp = struct {
                fn f(ctx: void, lhs: K, rhs: K) std.math.Order {
                    return cmpFn(lhs, rhs);
                }
            }.f;
            return std.sort.binarySearch(K, part, self.parts.items, {}, cmp);
        }

        /// Stolen from std.sort.binarySearch.
        fn searchForInsertPosition(
            key: K,
            parts: []const K,
        ) usize {
            var left: usize = 0;
            var right: usize = parts.len;

            while (left < right) {
                // Avoid overflowing in the midpoint calculation
                const mid = left + (right - left) / 2;
                // Compare the key with the midpoint element
                switch (cmpFn(key, parts[mid])) {
                    .eq => unreachable,
                    .gt => left = mid + 1,
                    .lt => right = mid,
                }
            }

            return left;
        }

        fn newEdge(self: *Self, allocator: *Allocator, part: K, val: V) !usize {
            assert(!self.hasPart(part)); // Edge already exists.
            const index = searchForInsertPosition(part, self.parts.items);
            try self.parts.insert(allocator, index, part);
            errdefer _ = self.parts.orderedRemove(index);
            try self.values.insert(allocator, index, val);
            errdefer _ = self.values.orderedRemove(index);
            try self.child_nodes.insert(allocator, index, .{ .s = null });
            errdefer _ = self.child_nodes.orderedRemove(index);
            return index;
        }

        fn newBranch(allocator: *Allocator, key: []const K, val: V, filler: V) !*Self {
            var top = try allocator.create(Self);
            top.* = Self{};
            errdefer top.deinit(allocator);
            _ = try top.newEdge(allocator, key[0], filler);

            var previous = top;
            for (key[1..]) |part| {
                var current = try allocator.create(Self);
                current.* = Self{};
                errdefer allocator.destroy(current);
                _ = try current.newEdge(allocator, part, filler);
                previous.child_nodes.items[0].s = current;
                previous = current;
            }

            previous.valuePtr(0).* = val;

            return top;
        }
    };
}

test "sample tree" {
    const cmpFn = (struct {
        fn f(lhs: u8, rhs: u8) std.math.Order {
            return std.math.order(lhs, rhs);
        }
    }).f;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    var tree = try PrefixTreeMapUnmanaged(u8, i32, cmpFn).init(allocator);
    defer tree.deinit(allocator);

    expect(!tree.exists("a"));
    expect((try tree.insert(allocator, "a", -64, 0)) == null);
    expect((try tree.insert(allocator, "b", 100, 0)) == null);
    expect((try tree.insert(allocator, "ab", 42, 0)) == null);
    expect(tree.exists("a"));
    expect(tree.exists("b"));
    expect(tree.exists("ab"));
    expect(!tree.exists("c"));
    expect(!tree.exists("ba"));
    expect(!tree.exists("abc"));
    expect(tree.get("a").? == -64);
    expect(tree.get("b").? == 100);
    expect(tree.get("ab").? == 42);
    expect((try tree.insert(allocator, "zyx", 1729, 1)) == null);
    expect(tree.exists("zyx"));
    expect(tree.exists("zy"));
    expect(tree.exists("z"));
    expect(tree.get("zyx").? == 1729);
    expect(tree.get("zy").? == 1);
    expect(tree.get("z").? == 1);
}
