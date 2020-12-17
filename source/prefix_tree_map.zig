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

        pub const FindResult = struct {
            parent: *const Self,
            index: usize,
        };

        pub fn find(self: *const Self, key: []const K) ?FindResult {
            if (key.len == 0) return null;

            var current = self;
            for (key[0 .. key.len - 1]) |part| {
                const part_index = current.indexOf(part) orelse return null;
                current = current.child(part_index) orelse return null;
            }

            const index = current.indexOf(key[key.len - 1]) orelse return null;
            return FindResult{ .parent = current, .index = index };
        }

        pub fn get(self: Self, key: []const K) V {
            return self.getFromFind(self.find(key) orelse unreachable);
        }

        pub fn getFromFind(self: Self, find_result: FindResult) V {
            return find_result.parent.value(find_result.index);
        }

        pub fn value(self: Self, index: usize) V {
            return self.values.items[index];
        }

        pub fn child(self: Self, index: usize) ?*Self {
            return self.child_nodes.items[index].s;
        }

        pub fn numChildren(self: Self) usize {
            return self.child_nodes.items.len;
        }

        pub fn exists(self: *const Self, key: []const K) bool {
            return self.find(key) != null;
        }

        pub fn insertChild(self: *Self, parent: []const K, k: K, v: V, allocator: *Allocator) !void {
            if (parent.len == 0) return insertChildRoot(self, k, v, allocator);

            const parent_elem = self.find(parent) orelse unreachable;
            const parent_node_ptr = &parent_elem.parent.child_nodes.items[parent_elem.index];
            const parent_node = if (parent_node_ptr.s) |n| n else blk: {
                const new_node = try allocator.create(Self);
                new_node.* = Self{};
                break :blk new_node;
            };
            errdefer if (parent_node_ptr.s == null) allocator.destroy(parent_node);

            const index = try parent_node.newEdge(k, v, allocator);
            errdefer parent_node.deleteNewEdge(index, allocator);
            parent_node_ptr.s = parent_node;
        }

        pub fn insertChildRoot(self: *Self, k: K, v: V, allocator: *Allocator) !void {
            _ = try self.newEdge(k, v, allocator);
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

        fn newEdge(self: *Self, k: K, v: V, allocator: *Allocator) !usize {
            const index = searchForInsertPosition(k, self.parts.items);
            try self.parts.insert(allocator, index, k);
            errdefer _ = self.parts.orderedRemove(index);
            try self.values.insert(allocator, index, v);
            errdefer _ = self.values.orderedRemove(index);
            try self.child_nodes.insert(allocator, index, .{ .s = null });
            errdefer _ = self.child_nodes.orderedRemove(index);
            return index;
        }

        /// Delete the most recent edge that was just created by newEdge
        fn deleteNewEdge(self: *Self, edge_index: usize, allocator: *Allocator) void {
            _ = self.parts.orderedRemove(edge_index);
            _ = self.values.orderedRemove(edge_index);
            _ = self.child_nodes.orderedRemove(edge_index);
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

    expect(!tree.exists(""));
    expect(!tree.exists("a"));
    try tree.insertChild("", 'a', -64, allocator);
    try tree.insertChild("", 'b', 100, allocator);
    try tree.insertChild("a", 'b', 42, allocator);
    expect(tree.exists("a"));
    expect(tree.exists("b"));
    expect(tree.exists("ab"));
    expect(!tree.exists("c"));
    expect(!tree.exists("ba"));
    expect(!tree.exists("abc"));
    expect(tree.get("a") == -64);
    expect(tree.get("b") == 100);
    expect(tree.get("ab") == 42);
}
