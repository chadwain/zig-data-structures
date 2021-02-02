const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const assert = std.debug.assert;
const expect = std.testing.expect;

fn FindResultType(comptime Node: type) type {
    return struct {
        parent: ?*const Node,
        index: usize,
        key: []const Node.Part,

        pub fn wasFound(self: @This()) bool {
            return self.key.len == 0;
        }

        pub fn get(self: @This()) ?Node.Value {
            return if (self.wasFound())
                self.parent.?.value(self.index)
            else
                null;
        }

        pub fn getPtr(self: @This()) ?*Node.Value {
            return if (self.wasFound())
                self.parent.?.valuePtr(self.index)
            else
                null;
        }
    };
}

pub fn PrefixTreeMapUnmanaged(comptime K: type, comptime V: type, comptime cmpFn: fn (lhs: K, rhs: K) std.math.Order) type {
    return struct {
        const Self = @This();
        pub const Part = K;
        pub const Value = V;
        pub const FindResult = FindResultType(Self);
        // https://github.com/ziglang/zig/issues/4562
        const S = struct { s: ?*Self };

        parts: ArrayListUnmanaged(K) = ArrayListUnmanaged(K){},
        values: ArrayListUnmanaged(V) = ArrayListUnmanaged(V){},
        child_nodes: ArrayListUnmanaged(S) = ArrayListUnmanaged(S){},

        fn clear(self: *Self, allocator: *Allocator) void {
            self.parts.deinit(allocator);
            self.values.deinit(allocator);
            self.child_nodes.deinit(allocator);
        }

        pub fn deinit(self: *Self, allocator: *Allocator, stack: []*Self) void {
            var count: usize = 0;
            for (self.child_nodes.items) |child_node, i| {
                if (child_node.s) |n| {
                    stack[count] = n;
                    count += 1;
                }
            }

            while (count > 0) {
                const node = stack[count - 1];
                count -= 1;
                for (node.child_nodes.items) |child_node| {
                    if (child_node.s) |n| {
                        stack[count] = n;
                        count += 1;
                    }
                }
                node.clear(allocator);
                allocator.destroy(node);
            }

            self.clear(allocator);
        }

        pub fn deinitRecursive(self: *Self, allocator: *Allocator) void {
            const impl = struct {
                fn f(node: *Self, a: *Allocator) void {
                    for (node.child_nodes.items) |child_node| {
                        if (child_node.s) |n| f(n, a);
                    }
                    node.clear(a);
                    a.destroy(node);
                }
            }.f;

            for (self.child_nodes.items) |child_node| {
                if (child_node.s) |n| {
                    impl(n, allocator);
                }
            }
            self.clear(allocator);
        }

        pub fn part(self: Self, index: usize) K {
            return self.parts.items[index];
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

        pub fn hasPart(self: Self, p: K) bool {
            return self.indexOf(p) != null;
        }

        pub fn find(self: *const Self, key: []const K) FindResult {
            assert(key.len > 0);
            var result = FindResult{
                .parent = null,
                .index = undefined,
                .key = key,
            };

            var next = self;
            for (key) |p| {
                const child_index = next.indexOf(p) orelse break;
                result = FindResult{
                    .parent = next,
                    .index = child_index,
                    .key = result.key[1..],
                };
                next = next.child(child_index) orelse break;
            }

            return result;
        }

        pub fn exists(self: *const Self, key: []const K) bool {
            return self.find(key).wasFound();
        }

        pub fn get(self: *const Self, key: []const K) ?V {
            return self.find(key).get();
        }

        pub fn getPtr(self: *const Self, key: []const K) ?*V {
            return self.find(key).getPtr();
        }

        /// Inserts a new key-value pair into the tree, filling any newly created intermediate
        /// key-value pairs with `filler`. If there was already a value associated with the key,
        /// it is returned.
        pub fn insert(self: *Self, allocator: *Allocator, key: []const K, val: V, filler: V) !?V {
            return insertWithFind(self, allocator, self.find(key), val, filler);
        }

        /// Inserts a new key-value pair into the tree, filling any newly created intermediate
        /// key-value pairs with `filler`. If there was already a value associated with the key,
        /// it is returned. A `FindResult` is used to skip traversing some of the tree.
        pub fn insertWithFind(self: *Self, allocator: *Allocator, find_result: FindResult, val: V, filler: V) !?V {
            if (find_result.wasFound()) {
                const ptr = find_result.getPtr().?;
                const old = ptr.*;
                ptr.* = val;
                return old;
            } else if (find_result.parent == null) {
                try insertBranch(self, allocator, find_result.key, val, filler);
                return null;
            } else {
                try insertMaybeCreateChildNode(allocator, find_result, val, filler);
                return null;
            }
        }

        fn insertMaybeCreateChildNode(allocator: *Allocator, find_result: FindResult, val: V, filler: V) !void {
            const child_node_ptr = &find_result.parent.?.child_nodes.items[find_result.index].s;
            if (child_node_ptr.*) |child_node| {
                try insertBranch(child_node, allocator, find_result.key, val, filler);
            } else {
                const child_node = try allocator.create(Self);
                errdefer allocator.destroy(child_node);
                child_node.* = Self{};
                try insertBranch(child_node, allocator, find_result.key, val, filler);
                child_node_ptr.* = child_node;
            }
        }

        fn insertBranch(parent: *Self, allocator: *Allocator, key: []const K, val: V, filler: V) !void {
            if (key.len == 1) {
                _ = try newEdge(parent, allocator, key[0], val, null);
            } else {
                const branch = try newBranch(allocator, key[1..], val, filler);
                errdefer deleteBranch(branch, allocator);
                _ = try newEdge(parent, allocator, key[0], filler, branch);
            }
        }

        fn indexOf(self: Self, p: K) ?usize {
            const cmp = struct {
                fn f(ctx: void, lhs: K, rhs: K) std.math.Order {
                    return cmpFn(lhs, rhs);
                }
            }.f;
            return std.sort.binarySearch(K, p, self.parts.items, {}, cmp);
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

        fn newEdge(self: *Self, allocator: *Allocator, p: K, val: V, child_node: ?*Self) !usize {
            assert(!self.hasPart(p)); // Edge already exists.
            const index = searchForInsertPosition(p, self.parts.items);
            try self.parts.insert(allocator, index, p);
            errdefer _ = self.parts.orderedRemove(index);
            try self.values.insert(allocator, index, val);
            errdefer _ = self.values.orderedRemove(index);
            try self.child_nodes.insert(allocator, index, .{ .s = child_node });
            errdefer _ = self.child_nodes.orderedRemove(index);
            return index;
        }

        fn newBranch(allocator: *Allocator, key: []const K, val: V, filler: V) !*Self {
            var top = try allocator.create(Self);
            top.* = Self{};
            errdefer deleteBranch(top, allocator);
            _ = try top.newEdge(allocator, key[0], filler, null);

            var previous = top;
            for (key[1..]) |p| {
                var current = try allocator.create(Self);
                current.* = Self{};
                errdefer allocator.destroy(current);
                _ = try current.newEdge(allocator, p, filler, null);
                previous.child_nodes.items[0].s = current;
                previous = current;
            }

            previous.valuePtr(0).* = val;
            return top;
        }

        fn deleteBranch(branch: *Self, allocator: *Allocator) void {
            var current = branch;
            while (current.child(0)) |next| {
                current.clear(allocator);
                allocator.destroy(current);
                current = next;
            }
        }
    };
}

test "sample tree" {
    const cmpFn = (struct {
        fn f(lhs: u8, rhs: u8) std.math.Order {
            return std.math.order(lhs, rhs);
        }
    }).f;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer expect(!gpa.deinit());
    const allocator = &gpa.allocator;

    const Tree = PrefixTreeMapUnmanaged(u8, i32, cmpFn);
    var stack = @as([3]*Tree, undefined);
    var tree = Tree{};
    defer tree.deinit(allocator, &stack);

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
    tree.getPtr("z").?.* = 5;
    expect(tree.get("z").? == 5);
}

test "empty tree" {
    const cmpFn = (struct {
        fn f(lhs: u8, rhs: u8) std.math.Order {
            return std.math.order(lhs, rhs);
        }
    }).f;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer expect(!gpa.deinit());
    const allocator = &gpa.allocator;

    const Tree = PrefixTreeMapUnmanaged(u8, i32, cmpFn);
    var stack = @as([3]*Tree, undefined);
    var tree = Tree{};
    defer tree.deinit(allocator, &stack);

    var tree2 = Tree{};
    defer tree2.deinitRecursive(allocator);
}

test "void tree" {
    if (false) {
        const cmpFn = (struct {
            fn f(lhs: u8, rhs: u8) std.math.Order {
                return std.math.order(lhs, rhs);
            }
        }).f;

        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        defer expect(!gpa.deinit());
        const allocator = &gpa.allocator;

        const Tree = PrefixTreeMapUnmanaged(u8, void, cmpFn);
        var tree = Tree{};
        defer tree.deinitRecursive(allocator);

        _ = try tree.insert(allocator, "a", {}, {});
        expect(tree.exists("a"));
        expect(tree.get("a").? == {});
    }
}
