const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const assert = std.debug.assert;
const expect = std.testing.expect;

/// Stolen from std.sort.binarySearch.
fn searchForInsertPosition(
    comptime T: type,
    key: T,
    items: []const T,
) usize {
    var left: usize = 0;
    var right: usize = items.len;

    while (left < right) {
        // Avoid overflowing in the midpoint calculation
        const mid = left + (right - left) / 2;
        // Compare the key with the midpoint element
        switch (key < items[mid]) {
            false => left = mid + 1,
            true => right = mid,
        }
    }

    return left;
}

pub fn PrefixTree(comptime T: type) type {
    const Node = struct {
        const Self = @This();
        // https://github.com/ziglang/zig/issues/4562
        const S = struct { s: ?*Self };

        edges: ArrayListUnmanaged(T) = ArrayListUnmanaged(T){},
        child_nodes: ArrayListUnmanaged(S) = ArrayListUnmanaged(S){},

        fn initCapacity(allocator: *Allocator, n: usize) !Self {
            var l = try ArrayListUnmanaged(T).initCapacity(allocator, n);
            errdefer l.deinit(allocator);
            var p = try ArrayListUnmanaged(S).initCapacity(allocator, n);
            errdefer p.deinit(allocator);
            return Self{
                .edges = l,
                .child_nodes = p,
            };
        }

        fn deinit(self: *Self, allocator: *Allocator) void {
            self.edges.deinit(allocator);
            self.child_nodes.deinit(allocator);
        }

        // FIXME recursive
        pub fn deallocRecursive(self: *Self, allocator: *Allocator) void {
            for (self.child_nodes.items) |p| {
                if (p.s) |node| node.deallocRecursive(allocator);
            }
            self.deinit(allocator);
            allocator.destroy(self);
        }

        fn indexOf(self: Self, edge: T) ?usize {
            const cmpFn = struct {
                fn f(ctx: void, lhs: T, rhs: T) std.math.Order {
                    return std.math.order(lhs, rhs);
                }
            }.f;
            return std.sort.binarySearch(T, edge, self.edges.items, {}, cmpFn);
        }

        pub fn clone(self: Self, allocator: *Allocator) Allocator.Error!*Self {
            switch (internalClone(self, allocator)) {
                .success => |result| {
                    return result;
                },
                .fail => |node| {
                    if (node) |n| n.deallocRecursive(allocator);
                    return error.OutOfMemory;
                },
            }
        }

        // FIXME recursive
        fn internalClone(self: Self, allocator: *Allocator) (union(enum) { success: *Self, fail: ?*Self }) {
            // Don't try to handle any memory allocation failures in this function.
            // Instead, the cleanup is done by the `clone` function.
            const result = allocator.create(Self) catch return .{ .fail = null };
            result.* = Self{};

            const edges_copy = allocator.dupe(T, self.edges.items) catch return .{ .fail = result };
            // Ideally, should be ArrayListUnmanaged(T).fromOwnedSlice(...)
            result.edges = ArrayList(T).fromOwnedSlice(allocator, edges_copy).toUnmanaged();

            result.child_nodes = ArrayListUnmanaged(S).initCapacity(allocator, self.edges.items.len) catch return .{ .fail = result };

            for (self.child_nodes.items) |p| {
                if (p.s) |node| {
                    const child_clone_result = node.internalClone(allocator);
                    switch (child_clone_result) {
                        .success => |n| result.child_nodes.appendAssumeCapacity(.{ .s = n }),
                        .fail => |n| {
                            result.child_nodes.appendAssumeCapacity(.{ .s = n });
                            return .{ .fail = result };
                        },
                    }
                } else {
                    result.child_nodes.appendAssumeCapacity(.{ .s = null });
                }
            }

            return .{ .success = result };
        }

        pub fn format(value: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            return internalFormat(value, writer, fmt);
        }

        // FIXME recursive
        fn internalFormat(self: Self, writer: anytype, comptime fmt: []const u8) @TypeOf(writer).Error!void {
            const fmt_string = "({" ++ fmt ++ "}";
            try writer.print(fmt_string, .{self.edges.items[0]});
            if (self.child_nodes.items[0].s) |p| {
                try writer.writeAll(" ");
                try p.internalFormat(writer, fmt);
            }
            try writer.writeAll(")");

            if (self.edges.items.len > 0) {
                for (self.edges.items[1..]) |edge, i| {
                    try writer.print(" " ++ fmt_string, .{edge});
                    if (self.child_nodes.items[1..][i].s) |p| {
                        try writer.writeAll(" ");
                        try p.internalFormat(writer, fmt);
                    }
                    try writer.writeAll(")");
                }
            }
        }
    };

    return struct {
        const Self = @This();

        root: *Node,
        allocator: *Allocator,

        pub fn init(allocator: *Allocator) !Self {
            const root = try allocator.create(Node);
            root.* = Node{};
            return Self{
                .root = root,
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            self.root.deallocRecursive(self.allocator);
        }

        pub fn exists(self: Self, item: []const T) bool {
            if (item.len == 0) return true;

            var current = self.root;
            for (item[0 .. item.len - 1]) |i| {
                const edge_index = current.indexOf(i) orelse return false;
                current = current.child_nodes.items[edge_index].s orelse return false;
            }
            return current.indexOf(item[item.len - 1]) != null;
        }

        fn newEdge(node: *Node, edge: T, allocator: *Allocator) !usize {
            const index = searchForInsertPosition(T, edge, node.edges.items);
            try node.edges.insert(allocator, index, edge);
            errdefer _ = node.edges.orderedRemove(index);
            try node.child_nodes.insert(allocator, index, .{ .s = null });
            errdefer _ = node.child_nodes.orderedRemove(index);
            return index;
        }

        fn deleteEdge(node: *Node, edge_index: usize, allocator: *Allocator) void {
            _ = node.edges.orderedRemove(edge_index);
            if (node.child_nodes.orderedRemove(edge_index).s) |p| allocator.destroy(p);
        }

        // Must be at least 1 element in item
        fn makeNodeChain(item: []const T, allocator: *Allocator) !*Node {
            var list = try std.ArrayList(*Node).initCapacity(allocator, item.len);
            defer list.deinit();
            errdefer for (list.items) |node| {
                node.deinit(allocator);
                allocator.destroy(node);
            };

            const singleItemNode = (struct {
                fn f(edge: T, alloc: *Allocator) !*Node {
                    const new_node = try alloc.create(Node);
                    errdefer alloc.destroy(new_node);

                    new_node.* = try Node.initCapacity(alloc, 1);
                    errdefer new_node.deinit(alloc);

                    new_node.edges.appendAssumeCapacity(edge);
                    new_node.child_nodes.appendAssumeCapacity(.{ .s = null });
                    return new_node;
                }
            }).f;

            const first_node = try singleItemNode(item[0], allocator);
            list.appendAssumeCapacity(first_node);

            var i: usize = 1;
            while (i < item.len) : (i += 1) {
                const new_node = try singleItemNode(item[i], allocator);
                list.appendAssumeCapacity(new_node);
                list.items[i - 1].child_nodes.items[0] = .{ .s = new_node };
            }

            return list.items[0];
        }

        pub fn insert(self: *Self, item: []const T) !void {
            if (item.len == 0) return;

            var last_edge_index: usize = undefined;
            var last_edge_is_new = false;

            var current_node = self.root;
            var i: usize = 0;
            while (i < item.len) : (i += 1) {
                const edge = item[i];
                if (current_node.indexOf(edge)) |edge_index| {
                    if (current_node.child_nodes.items[edge_index].s) |p| {
                        current_node = p;
                    } else {
                        last_edge_index = edge_index;
                        break;
                    }
                } else {
                    last_edge_index = try newEdge(current_node, edge, self.allocator);
                    last_edge_is_new = true;
                    break;
                }
            }

            if (i < item.len - 1) {
                errdefer if (last_edge_is_new) deleteEdge(current_node, last_edge_index, self.allocator);

                const the_rest = try makeNodeChain(item[i + 1 ..], self.allocator);
                errdefer the_rest.deallocRecursive(self.allocator);
                current_node.child_nodes.items[last_edge_index] = .{ .s = the_rest };
            }
        }

        pub fn clone(self: Self, allocator: *Allocator) !Self {
            return Self{
                .root = try self.root.clone(allocator),
                .allocator = allocator,
            };
        }

        pub fn merge(first: Self, second: Self) !Self {}
    };
}

test "sample tree" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var tree = try PrefixTree(u8).init(&arena.allocator);
    defer tree.deinit();

    expect(tree.exists(""));
    expect(!tree.exists("abc"));
    try tree.insert("abc");
    expect(tree.exists("abc"));
    expect(tree.exists("ab"));
    expect(tree.exists("a"));
    try tree.insert("rockstar");
    expect(tree.exists("rockstar"));
    expect(tree.exists("rocks"));
    expect(tree.exists("rock"));
    try tree.insert("rockstar");
    try tree.insert("rockstars");
    try tree.insert("rockers");
    expect(tree.exists("rockers"));
    expect(tree.exists("rocker"));
    expect(!tree.exists("arock"));
    try tree.insert("bean");
    try tree.insert("bean-truck");

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{c}\n", .{tree.root});

    var tree_clone = try tree.clone(tree.allocator);
    defer tree_clone.deinit();
    try stdout.print("{c}\n", .{tree_clone.root});
}
