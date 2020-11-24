const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const assert = std.debug.assert;
const expect = std.testing.expect;

fn PrefixTreeNode(comptime T: type, comptime cmpFn: fn (lhs: T, rhs: T) std.math.Order) type {
    return struct {
        const Self = @This();
        // https://github.com/ziglang/zig/issues/4562
        const S = struct { s: ?*Self };

        edges: ArrayListUnmanaged(T) = ArrayListUnmanaged(T){},
        child_nodes: ArrayListUnmanaged(S) = ArrayListUnmanaged(S){},

        fn initCapacity(allocator: *Allocator, n: usize) !Self {
            var edges = try ArrayListUnmanaged(T).initCapacity(allocator, n);
            errdefer edges.deinit(allocator);
            var child_nodes = try ArrayListUnmanaged(S).initCapacity(allocator, n);
            errdefer child_nodes.deinit(allocator);
            return Self{
                .edges = edges,
                .child_nodes = child_nodes,
            };
        }

        fn deinit(self: *Self, allocator: *Allocator) void {
            self.edges.deinit(allocator);
            self.child_nodes.deinit(allocator);
        }

        // FIXME recursive
        fn deallocRecursive(self: *Self, allocator: *Allocator) void {
            for (self.child_nodes.items) |child| {
                if (child.s) |node| node.deallocRecursive(allocator);
            }
            self.deinit(allocator);
            allocator.destroy(self);
        }

        fn exists(self: *const Self, item: []const T) bool {
            if (item.len == 0) return true;

            var current = self;
            for (item[0 .. item.len - 1]) |edge| {
                const edge_index = current.indexOf(edge) orelse return false;
                current = current.child_nodes.items[edge_index].s orelse return false;
            }
            return current.indexOf(item[item.len - 1]) != null;
        }

        fn indexOf(self: Self, edge: T) ?usize {
            const cmp = struct {
                fn f(ctx: void, lhs: T, rhs: T) std.math.Order {
                    return cmpFn(lhs, rhs);
                }
            }.f;
            return std.sort.binarySearch(T, edge, self.edges.items, {}, cmp);
        }

        /// Stolen from std.sort.binarySearch.
        fn searchForInsertPosition(
            key: T,
            items: []const T,
        ) usize {
            var left: usize = 0;
            var right: usize = items.len;

            while (left < right) {
                // Avoid overflowing in the midpoint calculation
                const mid = left + (right - left) / 2;
                // Compare the key with the midpoint element
                switch (cmpFn(key, items[mid])) {
                    .eq => unreachable,
                    .gt => left = mid + 1,
                    .lt => right = mid,
                }
            }

            return left;
        }

        fn newEdge(self: *Self, edge: T, allocator: *Allocator) !usize {
            const index = searchForInsertPosition(edge, self.edges.items);
            try self.edges.insert(allocator, index, edge);
            errdefer _ = self.edges.orderedRemove(index);
            try self.child_nodes.insert(allocator, index, .{ .s = null });
            errdefer _ = self.child_nodes.orderedRemove(index);
            return index;
        }

        /// Delete the most recent edge that was just created by newEdge
        fn deleteNewEdge(self: *Self, edge_index: usize, allocator: *Allocator) void {
            _ = self.edges.orderedRemove(edge_index);
            _ = self.child_nodes.orderedRemove(edge_index);
        }

        fn insert(self: *Self, item: []const T, allocator: *Allocator) !void {
            if (item.len == 0) return;

            var last_edge_index: usize = undefined;
            var last_edge_is_new = false;

            var current_node = self;
            var i: usize = 0;
            while (i < item.len) : (i += 1) {
                const edge = item[i];
                if (current_node.indexOf(edge)) |edge_index| {
                    if (current_node.child_nodes.items[edge_index].s) |child| {
                        current_node = child;
                    } else {
                        last_edge_index = edge_index;
                        break;
                    }
                } else {
                    last_edge_index = try current_node.newEdge(edge, allocator);
                    last_edge_is_new = true;
                    break;
                }
            }

            if (i < item.len - 1) {
                errdefer if (last_edge_is_new) current_node.deleteNewEdge(last_edge_index, allocator);

                const the_rest = try createBranch(item[i + 1 ..], allocator);
                errdefer the_rest.deallocRecursive(allocator);
                current_node.child_nodes.items[last_edge_index] = .{ .s = the_rest };
            }
        }

        // Must be at least 1 element in item
        fn createBranch(item: []const T, allocator: *Allocator) !*Self {
            var list = try std.ArrayList(*Self).initCapacity(allocator, item.len);
            defer list.deinit();
            errdefer for (list.items) |node| {
                node.deinit(allocator);
                allocator.destroy(node);
            };

            const singleEdgeNode = (struct {
                fn f(edge: T, alloc: *Allocator) !*Self {
                    const new_node = try alloc.create(Self);
                    errdefer alloc.destroy(new_node);

                    new_node.* = try Self.initCapacity(alloc, 1);
                    errdefer new_node.deinit(alloc);

                    new_node.edges.appendAssumeCapacity(edge);
                    new_node.child_nodes.appendAssumeCapacity(.{ .s = null });
                    return new_node;
                }
            }).f;

            const first_node = try singleEdgeNode(item[0], allocator);
            list.appendAssumeCapacity(first_node);

            var i: usize = 1;
            while (i < item.len) : (i += 1) {
                const new_node = try singleEdgeNode(item[i], allocator);
                list.appendAssumeCapacity(new_node);
                list.items[i - 1].child_nodes.items[0] = .{ .s = new_node };
            }

            return list.items[0];
        }

        fn clone(self: Self, allocator: *Allocator) Allocator.Error!*Self {
            const clone_result = internalClone(self, allocator);
            if (clone_result.err) |err| {
                if (clone_result.node) |node| node.deallocRecursive(allocator);
                return err;
            } else {
                return clone_result.node.?;
            }
        }

        // FIXME recursive
        fn internalClone(self: Self, allocator: *Allocator) (struct { node: ?*Self, err: ?Allocator.Error }) {
            // Don't try to handle any memory allocation failures in this function.
            // Instead, the cleanup is done by the `clone` function.
            const result = allocator.create(Self) catch |err| return .{ .node = null, .err = err };
            result.* = Self{};

            const edges_copy = allocator.dupe(T, self.edges.items) catch |err| return .{ .node = result, .err = err };
            // Ideally, should be ArrayListUnmanaged(T).fromOwnedSlice(...)
            result.edges = ArrayList(T).fromOwnedSlice(allocator, edges_copy).toUnmanaged();

            result.child_nodes = ArrayListUnmanaged(S).initCapacity(allocator, self.edges.items.len) catch |err| return .{ .node = result, .err = err };

            for (self.child_nodes.items) |child| {
                if (child.s) |node| {
                    const child_clone_result = node.internalClone(allocator);
                    result.child_nodes.appendAssumeCapacity(.{ .s = child_clone_result.node });
                    if (child_clone_result.err) |_| return .{ .node = result, .err = child_clone_result.err };
                } else {
                    result.child_nodes.appendAssumeCapacity(.{ .s = null });
                }
            }

            return .{ .node = result, .err = null };
        }

        // FIXME recursive
        fn format(self: Self, writer: anytype, comptime fmt: []const u8) @TypeOf(writer).Error!void {
            const fmt_string = "({" ++ fmt ++ "}";
            try writer.print(fmt_string, .{self.edges.items[0]});
            if (self.child_nodes.items[0].s) |child| {
                try writer.writeAll(" ");
                try child.format(writer, fmt);
            }
            try writer.writeAll(")");

            if (self.edges.items.len > 0) {
                for (self.edges.items[1..]) |edge, i| {
                    try writer.print(" " ++ fmt_string, .{edge});
                    if (self.child_nodes.items[1..][i].s) |child| {
                        try writer.writeAll(" ");
                        try child.format(writer, fmt);
                    }
                    try writer.writeAll(")");
                }
            }
        }

        const Leaves = struct {
            items: [][]T,
            allocator: *Allocator,

            fn free(self: @This()) void {
                for (self.items) |leaf| self.allocator.free(leaf);
            }
        };

        fn getLeaves(self: *const Self, allocator: *Allocator) !Leaves {
            var stack = ArrayList(T).init(allocator);
            defer stack.deinit();

            var list = ArrayList([]T).init(allocator);
            try internalGetLeaves(self, &list, &stack);
            return Leaves{ .items = list.toOwnedSlice(), .allocator = allocator };
        }

        fn internalGetLeaves(self: *const Self, list: *ArrayList([]T), stack: *ArrayList(T)) Allocator.Error!void {
            for (self.child_nodes.items) |child, i| {
                const edge = self.edges.items[i];
                if (child.s) |node| {
                    try stack.append(edge);
                    try internalGetLeaves(node, list, stack);
                    _ = stack.pop();
                } else {
                    const item = blk: {
                        const buf = try list.allocator.alloc(T, stack.items.len + 1);
                        std.mem.copy(T, buf, stack.items);
                        buf[buf.len - 1] = edge;
                        break :blk buf;
                    };
                    errdefer list.allocator.free(item);
                    try list.append(item);
                }
            }
        }
    };
}

pub fn PrefixTree(comptime T: type, comptime cmpFn: fn (lhs: T, rhs: T) std.math.Order) type {
    return struct {
        const Self = @This();
        const Node = PrefixTreeNode(T, cmpFn);

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
            return self.root.exists(item);
        }

        pub fn insert(self: Self, item: []const T) !void {
            return self.root.insert(item, self.allocator);
        }

        pub fn clone(self: Self, allocator: *Allocator) !Self {
            return Self{
                .root = try self.root.clone(allocator),
                .allocator = allocator,
            };
        }

        pub fn merge(first: Self, second: Self, allocator: *Allocator) !Self {
            var copy = try first.clone(allocator);
            errdefer copy.deinit();

            const second_leaves = try second.root.getLeaves(allocator);
            defer second_leaves.free();

            for (second_leaves.items) |leaf| try copy.insert(leaf);
            return copy;
        }

        pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            return self.root.format(writer, fmt);
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

    var tree = try PrefixTree(u8, cmpFn).init(&arena.allocator);
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
    try stdout.print("{c}\n", .{tree});

    var tree_clone = try tree.clone(tree.allocator);
    defer tree_clone.deinit();
    try stdout.print("{c}\n", .{tree_clone});

    const leaves = try tree.root.getLeaves(&arena.allocator);
    defer leaves.free();
    for (leaves.items) |leaf| {
        try stdout.print("{}\n", .{leaf});
    }

    var second_tree = blk: {
        var res = try PrefixTree(u8, cmpFn).init(&arena.allocator);
        errdefer res.deinit();
        try res.insert("roadmap");
        try res.insert("beethoven");
        try res.insert("zig");
        try res.insert("ziglang");
        break :blk res;
    };
    defer second_tree.deinit();

    const second_leaves = try second_tree.root.getLeaves(&arena.allocator);
    defer second_leaves.free();
    for (second_leaves.items) |leaf| {
        try stdout.print("{}\n", .{leaf});
    }

    var merged = try tree.merge(second_tree, &arena.allocator);
    defer merged.deinit();
    try stdout.print("{c}\n", .{merged});
}
