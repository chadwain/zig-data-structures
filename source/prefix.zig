const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const assert = std.debug.assert;
const expect = std.testing.expect;

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
    return struct {
        const Self = @This();

        const Node = struct {
            // https://github.com/ziglang/zig/issues/4562
            const S = struct { s: ?*Node };

            labels: ArrayListUnmanaged(T) = ArrayListUnmanaged(T){},
            pointers: ArrayListUnmanaged(S) = ArrayListUnmanaged(S){},

            fn initCapacity(allocator: *Allocator, n: usize) !Node {
                var l = try ArrayListUnmanaged(T).initCapacity(allocator, n);
                errdefer l.deinit(allocator);
                var p = try ArrayListUnmanaged(S).initCapacity(allocator, n);
                errdefer p.deinit(allocator);
                return Node{
                    .labels = l,
                    .pointers = p,
                };
            }

            fn deinit(self: *Node, allocator: *Allocator) void {
                self.labels.deinit(allocator);
                self.pointers.deinit(allocator);
            }

            fn indexOf(self: Node, label: T) ?usize {
                const cmpFn = struct {
                    fn f(ctx: void, lhs: T, rhs: T) std.math.Order {
                        return std.math.order(lhs, rhs);
                    }
                }.f;
                return std.sort.binarySearch(T, label, self.labels.items, {}, cmpFn);
            }

            pub fn format(value: Node, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
                return internalFormat(value, writer, fmt);
            }

            // TODO somehow make this non-recursive
            fn internalFormat(self: Node, writer: anytype, comptime fmt: []const u8) @TypeOf(writer).Error!void {
                const fmt_string = "({" ++ fmt ++ "}";
                try writer.print(fmt_string, .{self.labels.items[0]});
                if (self.pointers.items[0].s) |p| {
                    try writer.writeAll(" ");
                    try p.internalFormat(writer, fmt);
                }
                try writer.writeAll(")");

                if (self.labels.items.len > 0) {
                    for (self.labels.items[1..]) |label, i| {
                        try writer.print(" " ++ fmt_string, .{label});
                        if (self.pointers.items[1..][i].s) |p| {
                            try writer.writeAll(" ");
                            try p.internalFormat(writer, fmt);
                        }
                        try writer.writeAll(")");
                    }
                }
            }
        };

        root: Node = Node{},
        allocator: *Allocator,

        pub fn init(allocator: *Allocator) Self {
            return Self{
                .allocator = allocator,
            };
        }

        pub fn exists(self: Self, item: []const T) bool {
            if (item.len == 0) return true;

            var current = &self.root;
            for (item[0 .. item.len - 1]) |i| {
                const edge_index = current.indexOf(i) orelse return false;
                current = current.pointers.items[edge_index].s orelse return false;
            }
            return current.indexOf(item[item.len - 1]) != null;
        }

        fn newLabel(node: *Node, label: T, allocator: *Allocator) !usize {
            const index = searchForInsertPosition(T, label, node.labels.items);
            try node.labels.insert(allocator, index, label);
            errdefer _ = node.labels.orderedRemove(index);
            try node.pointers.insert(allocator, index, .{ .s = null });
            errdefer _ = node.pointers.orderedRemove(index);
            return index;
        }

        fn deleteEdge(node: *Node, edge_index: usize, allocator: *Allocator) void {
            _ = node.labels.orderedRemove(edge_index);
            if (node.pointers.orderedRemove(edge_index).s) |p| allocator.destroy(p);
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
                fn f(label: T, alloc: *Allocator) !*Node {
                    const new_node = try alloc.create(Node);
                    errdefer alloc.destroy(new_node);

                    new_node.* = try Node.initCapacity(alloc, 1);
                    errdefer new_node.deinit(alloc);

                    new_node.labels.appendAssumeCapacity(label);
                    new_node.pointers.appendAssumeCapacity(.{ .s = null });
                    return new_node;
                }
            }).f;

            const first_node = try singleItemNode(item[0], allocator);
            list.appendAssumeCapacity(first_node);

            var i: usize = 1;
            while (i < item.len) : (i += 1) {
                const new_node = try singleItemNode(item[i], allocator);
                list.appendAssumeCapacity(new_node);
                list.items[i - 1].pointers.items[0] = .{ .s = new_node };
            }

            return list.items[0];
        }

        pub fn insert(self: *Self, item: []const T) !void {
            if (item.len == 0) return;

            var last_label_index: usize = undefined;
            var last_label_is_new = false;

            var current_node = &self.root;
            var i: usize = 0;
            while (i < item.len) : (i += 1) {
                const label = item[i];
                if (current_node.indexOf(label)) |edge_index| {
                    if (current_node.pointers.items[edge_index].s) |p| {
                        current_node = p;
                    } else {
                        last_label_index = edge_index;
                        break;
                    }
                } else {
                    last_label_index = try newLabel(current_node, label, self.allocator);
                    last_label_is_new = true;
                    break;
                }
            }

            if (i < item.len - 1) {
                errdefer if (last_label_is_new) deleteEdge(current_node, last_label_index, self.allocator);

                const the_rest = try makeNodeChain(item[i + 1 ..], self.allocator);
                current_node.pointers.items[last_label_index] = .{ .s = the_rest };
            }
        }
    };
}

test "sample tree" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var tree = PrefixTree(u8).init(&arena.allocator);
    //defer tree.deinit();

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
}
