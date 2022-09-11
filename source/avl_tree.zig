const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub fn AvlTree(comptime Value: type) type {
    const N = struct {
        left: ?*Self = null,
        right: ?*Self = null,
        value: Value,
        bf: i2 = 0,

        const Self = @This();
    };

    return struct {
        root: ?*Node = null,
        allocator: Allocator,

        const Self = @This();
        const Node = N;

        fn init(allocator: Allocator) !Self {
            return Self{ .allocator = allocator };
        }

        fn exists(tree: Self, value: Value) bool {
            var node_opt = tree.root;
            while (node_opt) |node| {
                if (value < node.value) {
                    node_opt = node.left;
                } else if (value > node.value) {
                    node_opt = node.right;
                } else {
                    return true;
                }
            }
            return false;
        }

        const Item = struct {
            node: *Node,
            direction: Direction,

            const Direction = enum {
                left,
                right,
            };

            fn pickEdge(node: *Node, direction: Direction) *?*Node {
                return switch (direction) {
                    .left => &node.left,
                    .right => &node.right,
                };
            }
        };

        fn insert(tree: *Self, value: Value) !void {
            var stack = ArrayList(Item).init(tree.allocator);
            defer stack.deinit();

            var node_opt = tree.root;
            while (node_opt) |node| {
                if (value < node.value) {
                    try stack.append(.{ .node = node, .direction = .left });
                    node_opt = node.left;
                } else if (value > node.value) {
                    try stack.append(.{ .node = node, .direction = .right });
                    node_opt = node.right;
                } else {
                    unreachable;
                }
            }

            var child = try tree.allocator.create(Node);
            child.* = Node{ .value = value };
            if (stack.items.len == 0) {
                tree.root = child;
                return;
            }
            var parent = stack.pop();
            switch (parent.direction) {
                .left => {
                    parent.node.left = child;
                    parent.node.bf -= 1;
                },
                .right => {
                    parent.node.right = child;
                    parent.node.bf += 1;
                },
            }
            if (parent.node.bf == 0) {
                return;
            }

            var new_subtree_root: ?*Node = null;
            while (stack.items.len > 0) {
                const grandparent = stack.pop();
                const choice_int: i2 = switch (grandparent.direction) {
                    .left => -1,
                    .right => 1,
                };

                if (grandparent.node.bf == -2) {
                    unreachable;
                } else if (grandparent.node.bf == 0) {
                    grandparent.node.bf = choice_int;
                } else if (grandparent.node.bf == -choice_int) {
                    grandparent.node.bf = 0;
                    return;
                } else if (grandparent.node.bf == choice_int) {
                    var grandparent_edge: *?*Node = undefined;
                    var parent_edge: *?*Node = undefined;
                    var child_first_edge: *?*Node = undefined;
                    var child_second_edge: *?*Node = undefined;
                    switch (grandparent.direction) {
                        .left => {
                            grandparent_edge = &grandparent.node.left;
                            parent_edge = &parent.node.right;
                            child_first_edge = &child.left;
                            child_second_edge = &child.right;
                        },
                        .right => {
                            grandparent_edge = &grandparent.node.right;
                            parent_edge = &parent.node.left;
                            child_first_edge = &child.right;
                            child_second_edge = &child.left;
                        },
                    }

                    if (parent.node.bf == choice_int) {
                        // choice == left:  clockwise rotation
                        // choice == right: counter-clockwise rotation
                        grandparent_edge.* = parent_edge.*;
                        parent_edge.* = grandparent.node;

                        grandparent.node.bf = 0;
                        parent.node.bf = 0;

                        new_subtree_root = parent.node;
                    } else if (parent.node.bf == -choice_int) {
                        // choice == left:  counter-clockwise rotation, then clockwise rotation
                        // choice == right: clockwise rotation, then counter-clockwise rotation
                        parent_edge.* = child_first_edge.*;
                        child_first_edge.* = parent.node;
                        grandparent_edge.* = child_second_edge.*;
                        child_second_edge.* = grandparent.node;

                        if (child.bf == choice_int) {
                            grandparent.node.bf = -choice_int;
                            parent.node.bf = 0;
                        } else if (child.bf == -choice_int) {
                            grandparent.node.bf = 0;
                            parent.node.bf = choice_int;
                        } else if (child.bf == 0) {
                            // Only reachable on the first iteration of this loop.
                            grandparent.node.bf = 0;
                            parent.node.bf = 0;
                        } else if (child.bf == -2) {
                            unreachable;
                        }
                        child.bf = 0;

                        new_subtree_root = child;
                    } else {
                        unreachable;
                    }
                    break;
                }

                child = parent.node;
                parent = grandparent;
            }

            if (new_subtree_root) |root| {
                if (stack.items.len > 0) {
                    const item = stack.items[stack.items.len - 1];
                    Item.pickEdge(item.node, item.direction).* = root;
                } else {
                    tree.root = root;
                }
            }
        }

        fn delete(tree: *Self, value: Value) !void {
            var stack = ArrayList(Item).init(tree.allocator);
            defer stack.deinit();

            var node: *Node = tree.root orelse unreachable;
            while (value != node.value) {
                if (value < node.value) {
                    try stack.append(.{ .node = node, .direction = .left });
                    node = node.left.?;
                } else if (value > node.value) {
                    try stack.append(.{ .node = node, .direction = .right });
                    node = node.right.?;
                } else {
                    unreachable;
                }
            }

            var replacement: ?*Node = undefined;
            const index_of_parent_plus_one = stack.items.len;
            if (node.left == null and node.right == null) {
                // N = parent
                replacement = null;
            } else if (node.left == null and node.right != null) {
                // N = parent
                replacement = node.right;
            } else if (node.left != null and node.right == null) {
                // N = parent
                replacement = node.left;
            } else if (node.left != null and node.right != null) {
                const right = node.right.?;
                var successor_parent = node;
                var successor = right;
                var direction: Item.Direction = .right;
                while (true) {
                    try stack.append(.{ .node = successor_parent, .direction = direction });
                    const next = successor.left orelse break;
                    successor_parent = successor;
                    successor = next;
                    direction = .left;
                }
                if (successor != right) {
                    // N = successor_parent
                    successor_parent.left = successor.right;
                    successor.right = node.right;
                } // else N = parent
                successor.left = node.left;
                successor.bf = node.bf;
                stack.items[index_of_parent_plus_one].node = successor;
                replacement = successor;
            }
            tree.allocator.destroy(node);

            if (index_of_parent_plus_one != 0) {
                const parent = stack.items[index_of_parent_plus_one - 1];
                Item.pickEdge(parent.node, parent.direction).* = replacement;
            } else {
                tree.root = replacement;
                if (stack.items.len == 0) {
                    return;
                }
            }

            var parent = stack.pop();
            replacement = null;
            while (true) {
                const choice_int: i2 = switch (parent.direction) {
                    .left => -1,
                    .right => 1,
                };

                if (parent.node.bf == -2) {
                    unreachable;
                } else if (parent.node.bf == choice_int) {
                    parent.node.bf = 0;
                } else if (parent.node.bf == 0) {
                    parent.node.bf = -choice_int;
                    break;
                } else if (parent.node.bf == -choice_int) {
                    var parent_edge: *?*Node = undefined;
                    var child: *Node = undefined;
                    var child_edge: *?*Node = undefined;
                    switch (parent.direction) {
                        .left => {
                            parent_edge = &parent.node.right;
                            child = parent.node.right.?;
                            child_edge = &child.left;
                        },
                        .right => {
                            parent_edge = &parent.node.left;
                            child = parent.node.left.?;
                            child_edge = &child.right;
                        },
                    }

                    if (child.bf == -2) {
                        unreachable;
                    } else if (child.bf == 0 or child.bf == -choice_int) {
                        const stop = child.bf == 0;
                        // counter-clockwise rotation
                        parent_edge.* = child_edge.*;
                        child_edge.* = parent.node;
                        parent.node.bf = -choice_int * @boolToInt(stop);
                        child.bf = -parent.node.bf;
                        replacement = child;
                        if (stop) break;
                    } else if (child.bf == choice_int) {
                        const inner = child_edge.*.?;
                        var inner_first_edge: *?*Node = undefined;
                        var inner_second_edge: *?*Node = undefined;
                        switch (parent.direction) {
                            .left => {
                                inner_first_edge = &inner.right;
                                inner_second_edge = &inner.left;
                            },
                            .right => {
                                inner_first_edge = &inner.left;
                                inner_second_edge = &inner.right;
                            },
                        }

                        // clockwise rotation
                        child_edge.* = inner_first_edge.*;
                        inner_first_edge.* = child;

                        // counter-clockwise rotation
                        parent_edge.* = inner_second_edge.*;
                        inner_second_edge.* = parent.node;

                        if (inner.bf == choice_int) {
                            parent.node.bf = -choice_int;
                            child.bf = 0;
                        } else if (inner.bf == -choice_int) {
                            parent.node.bf = 0;
                            child.bf = choice_int;
                        } else if (inner.bf == 0) {
                            parent.node.bf = 0;
                            child.bf = 0;
                        } else if (inner.bf == -2) {
                            unreachable;
                        }
                        inner.bf = 0;
                        replacement = inner;
                    }
                }

                if (stack.items.len == 0) break;
                parent = stack.pop();
                if (replacement) |root| {
                    Item.pickEdge(parent.node, parent.direction).* = root;
                    replacement = null;
                }
            }

            if (replacement) |root| {
                if (stack.items.len > 0) {
                    const item = stack.items[stack.items.len - 1];
                    Item.pickEdge(item.node, item.direction).* = root;
                } else {
                    tree.root = root;
                }
            }
        }

        // O(n)
        fn print(tree: Self, writer: anytype) !void {
            var stack = ArrayList(*const Node).init(tree.allocator);
            defer stack.deinit();

            var node_opt: ?*const Node = tree.root orelse return;
            {
                while (node_opt.?.left) |left| {
                    try stack.append(node_opt.?);
                    node_opt = left;
                }
            }
            while (node_opt) |node| {
                try writer.print("{c}", .{node.value});
                if (node.right) |right| {
                    node_opt = right;
                    while (node_opt.?.left) |left| {
                        try stack.append(node_opt.?);
                        node_opt = left;
                    }
                } else {
                    node_opt = stack.popOrNull();
                }
            }
        }

        fn showTree(tree: Self, writer: anytype) !void {
            var stack = ArrayList(struct { node: *const Node, indent: usize, which: enum { root, left, right } }).init(tree.allocator);
            defer stack.deinit();

            if (tree.root) |root| {
                try stack.append(.{ .node = root, .indent = 0, .which = .root });
            }
            while (stack.items.len > 0) {
                const item = stack.pop();
                const node = item.node;
                try writer.writeByteNTimes(' ', item.indent * 4);
                try writer.print("({s} node) value={c}, bf={}\n", .{ @tagName(item.which), node.value, node.bf });

                if (node.right) |right| try stack.append(.{ .node = right, .indent = item.indent + 1, .which = .right });
                if (node.left) |left| try stack.append(.{ .node = left, .indent = item.indent + 1, .which = .left });
            }
        }
    };
}

test "avl tree" {
    const writer = std.io.getStdErr().writer();
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const R = AvlTree(u8);
    var tree = try R.init(allocator);

    for ("acbdmzGAXlkw987654") |c| {
        try tree.insert(c);
        try tree.print(writer);
        try writer.writeByte('\n');
    }

    for ("acbdmzGAXlkw987654") |c| {
        try tree.delete(c);
        try tree.print(writer);
        try writer.writeByte('\n');
    }
}
