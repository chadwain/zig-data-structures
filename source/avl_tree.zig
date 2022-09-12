const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

fn AvlTreeNode(comptime Value: type) type {
    return struct {
        const Self = @This();
        left: ?*Self = null,
        right: ?*Self = null,
        parent: ?*Self = null,
        value: Value,
        bf: i2 = 0,

        pub fn format(node: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = options;
            try writer.print(
                "value={" ++ fmt ++ "}, bf={}, parent={?" ++ fmt ++ "}, left={?" ++ fmt ++ "}, right={?" ++ fmt ++ "}",
                .{
                    node.value,
                    node.bf,
                    if (node.parent) |n| @as(?Value, n.value) else null,
                    if (node.left) |n| @as(?Value, n.value) else null,
                    if (node.right) |n| @as(?Value, n.value) else null,
                },
            );
        }
    };
}

pub fn AvlTree(comptime Value: type) type {
    return struct {
        root: ?*Node = null,
        allocator: Allocator,

        const Self = @This();
        const Node = AvlTreeNode(Value);

        fn init(allocator: Allocator) Self {
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

        fn insert(tree: *Self, value: Value) !void {
            var child = try tree.allocator.create(Node);
            child.* = Node{ .value = value };

            var parent = tree.root orelse {
                tree.root = child;
                return;
            };
            while (true) {
                if (value < parent.value) {
                    parent = parent.left orelse break;
                } else if (value > parent.value) {
                    parent = parent.right orelse break;
                } else {
                    unreachable;
                }
            }

            child.parent = parent;
            if (value < parent.value) {
                parent.left = child;
                parent.bf -= 1;
            } else {
                parent.right = child;
                parent.bf += 1;
            }
            if (parent.bf == 0) {
                return;
            }

            while (parent.parent) |grandparent| {
                const choice_int: i2 = if (value < grandparent.value) -1 else 1;

                if (grandparent.bf == -2) {
                    unreachable;
                } else if (grandparent.bf == 0) {
                    grandparent.bf = choice_int;
                } else if (grandparent.bf == -choice_int) {
                    grandparent.bf = 0;
                    return;
                } else if (grandparent.bf == choice_int) {
                    var grandparent_edge: *?*Node = undefined;
                    var parent_edge: *?*Node = undefined;
                    var child_first_edge: *?*Node = undefined;
                    var child_second_edge: *?*Node = undefined;
                    if (value < grandparent.value) {
                        grandparent_edge = &grandparent.left;
                        parent_edge = &parent.right;
                        child_first_edge = &child.left;
                        child_second_edge = &child.right;
                    } else {
                        grandparent_edge = &grandparent.right;
                        parent_edge = &parent.left;
                        child_first_edge = &child.right;
                        child_second_edge = &child.left;
                    }

                    var new_subtree_root: *Node = undefined;
                    if (parent.bf == choice_int) {
                        // choice_int == -1:  clockwise rotation
                        // choice_int == +1: counter-clockwise rotation
                        grandparent_edge.* = parent_edge.*;
                        if (parent_edge.*) |n| n.parent = grandparent;
                        parent_edge.* = grandparent;
                        parent.parent = grandparent.parent;
                        grandparent.parent = parent;

                        grandparent.bf = 0;
                        parent.bf = 0;

                        new_subtree_root = parent;
                    } else if (parent.bf == -choice_int) {
                        // choice_int == -1: counter-clockwise rotation
                        // choice_int == +1: clockwise rotation
                        parent_edge.* = child_first_edge.*;
                        if (child_first_edge.*) |n| n.parent = parent;
                        child_first_edge.* = parent;
                        parent.parent = child;

                        // choice_int == -1: clockwise rotation
                        // choice_int == +1: counter-clockwise rotation
                        grandparent_edge.* = child_second_edge.*;
                        if (child_second_edge.*) |n| n.parent = grandparent;
                        child_second_edge.* = grandparent;
                        child.parent = grandparent.parent;
                        grandparent.parent = child;

                        if (child.bf == choice_int) {
                            grandparent.bf = -choice_int;
                            parent.bf = 0;
                        } else if (child.bf == -choice_int) {
                            grandparent.bf = 0;
                            parent.bf = choice_int;
                        } else if (child.bf == 0) {
                            // Only reachable on the first iteration of this loop.
                            grandparent.bf = 0;
                            parent.bf = 0;
                        } else if (child.bf == -2) {
                            unreachable;
                        }
                        child.bf = 0;

                        new_subtree_root = child;
                    } else {
                        unreachable;
                    }

                    if (new_subtree_root.parent) |new_parent| {
                        if (value < new_parent.value) {
                            new_parent.left = new_subtree_root;
                        } else {
                            new_parent.right = new_subtree_root;
                        }
                    } else {
                        tree.root = new_subtree_root;
                    }
                    return;
                }

                child = parent;
                parent = grandparent;
            }
        }

        fn delete(tree: *Self, value: Value) void {
            var node: *Node = tree.root orelse unreachable;
            var choice_int: i2 = undefined;
            while (value != node.value) {
                if (value < node.value) {
                    node = node.left.?;
                    choice_int = -1;
                } else if (value > node.value) {
                    node = node.right.?;
                    choice_int = 1;
                } else {
                    unreachable;
                }
            }

            var replacement: ?*Node = undefined;
            var start_retracing_from: ?*Node = undefined;
            var successor_value: ?Value = undefined;
            if (node.left == null or node.right == null) {
                replacement = node.left orelse node.right orelse null;
                start_retracing_from = node.parent;
                successor_value = null;
            } else {
                const right = node.right.?;
                var successor = right;
                choice_int = 1;
                while (successor.left) |next| {
                    successor = next;
                    choice_int = -1;
                }
                if (successor != right) {
                    successor.parent.?.left = successor.right;
                    if (successor.right) |n| n.parent = successor.parent;
                    successor.right = right;
                    right.parent = successor;
                    start_retracing_from = successor.parent;
                } else {
                    start_retracing_from = successor;
                }
                successor.left = node.left;
                node.left.?.parent = successor;
                successor.bf = node.bf;
                replacement = successor;
                successor_value = successor.value;
            }

            {
                defer tree.allocator.destroy(node);
                if (replacement) |n| n.parent = node.parent;
                if (node.parent) |parent| {
                    if (value < parent.value) {
                        parent.left = replacement;
                    } else {
                        parent.right = replacement;
                    }
                } else {
                    tree.root = replacement;
                    if (start_retracing_from == null) {
                        return;
                    }
                }
            }

            node = start_retracing_from.?;
            while (true) {
                if (node.bf == -2) {
                    unreachable;
                } else if (node.bf == choice_int) {
                    node.bf = 0;
                } else if (node.bf == 0) {
                    node.bf = -choice_int;
                    break;
                } else if (node.bf == -choice_int) {
                    var node_edge: *?*Node = undefined;
                    var child: *Node = undefined;
                    var child_edge: *?*Node = undefined;
                    if (choice_int == -1) {
                        node_edge = &node.right;
                        child = node.right.?;
                        child_edge = &child.left;
                    } else {
                        node_edge = &node.left;
                        child = node.left.?;
                        child_edge = &child.right;
                    }

                    var new_subtree_root: *Node = undefined;
                    const stop = child.bf == 0;
                    if (child.bf == -2) {
                        unreachable;
                    } else if (child.bf == 0 or child.bf == -choice_int) {
                        // choice_int == -1: counter-clockwise rotation
                        // choice_int == +1: clockwise rotation
                        node_edge.* = child_edge.*;
                        if (child_edge.*) |n| n.parent = node;
                        child_edge.* = node;
                        child.parent = node.parent;
                        node.parent = child;
                        node.bf = -choice_int * @boolToInt(stop);
                        child.bf = -node.bf;
                        new_subtree_root = child;
                    } else if (child.bf == choice_int) {
                        const grandchild = child_edge.*.?;
                        var grandchild_first_edge: *?*Node = undefined;
                        var grandchild_second_edge: *?*Node = undefined;
                        if (choice_int == -1) {
                            grandchild_first_edge = &grandchild.right;
                            grandchild_second_edge = &grandchild.left;
                        } else {
                            grandchild_first_edge = &grandchild.left;
                            grandchild_second_edge = &grandchild.right;
                        }

                        // choice_int == -1: clockwise rotation
                        // choice_int == +1: counter-clockwise rotation
                        child_edge.* = grandchild_first_edge.*;
                        if (grandchild_first_edge.*) |n| n.parent = child;
                        grandchild_first_edge.* = child;
                        child.parent = grandchild;

                        // choice_int == -1: counter-clockwise rotation
                        // choice_int == +1: clockwise rotation
                        node_edge.* = grandchild_second_edge.*;
                        if (grandchild_second_edge.*) |n| n.parent = node;
                        grandchild_second_edge.* = node;
                        grandchild.parent = node.parent;
                        node.parent = grandchild;

                        if (grandchild.bf == choice_int) {
                            node.bf = 0;
                            child.bf = -choice_int;
                        } else if (grandchild.bf == -choice_int) {
                            node.bf = choice_int;
                            child.bf = 0;
                        } else if (grandchild.bf == 0) {
                            node.bf = 0;
                            child.bf = 0;
                        } else if (grandchild.bf == -2) {
                            unreachable;
                        }
                        grandchild.bf = 0;
                        new_subtree_root = grandchild;
                    }

                    node = new_subtree_root.parent orelse {
                        tree.root = new_subtree_root;
                        return;
                    };
                    choice_int = choice_int: {
                        if (successor_value) |s| {
                            if (node.value == s) break :choice_int 1;
                        }
                        break :choice_int if (value < node.value) @as(i2, -1) else 1;
                    };
                    if (choice_int == -1) {
                        node.left = new_subtree_root;
                    } else {
                        node.right = new_subtree_root;
                    }
                    if (stop) {
                        return;
                    } else {
                        continue;
                    }
                }

                node = node.parent orelse return;
                choice_int = choice_int: {
                    if (successor_value) |s| {
                        if (node.value == s) break :choice_int 1;
                    }
                    break :choice_int if (value < node.value) @as(i2, -1) else 1;
                };
            }
        }

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

        fn showTree(tree: Self, writer: anytype, comptime fmt: []const u8) !void {
            var stack = ArrayList(struct { node: *const Node, indent: usize, which: enum { root, left, right } }).init(tree.allocator);
            defer stack.deinit();

            if (tree.root) |root| {
                try stack.append(.{ .node = root, .indent = 0, .which = .root });
            }
            while (stack.items.len > 0) {
                const item = stack.pop();
                const node = item.node;
                try writer.writeByteNTimes(' ', item.indent * 4);
                try writer.print(
                    "({s} node) value={" ++ fmt ++ "}, bf={}, parent={?" ++ fmt ++ "}\n",
                    .{ @tagName(item.which), node.value, node.bf, if (node.parent) |p| @as(?u8, p.value) else null },
                );

                if (node.right) |right| try stack.append(.{ .node = right, .indent = item.indent + 1, .which = .right });
                if (node.left) |left| try stack.append(.{ .node = left, .indent = item.indent + 1, .which = .left });
            }
        }

        const VerifyTreeFailure = struct {
            node: *const Node,
            reason: enum {
                RootHasParent,
                MissingParent,
                WrongParent,
                WrongDirection,
                WrongBalanceFactor,
                HeightDifferenceTooLarge,
                DuplicateValue,
                Cycle,
            },

            fn print(failure: VerifyTreeFailure, writer: anytype, comptime fmt: []const u8, tree: Self) !void {
                try writer.print("Invalid tree detected! Reason: {s}\n{" ++ fmt ++ "}\n", .{ @tagName(failure.reason), failure.node });
                try tree.showTree(writer, fmt);
            }
        };

        fn verifyTree(tree: Self) !?VerifyTreeFailure {
            var stack = ArrayList(struct { node: ?*const Node, height_left: usize, height_right: usize, direction: enum { down, left, right } }).init(tree.allocator);
            defer stack.deinit();

            var visitied_nodes = std.AutoHashMap(*const Node, void).init(tree.allocator);
            defer visitied_nodes.deinit();

            {
                const root = tree.root orelse return null;
                if (root.parent) |_| return VerifyTreeFailure{ .node = root, .reason = .RootHasParent };
                try stack.append(.{ .node = root, .height_left = undefined, .height_right = undefined, .direction = .left });
                try stack.append(.{ .node = root.left, .height_left = undefined, .height_right = undefined, .direction = .down });
                try visitied_nodes.put(root, {});
            }

            while (stack.items.len > 0) {
                const this = &stack.items[stack.items.len - 1];
                switch (this.direction) {
                    .down => {
                        const parent = &stack.items[stack.items.len - 2];
                        const node = this.node orelse {
                            switch (parent.direction) {
                                .left => parent.height_left = 0,
                                .right => parent.height_right = 0,
                                else => unreachable,
                            }
                            _ = stack.pop();
                            continue;
                        };
                        const parent_node = parent.node.?;

                        if (try visitied_nodes.fetchPut(node, {})) |_| {
                            return VerifyTreeFailure{ .node = parent_node, .reason = .Cycle };
                        }

                        if (node.parent) |node_parent| {
                            if (node_parent != parent_node) return VerifyTreeFailure{ .node = node, .reason = .WrongParent };
                        } else {
                            return VerifyTreeFailure{ .node = node, .reason = .MissingParent };
                        }

                        if (node.value == parent_node.value) return VerifyTreeFailure{ .node = node, .reason = .DuplicateValue };
                        if ((parent.direction == .left and node.value > parent_node.value) or
                            (parent.direction == .right and node.value < parent_node.value))
                        {
                            return VerifyTreeFailure{ .node = node, .reason = .WrongDirection };
                        }

                        this.direction = .left;
                        try stack.append(.{ .node = node.left, .height_left = undefined, .height_right = undefined, .direction = .down });
                    },
                    .left => {
                        this.direction = .right;
                        try stack.append(.{ .node = this.node.?.right, .height_left = undefined, .height_right = undefined, .direction = .down });
                    },
                    .right => {
                        const node = this.node.?;

                        if (std.math.max(this.height_left, this.height_right) - std.math.min(this.height_left, this.height_right) > 1) {
                            return VerifyTreeFailure{ .node = node, .reason = .HeightDifferenceTooLarge };
                        }

                        if (!switch (node.bf) {
                            -2 => unreachable,
                            -1 => this.height_left > this.height_right,
                            0 => this.height_left == this.height_right,
                            1 => this.height_left < this.height_right,
                        }) {
                            return VerifyTreeFailure{ .node = node, .reason = .WrongBalanceFactor };
                        }

                        _ = stack.pop();
                        if (stack.items.len > 0) {
                            const height = std.math.max(this.height_left, this.height_right);
                            const parent = &stack.items[stack.items.len - 1];
                            switch (parent.direction) {
                                .left => parent.height_left = height + 1,
                                .right => parent.height_right = height + 1,
                                else => unreachable,
                            }
                        }
                    },
                }
            }

            return null;
        }
    };
}

test "avl tree" {
    const writer = std.io.getStdErr().writer();
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const R = AvlTree(u8);
    var tree = R.init(allocator);

    for ("acbdmzGAXlkw987654") |c| {
        try tree.insert(c);
        if (try tree.verifyTree()) |failure| {
            try failure.print(writer, "c", tree);
            return error.InvalidTree;
        }
    }

    if (try tree.verifyTree()) |failure| {
        try failure.print(writer, "c", tree);
        return error.InvalidTree;
    }

    for ("acbdmzGAXlkw987654") |c| {
        tree.delete(c);
        if (try tree.verifyTree()) |failure| {
            try writer.print("Failed to delete value: {c}\n", .{c});
            try failure.print(writer, "c", tree);
            return error.InvalidTree;
        }
    }
}

test "random inputs" {
    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator); // TODO: tree.deinit
    defer arena.deinit();
    const writer = std.io.getStdErr().writer();

    var rng = std.rand.DefaultPrng.init(0);
    const random = rng.random();

    var tree = AvlTree(u8).init(arena.allocator());

    var list: [256]u8 = undefined;
    var list_len: u9 = 0;
    var i: u9 = 0;
    while (i < list.len) : (i += 1) {
        const int = random.int(u8);
        if (!tree.exists(int)) {
            std.debug.print("insert {}\n", .{int});
            list[list_len] = int;
            list_len += 1;
            try tree.insert(int);
            if (try tree.verifyTree()) |failure| {
                try writer.print("Failed to delete value: {}\n", .{int});
                try failure.print(writer, "", tree);
                return error.InvalidTree;
            }
        }
    }

    i = list_len;
    while (i > 0) : (i -= 1) {
        const int = random.uintAtMostBiased(u8, @truncate(u8, i - 1));
        const value = list[int];
        assert(tree.exists(value));
        std.debug.print("delete {}\n", .{value});
        std.mem.swap(u8, &list[int], &list[i - 1]);
        tree.delete(value);
        if (try tree.verifyTree()) |failure| {
            try writer.print("Failed to delete value: {}\n", .{value});
            try failure.print(writer, "", tree);
            return error.InvalidTree;
        }
    }
}
