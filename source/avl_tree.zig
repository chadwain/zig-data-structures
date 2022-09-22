const std = @import("std");
const assert = std.debug.assert;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

fn AvlTreeNode(comptime Value: type) type {
    return struct {
        const Self = @This();
        left: ?*Self,
        right: ?*Self,
        parent: ?*Self,
        value: Value,
        bf: i2,

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
        const Edge = *?*Node;

        fn init(allocator: Allocator) Self {
            return Self{ .allocator = allocator };
        }

        fn deinit(tree: *Self) void {
            var node = tree.root orelse return;
            node.bf = -1;
            while (true) {
                if (node.bf == -1) {
                    node.bf = 0;
                    if (node.left) |left| {
                        node = left;
                        left.bf = -1;
                        continue;
                    }
                }

                if (node.bf == 0) {
                    node.bf = 1;
                    if (node.right) |right| {
                        node = right;
                        right.bf = -1;
                        continue;
                    }
                }

                const parent = node.parent;
                tree.allocator.destroy(node);
                node = parent orelse return;
            }
        }

        fn height(tree: Self) usize {
            var result: usize = 0;
            var node_opt = tree.root;
            while (node_opt) |node| {
                result += 1;
                switch (node.bf) {
                    -2 => unreachable,
                    -1 => node_opt = node.left,
                    0, 1 => node_opt = node.right,
                }
            }
            return result;
        }

        fn find(tree: Self, value: Value) ?*Node {
            var node_opt = tree.root;
            while (node_opt) |node| {
                if (value < node.value) {
                    node_opt = node.left;
                } else if (value > node.value) {
                    node_opt = node.right;
                } else {
                    return node;
                }
            }
            return null;
        }

        fn insert(tree: *Self, value: Value) !void {
            var new = try tree.allocator.create(Node);
            new.* = Node{ .left = null, .right = null, .parent = undefined, .value = value, .bf = 0 };

            var parent = tree.root orelse {
                new.parent = null;
                tree.root = new;
                return;
            };
            while (true) {
                if (value < parent.value) {
                    parent = parent.left orelse {
                        parent.left = new;
                        parent.bf -= 1;
                        break;
                    };
                } else if (value > parent.value) {
                    parent = parent.right orelse {
                        parent.right = new;
                        parent.bf += 1;
                        break;
                    };
                } else {
                    unreachable;
                }
            }

            new.parent = parent;
            if (rebalanceAfterInsert(parent, new)) |new_root| {
                tree.root = new_root;
            }
        }

        fn rebalanceAfterInsert(parent_node: *Node, inserted_node: *Node) ?*Node {
            if (parent_node.bf == 0) return null;

            const value = inserted_node.value;
            var parent = parent_node;
            var child = inserted_node;
            while (parent.parent) |grandparent| {
                const direction_int: i2 = if (value < grandparent.value) -1 else 1;

                if (grandparent.bf == -2) {
                    unreachable;
                } else if (grandparent.bf == 0) {
                    grandparent.bf = direction_int;
                } else if (grandparent.bf == -direction_int) {
                    grandparent.bf = 0;
                    return null;
                } else if (grandparent.bf == direction_int) {
                    var grandparent_edge: Edge = undefined;
                    var parent_edge: Edge = undefined;
                    var child_first_edge: Edge = undefined;
                    var child_second_edge: Edge = undefined;
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
                    if (parent.bf == direction_int) {
                        // direction_int == -1: clockwise rotation
                        // direction_int == +1: counter-clockwise rotation
                        grandparent_edge.* = parent_edge.*;
                        if (parent_edge.*) |n| n.parent = grandparent;
                        parent_edge.* = grandparent;
                        parent.parent = grandparent.parent;
                        grandparent.parent = parent;

                        grandparent.bf = 0;
                        parent.bf = 0;

                        new_subtree_root = parent;
                    } else if (parent.bf == -direction_int) {
                        // direction_int == -1: counter-clockwise rotation
                        // direction_int == +1: clockwise rotation
                        parent_edge.* = child_first_edge.*;
                        if (child_first_edge.*) |n| n.parent = parent;
                        child_first_edge.* = parent;
                        parent.parent = child;

                        // direction_int == -1: clockwise rotation
                        // direction_int == +1: counter-clockwise rotation
                        grandparent_edge.* = child_second_edge.*;
                        if (child_second_edge.*) |n| n.parent = grandparent;
                        child_second_edge.* = grandparent;
                        child.parent = grandparent.parent;
                        grandparent.parent = child;

                        if (child.bf == direction_int) {
                            grandparent.bf = -direction_int;
                            parent.bf = 0;
                        } else if (child.bf == -direction_int) {
                            grandparent.bf = 0;
                            parent.bf = direction_int;
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
                        return null;
                    } else {
                        return new_subtree_root;
                    }
                }

                child = parent;
                parent = grandparent;
            }

            return null;
        }

        fn delete(tree: *Self, value: Value) void {
            var node: *Node = tree.root orelse unreachable;
            var direction_int: i2 = undefined;
            while (value != node.value) {
                if (value < node.value) {
                    node = node.left.?;
                    direction_int = -1;
                } else if (value > node.value) {
                    node = node.right.?;
                    direction_int = 1;
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
                direction_int = 1;
                while (successor.left) |next| {
                    successor = next;
                    direction_int = -1;
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
                } else if (node.bf == direction_int) {
                    node.bf = 0;
                } else if (node.bf == 0) {
                    node.bf = -direction_int;
                    break;
                } else if (node.bf == -direction_int) {
                    var node_edge: Edge = undefined;
                    var child: *Node = undefined;
                    var child_edge: Edge = undefined;
                    if (direction_int == -1) {
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
                    } else if (child.bf == 0 or child.bf == -direction_int) {
                        // direction_int == -1: counter-clockwise rotation
                        // direction_int == +1: clockwise rotation
                        node_edge.* = child_edge.*;
                        if (child_edge.*) |n| n.parent = node;
                        child_edge.* = node;
                        child.parent = node.parent;
                        node.parent = child;
                        node.bf = -direction_int * @boolToInt(stop);
                        child.bf = -node.bf;
                        new_subtree_root = child;
                    } else if (child.bf == direction_int) {
                        const grandchild = child_edge.*.?;
                        var grandchild_first_edge: Edge = undefined;
                        var grandchild_second_edge: Edge = undefined;
                        if (direction_int == -1) {
                            grandchild_first_edge = &grandchild.right;
                            grandchild_second_edge = &grandchild.left;
                        } else {
                            grandchild_first_edge = &grandchild.left;
                            grandchild_second_edge = &grandchild.right;
                        }

                        // direction_int == -1: clockwise rotation
                        // direction_int == +1: counter-clockwise rotation
                        child_edge.* = grandchild_first_edge.*;
                        if (grandchild_first_edge.*) |n| n.parent = child;
                        grandchild_first_edge.* = child;
                        child.parent = grandchild;

                        // direction_int == -1: counter-clockwise rotation
                        // direction_int == +1: clockwise rotation
                        node_edge.* = grandchild_second_edge.*;
                        if (grandchild_second_edge.*) |n| n.parent = node;
                        grandchild_second_edge.* = node;
                        grandchild.parent = node.parent;
                        node.parent = grandchild;

                        if (grandchild.bf == direction_int) {
                            node.bf = 0;
                            child.bf = -direction_int;
                        } else if (grandchild.bf == -direction_int) {
                            node.bf = direction_int;
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
                    direction_int = direction_int: {
                        if (successor_value) |s| {
                            if (node.value == s) break :direction_int 1;
                        }
                        break :direction_int if (value < node.value) @as(i2, -1) else 1;
                    };
                    if (direction_int == -1) {
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
                direction_int = direction_int: {
                    if (successor_value) |s| {
                        if (node.value == s) break :direction_int 1;
                    }
                    break :direction_int if (value < node.value) @as(i2, -1) else 1;
                };
            }
        }

        fn join(left: *Self, value: Value, right: *Self) !Self {
            assert(std.meta.eql(left.allocator, right.allocator));
            const allocator = left.allocator;
            const new = try allocator.create(Node);
            const left_height = left.height();
            const right_height = right.height();

            const root = root: {
                switch (std.math.order(left_height, right_height)) {
                    .gt => if (left_height > right_height + 1) {
                        break :root joinRight(left.*, value, right.*, new, left_height, right_height) orelse left.root.?;
                    } else {
                        new.bf = -1;
                    },
                    .lt => if (right_height > left_height + 1) {
                        panic("TODO\n", .{});
                    } else {
                        new.bf = 1;
                    },
                    .eq => new.bf = 0,
                }

                if (left.root) |r| r.parent = new;
                if (right.root) |r| r.parent = new;
                new.left = left.root;
                new.right = right.root;
                new.parent = null;
                new.value = value;
                break :root new;
            };

            left.* = undefined;
            right.* = undefined;
            return Self{ .root = root, .allocator = allocator };
        }

        fn joinRight(left: Self, value: Value, right: Self, new: *Node, left_height: usize, right_height: usize) ?*Node {
            var node = left.root.?;
            var current_height = left_height;
            while (current_height > right_height + 1) {
                switch (node.bf) {
                    -2 => unreachable,
                    -1 => {
                        node = node.right orelse {
                            assert(current_height == 2);
                            assert(right_height == 0);
                            new.* = Node{ .left = null, .right = null, .parent = node, .bf = 0, .value = value };
                            node.right = new;
                            node.bf = 0;
                            return null;
                        };
                        current_height -= 2;
                    },
                    0, 1 => {
                        node = node.right.?;
                        current_height -= 1;
                    },
                }
            }

            const parent = node.parent.?;
            new.* = Node{
                .left = node,
                .right = right.root,
                .parent = parent,
                .bf = @intCast(i2, @bitCast(isize, right_height -% current_height)),
                .value = value,
            };
            if (right.root) |r| r.parent = new;
            node.parent = new;
            parent.right = new;
            parent.bf += 1; // TODO: parent.bf could've been 1
            return rebalanceAfterInsert(parent, new);
        }

        const testing = struct {
            fn print(tree: Self, writer: anytype, comptime fmt: []const u8) !void {
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
                    try writer.print("{" ++ fmt ++ "}", .{node.value});
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
                        .{ @tagName(item.which), node.value, node.bf, if (node.parent) |p| @as(?Value, p.value) else null },
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
                    try showTree(tree, writer, fmt);
                }
            };

            fn verifyTree(tree: Self) !?VerifyTreeFailure {
                var stack = ArrayList(struct {
                    node: *const Node,
                    height_left: usize,
                    height_right: usize,
                    direction: enum { down, left, right },
                }).init(tree.allocator);
                defer stack.deinit();

                var visitied_nodes = std.AutoHashMap(*const Node, void).init(tree.allocator);
                defer visitied_nodes.deinit();

                {
                    const root = tree.root orelse return null;
                    if (root.parent) |_| return VerifyTreeFailure{ .node = root, .reason = .RootHasParent };
                    try stack.append(.{ .node = root, .height_left = undefined, .height_right = undefined, .direction = .down });
                    try visitied_nodes.put(root, {});
                }

                while (stack.items.len > 0) {
                    const this = &stack.items[stack.items.len - 1];
                    if (this.direction == .down) {
                        if (this.node.bf == -2) {
                            return VerifyTreeFailure{ .node = this.node, .reason = .WrongBalanceFactor };
                        }

                        if (stack.items.len > 1) {
                            const parent = stack.items[stack.items.len - 2];

                            if (try visitied_nodes.fetchPut(this.node, {})) |_| {
                                return VerifyTreeFailure{ .node = parent.node, .reason = .Cycle };
                            }

                            if (this.node.parent) |node_parent| {
                                if (node_parent != parent.node) return VerifyTreeFailure{ .node = this.node, .reason = .WrongParent };
                            } else {
                                return VerifyTreeFailure{ .node = this.node, .reason = .MissingParent };
                            }

                            if (this.node.value == parent.node.value) {
                                return VerifyTreeFailure{ .node = this.node, .reason = .DuplicateValue };
                            }

                            if ((parent.direction == .left and this.node.value > parent.node.value) or
                                (parent.direction == .right and this.node.value < parent.node.value))
                            {
                                return VerifyTreeFailure{ .node = this.node, .reason = .WrongDirection };
                            }
                        }

                        this.direction = .left;
                        if (this.node.left) |left| {
                            try stack.append(.{ .node = left, .height_left = undefined, .height_right = undefined, .direction = .down });
                            continue;
                        } else {
                            this.height_left = 0;
                        }
                    }

                    if (this.direction == .left) {
                        this.direction = .right;
                        if (this.node.right) |right| {
                            try stack.append(.{ .node = right, .height_left = undefined, .height_right = undefined, .direction = .down });
                            continue;
                        } else {
                            this.height_right = 0;
                        }
                    }

                    if (std.math.max(this.height_left, this.height_right) - std.math.min(this.height_left, this.height_right) > 1) {
                        return VerifyTreeFailure{ .node = this.node, .reason = .HeightDifferenceTooLarge };
                    }

                    if (!switch (this.node.bf) {
                        -2 => unreachable,
                        -1 => this.height_left > this.height_right,
                        0 => this.height_left == this.height_right,
                        1 => this.height_left < this.height_right,
                    }) {
                        return VerifyTreeFailure{ .node = this.node, .reason = .WrongBalanceFactor };
                    }

                    _ = stack.pop();
                    if (stack.items.len > 0) {
                        const this_height = std.math.max(this.height_left, this.height_right);
                        const parent = &stack.items[stack.items.len - 1];
                        switch (parent.direction) {
                            .left => parent.height_left = this_height + 1,
                            .right => parent.height_right = this_height + 1,
                            else => unreachable,
                        }
                    }
                }

                return null;
            }
        };
    };
}

test "AvlTree basic usage" {
    var tree = AvlTree(u8).init(std.testing.allocator);
    defer tree.deinit();

    try tree.insert('a');
    try tree.insert('f');
    try tree.insert(0);
    assert(tree.find('a') != null);
    tree.delete('a');
    assert(tree.find('a') == null);
    try tree.insert(42);
    try tree.insert('r');
    tree.delete(0);
}

test "small string" {
    const allocator = std.testing.allocator;
    const writer = std.io.getStdErr().writer();

    const Tree = AvlTree(u8);
    var tree = Tree.init(allocator);
    defer tree.deinit();

    for ("acbdmzGAXlkw987654") |c| {
        try tree.insert(c);
        if (try Tree.testing.verifyTree(tree)) |failure| {
            try writer.print("Failed to insert value: {c}\n", .{c});
            try failure.print(writer, "c", tree);
            return error.InvalidTree;
        }
    }

    for ("acbdmzGAXlkw987654") |c| {
        tree.delete(c);
        if (try Tree.testing.verifyTree(tree)) |failure| {
            try writer.print("Failed to delete value: {c}\n", .{c});
            try failure.print(writer, "c", tree);
            return error.InvalidTree;
        }
    }
}

test "500 random values" {
    // Create an AVL tree with 500 random values, then
    // delete them all in a random order.
    // Check the tree after every insertion/deletion.

    const allocator = std.testing.allocator;
    const writer = std.io.getStdErr().writer();

    var rng = std.rand.DefaultPrng.init(0);
    const random = rng.random();

    const Value = u16;
    const Tree = AvlTree(Value);
    var tree = Tree.init(allocator);
    defer tree.deinit();

    var list: [500]Value = undefined;
    var list_len: u16 = 0;
    var i: u16 = 0;
    while (i < list.len) : (i += 1) {
        const value = random.int(Value);
        if (tree.find(value) == null) {
            list[list_len] = value;
            list_len += 1;
            try tree.insert(value);
            if (try Tree.testing.verifyTree(tree)) |failure| {
                try writer.print("Failed to delete value: {}\n", .{value});
                try failure.print(writer, "", tree);
                return error.InvalidTree;
            }
        }
    }

    i = list_len;
    while (i > 0) : (i -= 1) {
        const int = random.uintAtMostBiased(u16, i - 1);
        const value = list[int];
        assert(tree.find(value) != null);
        std.mem.swap(Value, &list[int], &list[i - 1]);
        tree.delete(value);
        if (try Tree.testing.verifyTree(tree)) |failure| {
            try writer.print("Failed to delete value: {}\n", .{value});
            try failure.print(writer, "", tree);
            return error.InvalidTree;
        }
    }
}

test "join" {
    const allocator = std.testing.allocator;
    const writer = std.io.getStdErr().writer();

    const Value = u8;
    const Tree = AvlTree(Value);
    var left = Tree.init(allocator);
    var right = Tree.init(allocator);

    {
        errdefer left.deinit();
        for ("abcdefghijk") |c| try left.insert(c);
    }
    {
        errdefer right.deinit();
        for ("wxyz") |c| try right.insert(c);
    }

    var joined = Tree.join(&left, 'm', &right) catch |err| {
        left.deinit();
        right.deinit();
        return err;
    };
    defer joined.deinit();

    if (try Tree.testing.verifyTree(joined)) |failure| {
        try writer.print("Failed to join trees\n", .{});
        try failure.print(writer, "c", joined);
        return error.InvalidTree;
    } else {
        try Tree.testing.showTree(joined, writer, "c");
        try writer.writeAll("\n");
        try Tree.testing.print(joined, writer, "c");
        try writer.writeAll("\n");
    }

    {
        left = Tree.init(allocator);
        defer left.deinit();

        for ("abcdefgh") |c| {
            right = Tree.init(allocator);
            left = try Tree.join(&left, c, &right);
            if (try Tree.testing.verifyTree(left)) |failure| {
                try writer.print("Failed to join trees: {c}\n", .{c});
                try failure.print(writer, "c", left);
                return error.InvalidTree;
            }
        }

        try Tree.testing.showTree(left, writer, "c");
        try writer.writeAll("\n");
        try Tree.testing.print(left, writer, "c");
        try writer.writeAll("\n");
    }
}
