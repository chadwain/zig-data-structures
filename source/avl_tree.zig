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

        pub fn format(node: Self, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
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
        height: usize = 0,
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
            const new = try tree.allocator.create(Node);
            new.* = Node{ .left = null, .right = null, .parent = undefined, .value = value, .bf = 0 };

            var parent = tree.root orelse {
                new.parent = null;
                tree.root = new;
                tree.height = 1;
                return;
            };
            while (true) {
                if (value < parent.value) {
                    parent = parent.left orelse {
                        parent.left = new;
                        break;
                    };
                } else if (value > parent.value) {
                    parent = parent.right orelse {
                        parent.right = new;
                        break;
                    };
                } else {
                    unreachable;
                }
            }

            new.parent = parent;
            rebalanceAfterInsert(tree, parent, value);
            testing.doCheck(tree.*, "insert", value);
        }

        fn rebalanceAfterInsert(tree: *Self, parent_node: *Node, value: Value) void {
            var parent = parent_node;
            while (true) {
                const direction_int: i2 = if (value < parent.value) -1 else 1;

                if (parent.bf == -2) {
                    unreachable;
                } else if (parent.bf == 0) {
                    parent.bf = direction_int;
                    parent = parent.parent orelse {
                        tree.height += 1;
                        return;
                    };
                } else if (parent.bf == -direction_int) {
                    parent.bf = 0;
                    return;
                } else if (parent.bf == direction_int) {
                    var parent_edge: Edge = undefined;
                    var child: *Node = undefined;
                    var child_edge: Edge = undefined;
                    if (direction_int == -1) {
                        parent_edge = &parent.left;
                        child = parent.left.?;
                        child_edge = &child.right;
                    } else {
                        parent_edge = &parent.right;
                        child = parent.right.?;
                        child_edge = &child.left;
                    }

                    var new_subtree_root: *Node = undefined;
                    if (child.bf == direction_int) {
                        // direction_int == -1: clockwise rotation
                        // direction_int == +1: counter-clockwise rotation
                        parent_edge.* = child_edge.*;
                        if (child_edge.*) |n| n.parent = parent;
                        child_edge.* = parent;
                        child.parent = parent.parent;
                        parent.parent = child;

                        parent.bf = 0;
                        child.bf = 0;

                        new_subtree_root = child;
                    } else if (child.bf == -direction_int) {
                        const grandchild = child_edge.*.?;
                        var grandchild_first_edge: Edge = undefined;
                        var grandchild_second_edge: Edge = undefined;
                        if (direction_int == -1) {
                            grandchild_first_edge = &grandchild.left;
                            grandchild_second_edge = &grandchild.right;
                        } else {
                            grandchild_first_edge = &grandchild.right;
                            grandchild_second_edge = &grandchild.left;
                        }

                        // direction_int == -1: counter-clockwise rotation
                        // direction_int == +1: clockwise rotation
                        child_edge.* = grandchild_first_edge.*;
                        if (grandchild_first_edge.*) |n| n.parent = child;
                        grandchild_first_edge.* = child;
                        child.parent = grandchild;

                        // direction_int == -1: clockwise rotation
                        // direction_int == +1: counter-clockwise rotation
                        parent_edge.* = grandchild_second_edge.*;
                        if (grandchild_second_edge.*) |n| n.parent = parent;
                        grandchild_second_edge.* = parent;
                        grandchild.parent = parent.parent;
                        parent.parent = grandchild;

                        if (grandchild.bf == direction_int) {
                            parent.bf = -direction_int;
                            child.bf = 0;
                        } else if (grandchild.bf == -direction_int) {
                            parent.bf = 0;
                            child.bf = direction_int;
                        } else if (grandchild.bf == 0) {
                            // Only reachable on the second iteration of this loop.
                            assert(child == parent_node);
                            parent.bf = 0;
                            child.bf = 0;
                        } else if (grandchild.bf == -2) {
                            unreachable;
                        }
                        grandchild.bf = 0;

                        new_subtree_root = grandchild;
                    } else {
                        unreachable;
                    }

                    parent = new_subtree_root.parent orelse {
                        tree.root = new_subtree_root;
                        return;
                    };
                    if (value < parent.value) {
                        parent.left = new_subtree_root;
                    } else {
                        parent.right = new_subtree_root;
                    }
                    return;
                }
            }
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
                        tree.height = 0;
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
                    var decrease_height: bool = undefined;
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
                        decrease_height = !stop;
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
                        decrease_height = true;
                    }

                    node = new_subtree_root.parent orelse {
                        tree.root = new_subtree_root;
                        tree.height -= @boolToInt(decrease_height);
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

                node = node.parent orelse {
                    tree.height -= 1;
                    return;
                };
                direction_int = direction_int: {
                    if (successor_value) |s| {
                        if (node.value == s) break :direction_int 1;
                    }
                    break :direction_int if (value < node.value) @as(i2, -1) else 1;
                };
            }
            testing.doCheck(tree.*, "delete", value);
        }

        fn join(left: *Self, value: Value, right: *Self) !Self {
            assert(std.meta.eql(left.allocator, right.allocator));
            const allocator = left.allocator;
            const new = try allocator.create(Node);

            const result = result: {
                switch (std.math.order(left.height, right.height)) {
                    .gt => if (left.height > right.height + 1) {
                        joinGeneric(.left, left, value, right, new);
                        break :result left.*;
                    } else {
                        new.bf = -1;
                    },
                    .lt => if (right.height > left.height + 1) {
                        joinGeneric(.right, left, value, right, new);
                        break :result right.*;
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
                break :result Self{ .root = new, .height = 1 + std.math.max(left.height, right.height), .allocator = left.allocator };
            };

            left.* = undefined;
            right.* = undefined;

            testing.doCheck(result, "join", value);
            return result;
        }

        // TODO: stage1 won't compile without this
        const Taller = enum { left, right };

        fn joinGeneric(comptime taller: Taller, left: *Self, value: Value, right: *Self, new: *Node) void {
            const taller_tree = if (taller == .left) left else right;
            const shorter_tree = if (taller == .left) right else left;
            const direction_int = if (taller == .left) -1 else 1;
            const travel_direction = if (taller == .left) "right" else "left";

            var node = taller_tree.root.?;
            var current_height = taller_tree.height;
            while (current_height > shorter_tree.height + 1) {
                switch (node.bf) {
                    -2 => unreachable,
                    direction_int => {
                        node = @field(node, travel_direction) orelse {
                            assert(current_height == 2);
                            assert(shorter_tree.height == 0);
                            new.* = Node{ .left = null, .right = null, .parent = node, .bf = 0, .value = value };
                            @field(node, travel_direction) = new;
                            node.bf = 0;
                            return;
                        };
                        current_height -= 2;
                    },
                    0, -direction_int => {
                        node = @field(node, travel_direction).?;
                        current_height -= 1;
                    },
                }
            }

            const parent = node.parent.?;
            new.* = Node{
                .left = if (taller == .left) node else left.root,
                .right = if (taller == .left) right.root else node,
                .parent = parent,
                .bf = @intCast(i2, @bitCast(isize, if (taller == .left) right.height -% current_height else current_height -% left.height)),
                .value = value,
            };
            if (shorter_tree.root) |r| r.parent = new;
            node.parent = new;
            @field(parent, travel_direction) = new;
            rebalanceAfterInsert(taller_tree, parent, value);
        }

        const format = testing.showTree;

        const testing = struct {
            const pedantic_checking = true;
            const check_fmt = "";

            fn print(tree: Self, writer: anytype, comptime fmt: []const u8) !void {
                var node = tree.root orelse return;
                while (node.left) |left| {
                    node = left;
                }

                while (true) {
                    try writer.print("{" ++ fmt ++ "}", .{node.value});
                    if (node.right) |right| {
                        node = right;
                        while (node.left) |left| {
                            node = left;
                        }
                    } else {
                        while (true) {
                            const parent = node.parent orelse return;
                            const is_left = node == parent.left;
                            node = parent;
                            if (is_left) break;
                        }
                    }
                }
            }

            fn showTree(tree: Self, comptime fmt: []const u8) !void {
                var stack = ArrayList(struct { node: *const Node, indent: usize, which: enum { root, left, right } }).init(tree.allocator);
                defer stack.deinit();

                if (tree.root) |root| {
                    try stack.append(.{ .node = root, .indent = 0, .which = .root });
                }
                while (stack.items.len > 0) {
                    const item = stack.pop();
                    const node = item.node;
                    for (@as([*]u0, undefined)[0 .. item.indent * 4]) |_| std.debug.print(" ", .{});
                    std.debug.print("({s} node) {" ++ fmt ++ "}\n", .{ @tagName(item.which), node });

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
                    HeightFieldIsWrong,
                    DuplicateValue,
                    Cycle,
                },
            };

            fn doCheck(tree: Self, comptime operation: []const u8, context: anytype) void {
                if (comptime pedantic_checking) {
                    _ = @as(Allocator.Error!void, blk: {
                        if (verifyTree(tree) catch |err| break :blk err) |failure| {
                            std.debug.print(
                                "Invalid tree detected: {s}\nOperation: " ++ operation ++ "\nContext: {}\nWhile checking this node:\n{" ++ check_fmt ++ "}\n\n",
                                .{ @tagName(failure.reason), context, failure.node },
                            );
                            showTree(tree, check_fmt) catch |err| break :blk err;
                            std.debug.print("\n", .{});
                            unreachable;
                        }
                    }) catch |err| panic("Error while verifying tree integrity: {s}\n", .{@errorName(err)});
                }
            }

            /// Check that the tree satisfies the properties of a binary tree, binary search tree, and AVL Tree.
            fn verifyTree(tree: Self) !?VerifyTreeFailure {
                var stack = ArrayList(struct {
                    node: *const Node,
                    left_height: usize,
                    right_height: usize,
                    direction: enum { down, left, right },
                }).init(tree.allocator);
                defer stack.deinit();

                var visited_nodes = std.AutoHashMap(*const Node, void).init(tree.allocator);
                defer visited_nodes.deinit();

                const root = tree.root orelse return null;
                if (root.parent) |_| return VerifyTreeFailure{ .node = root, .reason = .RootHasParent };
                try stack.append(.{ .node = root, .left_height = undefined, .right_height = undefined, .direction = .down });

                while (stack.items.len > 0) {
                    const this = &stack.items[stack.items.len - 1];
                    if (this.direction == .down) {
                        if (this.node.bf == -2) {
                            return VerifyTreeFailure{ .node = this.node, .reason = .WrongBalanceFactor };
                        }

                        if (try visited_nodes.fetchPut(this.node, {})) |_| {
                            return VerifyTreeFailure{ .node = this.node, .reason = .Cycle };
                        }

                        if (stack.items.len > 1) {
                            const parent = stack.items[stack.items.len - 2];

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
                            try stack.append(.{ .node = left, .left_height = undefined, .right_height = undefined, .direction = .down });
                            continue;
                        } else {
                            this.left_height = 0;
                        }
                    }

                    if (this.direction == .left) {
                        this.direction = .right;
                        if (this.node.right) |right| {
                            try stack.append(.{ .node = right, .left_height = undefined, .right_height = undefined, .direction = .down });
                            continue;
                        } else {
                            this.right_height = 0;
                        }
                    }

                    if (std.math.max(this.left_height, this.right_height) - std.math.min(this.left_height, this.right_height) > 1) {
                        return VerifyTreeFailure{ .node = this.node, .reason = .HeightDifferenceTooLarge };
                    }

                    if (!switch (this.node.bf) {
                        -2 => unreachable,
                        -1 => this.left_height > this.right_height,
                        0 => this.left_height == this.right_height,
                        1 => this.left_height < this.right_height,
                    }) {
                        return VerifyTreeFailure{ .node = this.node, .reason = .WrongBalanceFactor };
                    }

                    const this_height = 1 + std.math.max(this.left_height, this.right_height);
                    _ = stack.pop();
                    if (stack.items.len > 0) {
                        const parent = &stack.items[stack.items.len - 1];
                        switch (parent.direction) {
                            .left => parent.left_height = this_height,
                            .right => parent.right_height = this_height,
                            else => unreachable,
                        }
                    } else {
                        if (this_height != tree.height) {
                            return VerifyTreeFailure{ .node = root, .reason = .HeightFieldIsWrong };
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

    const Tree = AvlTree(u8);
    var tree = Tree.init(allocator);
    defer tree.deinit();

    for ("acbdmzGAXlkw987654") |c| {
        try tree.insert(c);
    }

    for ("acbdmzGAXlkw987654") |c| {
        tree.delete(c);
    }
}

test "500 random values" {
    // Create an AVL tree with 500 random values, then
    // delete them all in a random order.
    // Check the tree after every insertion/deletion.

    const allocator = std.testing.allocator;

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
        }
    }

    i = list_len;
    while (i > 0) : (i -= 1) {
        const int = random.uintAtMostBiased(u16, i - 1);
        const value = list[int];
        assert(tree.find(value) != null);
        std.mem.swap(Value, &list[int], &list[i - 1]);
        tree.delete(value);
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

        {
            left = Tree.init(allocator);
            defer left.deinit();

            for ("abcdefgh") |c| {
                right = Tree.init(allocator);
                left = try Tree.join(&left, c, &right);
            }

            try Tree.testing.showTree(left, "c");
            try writer.writeAll("\n");
            try Tree.testing.print(left, writer, "c");
            try writer.writeAll("\n");
        }
    }

    {
        left = Tree.init(allocator);
        right = Tree.init(allocator);
        {
            errdefer left.deinit();
            for ("abcd") |c| try left.insert(c);
        }
        {
            errdefer right.deinit();
            for ("pqrstuvwxyz") |c| try right.insert(c);
        }

        var joined = Tree.join(&left, 'm', &right) catch |err| {
            left.deinit();
            right.deinit();
            return err;
        };
        defer joined.deinit();

        {
            right = Tree.init(allocator);
            defer right.deinit();

            for ("zyxwvuts") |c| {
                left = Tree.init(allocator);
                right = try Tree.join(&left, c, &right);
            }

            try Tree.testing.showTree(right, "c");
            try writer.writeAll("\n");
            try Tree.testing.print(right, writer, "c");
            try writer.writeAll("\n");
        }
    }
}
