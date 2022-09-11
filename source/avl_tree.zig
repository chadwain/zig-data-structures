const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub fn AvlTree(comptime Value: type) type {
    const N = struct {
        left: ?*Self = null,
        right: ?*Self = null,
        parent: ?*Self = null,
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
                            node.bf = -choice_int;
                            child.bf = 0;
                        } else if (grandchild.bf == -choice_int) {
                            node.bf = 0;
                            child.bf = choice_int;
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
                try writer.print("({s} node) value={c}, bf={}, parent={?c}\n", .{ @tagName(item.which), node.value, node.bf, if (node.parent) |p| @as(?u8, p.value) else null });

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
        tree.delete(c);
        try tree.print(writer);
        try writer.writeByte('\n');
    }
}
