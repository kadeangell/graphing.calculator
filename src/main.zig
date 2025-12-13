const std = @import("std");
const rl = @import("raylib");
const rg = @import("raygui");
const equation = @import("equation.zig");

const Equation = equation.Equation;

pub fn main() anyerror!void {
    const screen_width: i32 = 1200;
    const screen_height: i32 = 900;

    rl.setConfigFlags(.{
        .window_resizable = true,
    });
    rl.initWindow(screen_width, screen_height, "Graphing Engine");
    defer rl.closeWindow();

    rl.setTargetFPS(60);

    const grid_spacing: i32 = 50;
    const grid_color = rl.Color.light_gray;
    const axis_color = rl.Color.dark_gray;
    const sidebar_width: i32 = 250;

    // Setup allocator for equation list
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Equation list
    var equations = std.ArrayList(*Equation){};
    defer {
        for (equations.items) |eq| {
            allocator.destroy(eq);
        }
        equations.deinit(allocator);
    }

    // Color palette for equations
    const colors = [_]rl.Color{
        rl.Color.red,
        rl.Color.blue,
        rl.Color.green,
        rl.Color.purple,
        rl.Color.orange,
        rl.Color.pink,
    };

    // Start with one equation
    const first_eq = try allocator.create(Equation);
    first_eq.* = Equation.init(colors[0]);
    try equations.append(allocator, first_eq);

    while (!rl.windowShouldClose()) {
        const width = rl.getScreenWidth();
        const height = rl.getScreenHeight();

        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(.white);

        // Calculate center position
        const center_x = @divTrunc(width, 2);
        const center_y = @divTrunc(height, 2);

        // Draw vertical grid lines from center outward
        var x: i32 = center_x;
        while (x <= width) : (x += grid_spacing) {
            rl.drawLine(x, 0, x, height, grid_color);
        }
        x = center_x - grid_spacing;
        while (x >= 0) : (x -= grid_spacing) {
            rl.drawLine(x, 0, x, height, grid_color);
        }

        // Draw horizontal grid lines from center outward
        var y: i32 = center_y;
        while (y <= height) : (y += grid_spacing) {
            rl.drawLine(0, y, width, y, grid_color);
        }
        y = center_y - grid_spacing;
        while (y >= 0) : (y -= grid_spacing) {
            rl.drawLine(0, y, width, y, grid_color);
        }

        // Draw axes (centered)
        rl.drawLine(center_x, 0, center_x, height, axis_color);
        rl.drawLine(0, center_y, width, center_y, axis_color);

        // Plot equations
        const center_x_f: f32 = @floatFromInt(center_x);
        const center_y_f: f32 = @floatFromInt(center_y);
        const grid_spacing_f: f32 = @floatFromInt(grid_spacing);

        // Create evaluation context with x and y variables
        var eval_context = std.StringHashMap(f32).init(allocator);
        defer eval_context.deinit();
        try eval_context.put("x", 0.0);
        try eval_context.put("y", 0.0);

        // Build dependency graph and evaluate assignments
        var dep_graph = equation.DependencyGraph.init(allocator);
        defer dep_graph.deinit();

        try dep_graph.build(equations.items);
        const eval_order = dep_graph.topologicalSort(equations.items.len) catch |err| blk: {
            // If there's a circular dependency, just use original order
            if (err == error.CircularDependency) {
                var order = try allocator.alloc(usize, equations.items.len);
                for (0..equations.items.len) |i| {
                    order[i] = i;
                }
                break :blk order;
            } else {
                return err;
            }
        };
        defer allocator.free(eval_order);

        // Evaluate assignments in dependency order
        for (eval_order) |idx| {
            const eq = equations.items[idx];
            if (eq.equation_type == .assignment) {
                if (eq.evaluate(eval_context)) |value| {
                    // Get the variable name from the assignment
                    if (eq.equation_ast) |eq_ast| {
                        if (eq_ast == .assignment) {
                            try eval_context.put(eq_ast.assignment.var_name, value);
                        }
                    }
                } else |_| {
                    // Ignore errors in assignments for now
                }
            }
        }

        // Plot equations
        for (equations.items) |eq| {
            // Skip equations with errors or no AST
            if (eq.has_error or eq.equation_ast == null) continue;

            // Skip assignments (they don't plot)
            if (eq.equation_type == .assignment) continue;

            // Handle different equation types
            if (eq.equation_type == .constraint_implicit) {
                // Try simple constraint solving first (x = y^2 style)
                const can_solve_simple = blk: {
                    _ = eq.evaluateConstraint(eval_context, "x") catch {
                        break :blk false;
                    };
                    break :blk true;
                };

                if (can_solve_simple) {
                    // Simple constraint: sample y, solve for x
                    var prev_screen_x: ?f32 = null;
                    var prev_screen_y: ?f32 = null;

                    var screen_y: f32 = 0.0;
                    const height_f: f32 = @floatFromInt(height);
                    while (screen_y < height_f) : (screen_y += 1.0) {
                        const graph_y = (center_y_f - screen_y) / grid_spacing_f;
                        try eval_context.put("y", graph_y);

                        const graph_x = eq.evaluateConstraint(eval_context, "x") catch continue;
                        const screen_x = center_x_f + (graph_x * grid_spacing_f);

                        if (screen_x < @as(f32, @floatFromInt(sidebar_width)) or screen_x >= @as(f32, @floatFromInt(width))) {
                            prev_screen_x = null;
                            prev_screen_y = null;
                            continue;
                        }

                        if (prev_screen_x) |px| {
                            if (prev_screen_y) |py| {
                                if (@abs(screen_x - px) < 1000) {
                                    rl.drawLineEx(
                                        .{ .x = px, .y = py },
                                        .{ .x = screen_x, .y = screen_y },
                                        2.0,
                                        eq.color
                                    );
                                }
                            }
                        }

                        prev_screen_x = screen_x;
                        prev_screen_y = screen_y;
                    }
                } else {
                    // Complex constraint (like x^2+y^2=r^2): use grid sampling
                    const tolerance: f32 = 0.5; // Tolerance for equality check
                    const step: f32 = 2.0; // Sample every 2 pixels for performance

                    var screen_y: f32 = 0.0;
                    const height_f: f32 = @floatFromInt(height);
                    while (screen_y < height_f) : (screen_y += step) {
                        var screen_x: f32 = @floatFromInt(sidebar_width);
                        while (screen_x < @as(f32, @floatFromInt(width))) : (screen_x += step) {
                            // Convert to graph coordinates
                            const graph_x = (screen_x - center_x_f) / grid_spacing_f;
                            const graph_y = (center_y_f - screen_y) / grid_spacing_f;

                            try eval_context.put("x", graph_x);
                            try eval_context.put("y", graph_y);

                            // Check if this point satisfies the constraint
                            if (eq.checkConstraintSatisfied(eval_context, tolerance) catch false) {
                                // Draw a small point
                                rl.drawCircle(@intFromFloat(screen_x), @intFromFloat(screen_y), 1.5, eq.color);
                            }
                        }
                    }
                }
            } else {
                // Plot regular functions (implicit and explicit)
                var prev_screen_x: ?f32 = null;
                var prev_screen_y: ?f32 = null;

                var screen_x: f32 = @floatFromInt(sidebar_width);
                while (screen_x < @as(f32, @floatFromInt(width))) : (screen_x += 1.0) {
                    // Convert screen x to graph x
                    const graph_x = (screen_x - center_x_f) / grid_spacing_f;

                    // Update x in context
                    try eval_context.put("x", graph_x);

                    // Evaluate function using context
                    const graph_y = eq.evaluate(eval_context) catch continue;

                    // Convert graph y to screen y
                    const screen_y = center_y_f - (graph_y * grid_spacing_f);

                    // Draw line from previous point
                    if (prev_screen_x) |px| {
                        if (prev_screen_y) |py| {
                            // Only draw if both points are reasonable
                            if (@abs(screen_y - py) < 1000) {
                                rl.drawLineEx(
                                    .{ .x = px, .y = py },
                                    .{ .x = screen_x, .y = screen_y },
                                    2.0,
                                    eq.color
                                );
                            }
                        }
                    }

                    prev_screen_x = screen_x;
                    prev_screen_y = screen_y;
                }
            }
        }

        // Draw sidebar (on the left)
        const sidebar_x: i32 = 0;
        rl.drawRectangle(sidebar_x, 0, sidebar_width, height, rl.Color.init(240, 240, 240, 255));

        // Sidebar content
        _ = rg.label(.init(10, 20, 230, 20), "Equations:");

        // Add equation button
        if (rg.button(.init(10, 50, 230, 30), "+ Add Equation")) {
            const new_eq = try allocator.create(Equation);
            const color_idx = equations.items.len % colors.len;
            new_eq.* = Equation.init(colors[color_idx]);
            try equations.append(allocator, new_eq);
        }

        // Display all equations
        var eq_y: f32 = 90;
        var i: usize = 0;
        while (i < equations.items.len) {
            const eq = equations.items[i];

            // Equation text box
            if (rg.textBox(.init(10, eq_y, 190, 30), &eq.function, 256, eq.edit_mode)) {
                eq.edit_mode = !eq.edit_mode;
                if (!eq.edit_mode) {
                    // Exiting edit mode - do final parse with error reporting
                    eq.updateAST(allocator, false);
                }
            }

            // Delete button with trash icon
            if (rg.button(.init(205, eq_y, 35, 30), "#143#")) {
                // Delete this equation
                const removed = equations.orderedRemove(i);
                removed.deinit();
                allocator.destroy(removed);
                continue; // Don't increment i, check same index again
            }

            // Optimistically parse while editing for real-time updates
            if (eq.edit_mode) {
                eq.updateAST(allocator, true);
            }

            // Show error message only when not editing
            if (eq.has_error and !eq.edit_mode) {
                var err_len: usize = 0;
                while (err_len < eq.error_msg.len and eq.error_msg[err_len] != 0) {
                    err_len += 1;
                }
                if (err_len > 0) {
                    const err_str = eq.error_msg[0..err_len :0];
                    _ = rg.label(.init(10, eq_y + 32, 190, 15), err_str);
                }
            }

            eq_y += 40;
            i += 1;
        }
    }
}
