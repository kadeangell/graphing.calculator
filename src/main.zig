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

        for (equations.items) |eq| {
            // Skip equations with errors or no AST
            if (eq.has_error or eq.ast == null) continue;

            // Plot the function
            var prev_screen_x: ?f32 = null;
            var prev_screen_y: ?f32 = null;

            var screen_x: f32 = @floatFromInt(sidebar_width);
            while (screen_x < @as(f32, @floatFromInt(width))) : (screen_x += 1.0) {
                // Convert screen x to graph x
                const graph_x = (screen_x - center_x_f) / grid_spacing_f;

                // Evaluate function using AST
                const graph_y = eq.evaluate(graph_x, allocator) catch continue;

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
        for (equations.items, 0..) |eq, i| {
            var buf: [32:0]u8 = undefined;
            const label_text = try std.fmt.bufPrintZ(&buf, "f{}(x):", .{i + 1});
            _ = rg.label(.init(10, eq_y, 60, 20), label_text);

            if (rg.textBox(.init(70, eq_y, 170, 30), &eq.function, 256, eq.edit_mode)) {
                eq.edit_mode = !eq.edit_mode;
                if (!eq.edit_mode) {
                    // Exiting edit mode - do final parse with error reporting
                    eq.updateAST(allocator, false);
                }
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
                    _ = rg.label(.init(70, eq_y + 32, 170, 15), err_str);
                }
            }

            eq_y += 40;
        }
    }
}
