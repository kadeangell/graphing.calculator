const std = @import("std");
const rl = @import("raylib");
const equation_mod = @import("equation.zig");

const Equation = equation_mod.Equation;

pub const Viewport = struct {
    screen_width: i32,
    screen_height: i32,
    center_x: i32,
    center_y: i32,
    grid_spacing: f32,
    sidebar_width: i32,

    pub fn graphToScreenX(self: Viewport, graph_x: f32) f32 {
        const center_x_f: f32 = @floatFromInt(self.center_x);
        return center_x_f + (graph_x * self.grid_spacing);
    }

    pub fn graphToScreenY(self: Viewport, graph_y: f32) f32 {
        const center_y_f: f32 = @floatFromInt(self.center_y);
        return center_y_f - (graph_y * self.grid_spacing);
    }

    pub fn screenToGraphX(self: Viewport, screen_x: f32) f32 {
        const center_x_f: f32 = @floatFromInt(self.center_x);
        return (screen_x - center_x_f) / self.grid_spacing;
    }

    pub fn screenToGraphY(self: Viewport, screen_y: f32) f32 {
        const center_y_f: f32 = @floatFromInt(self.center_y);
        return (center_y_f - screen_y) / self.grid_spacing;
    }
};

pub const GraphRenderer = struct {
    grid_spacing: i32,
    grid_color: rl.Color,
    axis_color: rl.Color,

    pub fn init(grid_spacing: i32, grid_color: rl.Color, axis_color: rl.Color) GraphRenderer {
        return .{
            .grid_spacing = grid_spacing,
            .grid_color = grid_color,
            .axis_color = axis_color,
        };
    }

    pub fn drawGrid(self: *GraphRenderer, center_x: i32, center_y: i32, width: i32, height: i32) void {
        // Draw vertical grid lines from center outward
        var x: i32 = center_x;
        while (x <= width) : (x += self.grid_spacing) {
            rl.drawLine(x, 0, x, height, self.grid_color);
        }
        x = center_x - self.grid_spacing;
        while (x >= 0) : (x -= self.grid_spacing) {
            rl.drawLine(x, 0, x, height, self.grid_color);
        }

        // Draw horizontal grid lines from center outward
        var y: i32 = center_y;
        while (y <= height) : (y += self.grid_spacing) {
            rl.drawLine(0, y, width, y, self.grid_color);
        }
        y = center_y - self.grid_spacing;
        while (y >= 0) : (y -= self.grid_spacing) {
            rl.drawLine(0, y, width, y, self.grid_color);
        }
    }

    pub fn drawAxes(self: *GraphRenderer, center_x: i32, center_y: i32, width: i32, height: i32) void {
        rl.drawLine(center_x, 0, center_x, height, self.axis_color);
        rl.drawLine(0, center_y, width, center_y, self.axis_color);
    }

    pub fn plotEquations(
        self: *GraphRenderer,
        equations: []const *Equation,
        eval_context: *std.StringHashMap(f32),
        viewport: Viewport,
        allocator: std.mem.Allocator,
    ) !void {
        _ = self;

        for (equations) |eq| {
            // Skip equations with errors
            if (eq.has_error) continue;

            // Skip if no AST
            if (eq.equation_ast == null) continue;

            // Debug: print equation type
            std.debug.print("Plotting equation type: {s}\n", .{@tagName(eq.equation_type)});

            // Skip assignments (they don't plot)
            if (eq.equation_type == .assignment) continue;

            // Handle different equation types
            if (eq.equation_type == .constraint_implicit) {
                // Use marching squares for all constraints
                try MarchingSquares.plotImplicitCurve(eq, eval_context, viewport, eq.color, allocator);
            } else {
                // Plot regular functions
                try plotExplicitFunction(eq, eval_context, viewport, eq.color);
            }
        }
    }

    fn plotExplicitFunction(
        eq: *const Equation,
        eval_context: *std.StringHashMap(f32),
        viewport: Viewport,
        color: rl.Color,
    ) !void {
        var prev_screen_x: ?f32 = null;
        var prev_screen_y: ?f32 = null;

        const sidebar_f: f32 = @floatFromInt(viewport.sidebar_width);
        const width_f: f32 = @floatFromInt(viewport.screen_width);

        var screen_x: f32 = sidebar_f;
        while (screen_x < width_f) : (screen_x += 1.0) {
            const graph_x = viewport.screenToGraphX(screen_x);

            try eval_context.put("x", graph_x);

            const graph_y = eq.evaluate(eval_context.*) catch continue;

            const screen_y = viewport.graphToScreenY(graph_y);

            if (prev_screen_x) |px| {
                if (prev_screen_y) |py| {
                    if (@abs(screen_y - py) < 1000) {
                        rl.drawLineEx(
                            .{ .x = px, .y = py },
                            .{ .x = screen_x, .y = screen_y },
                            2.0,
                            color
                        );
                    }
                }
            }

            prev_screen_x = screen_x;
            prev_screen_y = screen_y;
        }
    }
};

// Marching Squares implementation for implicit curve plotting
const MarchingSquares = struct {
    pub fn plotImplicitCurve(
        eq: *const Equation,
        eval_context: *std.StringHashMap(f32),
        viewport: Viewport,
        color: rl.Color,
        allocator: std.mem.Allocator,
    ) !void {
        _ = allocator;

        // Adaptive cell size based on zoom level
        // Target ~5 pixel cells for good resolution
        // viewport.grid_spacing is in pixels (e.g., 50 pixels = 1 graph unit)
        // So cell_size in graph units = pixels_per_cell / viewport.grid_spacing
        const pixels_per_cell: f32 = 5.0;
        const cell_size = pixels_per_cell / viewport.grid_spacing;

        // Calculate graph bounds
        const sidebar_f: f32 = @floatFromInt(viewport.sidebar_width);
        const width_f: f32 = @floatFromInt(viewport.screen_width);
        const height_f: f32 = @floatFromInt(viewport.screen_height);

        const x_min = viewport.screenToGraphX(sidebar_f);
        const x_max = viewport.screenToGraphX(width_f);
        const y_min = viewport.screenToGraphY(height_f);
        const y_max = viewport.screenToGraphY(0.0);

        std.debug.print("Marching squares: cell_size={d}, x_min={d}, x_max={d}, y_min={d}, y_max={d}\n",
            .{cell_size, x_min, x_max, y_min, y_max});

        var cells_processed: usize = 0;
        var cells_drawn: usize = 0;

        // Iterate over grid cells
        var graph_y = y_min;
        while (graph_y < y_max) : (graph_y += cell_size) {
            var graph_x = x_min;
            while (graph_x < x_max) : (graph_x += cell_size) {
                cells_processed += 1;
                const drew = try processCell(graph_x, graph_y, cell_size, eq, eval_context, viewport, color);
                if (drew) cells_drawn += 1;
            }
        }

        std.debug.print("Marching squares: processed {d} cells, drew {d} cells\n", .{cells_processed, cells_drawn});
    }

    fn processCell(
        x0: f32,
        y0: f32,
        cell_size: f32,
        eq: *const Equation,
        eval_context: *std.StringHashMap(f32),
        viewport: Viewport,
        color: rl.Color,
    ) !bool {
        // Evaluate at 4 corners
        // Corner order: bottom-left, bottom-right, top-right, top-left
        const corners = [4][2]f32{
            .{ x0, y0 },                        // bottom-left
            .{ x0 + cell_size, y0 },            // bottom-right
            .{ x0 + cell_size, y0 + cell_size}, // top-right
            .{ x0, y0 + cell_size },            // top-left
        };

        var values: [4]f32 = undefined;
        for (corners, 0..) |corner, i| {
            values[i] = evaluateConstraint(corner[0], corner[1], eq, eval_context) catch {
                // If evaluation fails, skip this cell entirely
                return false;
            };
        }

        // Check for NaN or infinite values
        for (values) |val| {
            if (std.math.isNan(val) or std.math.isInf(val)) return false;
        }

        // Determine case (which corners are positive)
        var case: u4 = 0;
        for (values, 0..) |val, i| {
            if (val > 0) {
                case |= @as(u4, 1) << @intCast(i);
            }
        }

        // Skip if no lines to draw
        if (case == 0 or case == 15) return false;

        // Draw lines based on case
        try drawCaseLines(case, &corners, &values, viewport, color);
        return true;
    }

    fn evaluateConstraint(
        x: f32,
        y: f32,
        eq: *const Equation,
        eval_context: *std.StringHashMap(f32),
    ) !f32 {
        const eq_ast = eq.equation_ast orelse return error.NoAST;

        if (eq_ast != .constraint_implicit) return error.NotAConstraint;

        const c = eq_ast.constraint_implicit;

        // Set x and y in context
        try eval_context.put("x", x);
        try eval_context.put("y", y);

        // Return left - right (zero crossing is the curve)
        const left_val = try c.left.evaluate(eval_context.*);
        const right_val = try c.right.evaluate(eval_context.*);

        return left_val - right_val;
    }

    fn interpolate(_: f32, _: f32, val0: f32, val1: f32) f32 {
        // Linear interpolation to find where function crosses zero
        if (@abs(val1 - val0) < 0.0001) return 0.5;
        const t = -val0 / (val1 - val0);
        return std.math.clamp(t, 0.0, 1.0);
    }

    fn drawCaseLines(
        case: u4,
        corners: *const [4][2]f32,
        values: *const [4]f32,
        viewport: Viewport,
        color: rl.Color,
    ) !void {
        // Marching squares lookup table
        // Each case defines which edges have line segments
        // Edges: 0=bottom, 1=right, 2=top, 3=left

        // For simplicity, handle key cases
        // Cases 0 and 15 have no lines
        if (case == 0 or case == 15) return;

        // Define edge midpoints with interpolation
        var edges: [4][2]f32 = undefined;

        // Bottom edge (between corners 0 and 1)
        const t_bottom = interpolate(0, 1, values[0], values[1]);
        edges[0] = .{
            corners[0][0] + t_bottom * (corners[1][0] - corners[0][0]),
            corners[0][1] + t_bottom * (corners[1][1] - corners[0][1]),
        };

        // Right edge (between corners 1 and 2)
        const t_right = interpolate(0, 1, values[1], values[2]);
        edges[1] = .{
            corners[1][0] + t_right * (corners[2][0] - corners[1][0]),
            corners[1][1] + t_right * (corners[2][1] - corners[1][1]),
        };

        // Top edge (between corners 2 and 3)
        const t_top = interpolate(0, 1, values[2], values[3]);
        edges[2] = .{
            corners[2][0] + t_top * (corners[3][0] - corners[2][0]),
            corners[2][1] + t_top * (corners[3][1] - corners[2][1]),
        };

        // Left edge (between corners 3 and 0)
        const t_left = interpolate(0, 1, values[3], values[0]);
        edges[3] = .{
            corners[3][0] + t_left * (corners[0][0] - corners[3][0]),
            corners[3][1] + t_left * (corners[0][1] - corners[3][1]),
        };

        // Draw lines based on case using lookup table
        const lines = marchingSquaresLookup(case);

        for (lines) |line| {
            if (line[0] == 255) break; // End of lines for this case

            const edge1 = edges[line[0]];
            const edge2 = edges[line[1]];

            const screen1_x = viewport.graphToScreenX(edge1[0]);
            const screen1_y = viewport.graphToScreenY(edge1[1]);
            const screen2_x = viewport.graphToScreenX(edge2[0]);
            const screen2_y = viewport.graphToScreenY(edge2[1]);

            rl.drawLineEx(
                .{ .x = screen1_x, .y = screen1_y },
                .{ .x = screen2_x, .y = screen2_y },
                2.0,
                color
            );
        }
    }

    fn marchingSquaresLookup(case: u4) [2][2]u8 {
        // Returns up to 2 line segments for each case
        // Each line is defined by two edge indices (0-3)
        // 255 indicates no line

        return switch (case) {
            0, 15 => .{.{255, 255}, .{255, 255}}, // No lines
            1, 14 => .{.{3, 0}, .{255, 255}},     // One line
            2, 13 => .{.{0, 1}, .{255, 255}},
            3, 12 => .{.{3, 1}, .{255, 255}},
            4, 11 => .{.{1, 2}, .{255, 255}},
            5 => .{.{3, 0}, .{1, 2}},              // Two lines (saddle)
            6, 9 => .{.{0, 2}, .{255, 255}},
            7, 8 => .{.{3, 2}, .{255, 255}},
            10 => .{.{0, 1}, .{3, 2}},             // Two lines (saddle)
        };
    }
};
