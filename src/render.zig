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
    zoom: f32, // Zoom level
    pan_x: f32, // Pan offset in graph units
    pan_y: f32,

    pub fn graphToScreenX(self: Viewport, graph_x: f32) f32 {
        const center_x_f: f32 = @floatFromInt(self.center_x);
        const adjusted_x = (graph_x - self.pan_x) * self.zoom;
        return center_x_f + (adjusted_x * self.grid_spacing);
    }

    pub fn graphToScreenY(self: Viewport, graph_y: f32) f32 {
        const center_y_f: f32 = @floatFromInt(self.center_y);
        const adjusted_y = (graph_y - self.pan_y) * self.zoom;
        return center_y_f - (adjusted_y * self.grid_spacing);
    }

    pub fn screenToGraphX(self: Viewport, screen_x: f32) f32 {
        const center_x_f: f32 = @floatFromInt(self.center_x);
        return ((screen_x - center_x_f) / self.grid_spacing) / self.zoom + self.pan_x;
    }

    pub fn screenToGraphY(self: Viewport, screen_y: f32) f32 {
        const center_y_f: f32 = @floatFromInt(self.center_y);
        return ((center_y_f - screen_y) / self.grid_spacing) / self.zoom + self.pan_y;
    }
};

pub const GraphRenderer = struct {
    grid_spacing: i32,
    grid_color: rl.Color,
    axis_color: rl.Color,
    zoom: f32, // Zoom level (1.0 = default, 2.0 = 2x zoom)
    pan_x: f32, // Pan offset in graph units
    pan_y: f32,
    is_panning: bool, // Whether currently panning

    pub fn init(grid_spacing: i32, grid_color: rl.Color, axis_color: rl.Color) GraphRenderer {
        return .{
            .grid_spacing = grid_spacing,
            .grid_color = grid_color,
            .axis_color = axis_color,
            .zoom = 1.0,
            .pan_x = 0.0,
            .pan_y = 0.0,
            .is_panning = false,
        };
    }

    pub fn handleInput(self: *GraphRenderer, sidebar_width: i32) void {
        // Zoom with mouse wheel
        const wheel = rl.getMouseWheelMove();
        if (wheel != 0.0) {
            const zoom_factor: f32 = 1.1;
            if (wheel > 0) {
                self.zoom *= zoom_factor;
            } else {
                self.zoom /= zoom_factor;
            }
            // Clamp zoom to reasonable range
            self.zoom = std.math.clamp(self.zoom, 0.1, 10.0);
        }

        // Pan with middle mouse or Shift+Left mouse
        const is_shift = rl.isKeyDown(rl.KeyboardKey.left_shift) or rl.isKeyDown(rl.KeyboardKey.right_shift);
        const should_pan = rl.isMouseButtonDown(rl.MouseButton.middle) or
                          (is_shift and rl.isMouseButtonDown(rl.MouseButton.left));

        if (should_pan) {
            const mouse_pos = rl.getMousePosition();
            // Only pan if not in sidebar
            if (mouse_pos.x > @as(f32, @floatFromInt(sidebar_width))) {
                if (!self.is_panning) {
                    self.is_panning = true;
                } else {
                    // Pan based on mouse delta
                    const delta = rl.getMouseDelta();
                    const grid_spacing_f: f32 = @floatFromInt(self.grid_spacing);
                    self.pan_x -= delta.x / (grid_spacing_f * self.zoom);
                    self.pan_y += delta.y / (grid_spacing_f * self.zoom);
                }
            }
        } else {
            self.is_panning = false;
        }
    }

    pub fn drawGrid(self: *GraphRenderer, viewport: Viewport, width: i32, height: i32) void {
        // Calculate visible graph range
        const sidebar_f: f32 = @floatFromInt(viewport.sidebar_width);
        const width_f: f32 = @floatFromInt(width);
        const height_f: f32 = @floatFromInt(height);

        const x_min = viewport.screenToGraphX(sidebar_f);
        const x_max = viewport.screenToGraphX(width_f);
        const y_min = viewport.screenToGraphY(height_f);
        const y_max = viewport.screenToGraphY(0.0);

        // Draw vertical grid lines (at integer graph coordinates)
        var graph_x = @floor(x_min);
        while (graph_x <= x_max) : (graph_x += 1.0) {
            const screen_x = viewport.graphToScreenX(graph_x);
            const screen_x_i: i32 = @intFromFloat(screen_x);

            if (screen_x_i >= viewport.sidebar_width) {
                rl.drawLine(screen_x_i, 0, screen_x_i, height, self.grid_color);
            }
        }

        // Draw horizontal grid lines (at integer graph coordinates)
        var graph_y = @floor(y_min);
        while (graph_y <= y_max) : (graph_y += 1.0) {
            const screen_y = viewport.graphToScreenY(graph_y);
            const screen_y_i: i32 = @intFromFloat(screen_y);

            if (screen_y_i >= 0 and screen_y_i < height) {
                const start_x: i32 = @max(viewport.sidebar_width, 0);
                rl.drawLine(start_x, screen_y_i, width, screen_y_i, self.grid_color);
            }
        }
    }

    pub fn drawAxes(self: *GraphRenderer, viewport: Viewport, width: i32, height: i32) void {
        // Draw x-axis (y = 0 in graph space)
        const y_axis_screen = viewport.graphToScreenY(0.0);
        const y_axis_i: i32 = @intFromFloat(y_axis_screen);

        if (y_axis_i >= 0 and y_axis_i < height) {
            const start_x: i32 = @max(viewport.sidebar_width, 0);
            rl.drawLine(start_x, y_axis_i, width, y_axis_i, self.axis_color);
        }

        // Draw y-axis (x = 0 in graph space)
        const x_axis_screen = viewport.graphToScreenX(0.0);
        const x_axis_i: i32 = @intFromFloat(x_axis_screen);

        if (x_axis_i >= viewport.sidebar_width and x_axis_i < width) {
            rl.drawLine(x_axis_i, 0, x_axis_i, height, self.axis_color);
        }
    }

    pub fn plotEquations(
        self: *GraphRenderer,
        equations: []*Equation,
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

            // Skip assignments (they don't plot)
            if (eq.equation_type == .assignment) continue;

            // Render equation directly to screen
            // TODO: Optimize marching squares performance for real-time rendering
            if (eq.equation_type == .constraint_implicit) {
                try MarchingSquares.plotImplicitCurve(eq, eval_context, viewport, eq.color, allocator);
            } else {
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
        // Account for zoom: when zoomed in, use smaller cells in graph space
        const pixels_per_cell: f32 = 5.0;
        const cell_size = pixels_per_cell / (viewport.grid_spacing * viewport.zoom);

        // Calculate graph bounds
        const sidebar_f: f32 = @floatFromInt(viewport.sidebar_width);
        const width_f: f32 = @floatFromInt(viewport.screen_width);
        const height_f: f32 = @floatFromInt(viewport.screen_height);

        const x_min = viewport.screenToGraphX(sidebar_f);
        const x_max = viewport.screenToGraphX(width_f);
        const y_min = viewport.screenToGraphY(height_f);
        const y_max = viewport.screenToGraphY(0.0);

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

