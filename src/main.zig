const std = @import("std");
const rl = @import("raylib");
const rg = @import("raygui");
const equation = @import("equation.zig");
const ui_mod = @import("ui.zig");
const render_mod = @import("render.zig");

const Equation = equation.Equation;
const UI = ui_mod.UI;
const GraphRenderer = render_mod.GraphRenderer;
const Viewport = render_mod.Viewport;

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

    // Initialize modules
    var ui = UI.init(sidebar_width, &colors);
    var renderer = GraphRenderer.init(grid_spacing, grid_color, axis_color);

    // Start with one equation
    const first_eq = try allocator.create(Equation);
    first_eq.* = Equation.init(colors[0]);
    try equations.append(allocator, first_eq);

    while (!rl.windowShouldClose()) {
        const width = rl.getScreenWidth();
        const height = rl.getScreenHeight();

        // Handle zoom and pan input
        renderer.handleInput(sidebar_width);

        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(.white);

        // Calculate center position
        const center_x = @divTrunc(width, 2);
        const center_y = @divTrunc(height, 2);

        // Create viewport for coordinate transformations
        const viewport = Viewport{
            .screen_width = width,
            .screen_height = height,
            .center_x = center_x,
            .center_y = center_y,
            .grid_spacing = @floatFromInt(grid_spacing),
            .sidebar_width = sidebar_width,
            .zoom = renderer.zoom,
            .pan_x = renderer.pan_x,
            .pan_y = renderer.pan_y,
        };

        // Draw grid and axes
        renderer.drawGrid(viewport, width, height);
        renderer.drawAxes(viewport, width, height);

        // Create evaluation context
        var eval_context = std.StringHashMap(f32).init(allocator);
        defer eval_context.deinit();
        try eval_context.put("x", 0.0);
        try eval_context.put("y", 0.0);

        // Evaluate dependencies
        var dep_graph = equation.DependencyGraph.init(allocator);
        defer dep_graph.deinit();
        try dep_graph.build(equations.items);

        const eval_order = dep_graph.topologicalSort(equations.items.len) catch |err| blk: {
            if (err == error.CircularDependency) {
                var order = try allocator.alloc(usize, equations.items.len);
                for (0..equations.items.len) |i| order[i] = i;
                break :blk order;
            } else return err;
        };
        defer allocator.free(eval_order);

        for (eval_order) |idx| {
            const eq = equations.items[idx];
            if (eq.equation_type == .assignment) {
                if (eq.evaluate(eval_context)) |value| {
                    if (eq.equation_ast) |eq_ast| {
                        if (eq_ast == .assignment) {
                            try eval_context.put(eq_ast.assignment.var_name, value);
                        }
                    }
                } else |_| {}
            }
        }

        // Plot equations with caching
        try renderer.plotEquations(equations.items, &eval_context, viewport, allocator);

        // Render UI sidebar
        try ui.renderSidebar(&equations, allocator, height);
    }
}
