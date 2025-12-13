const std = @import("std");
const rl = @import("raylib");
const rg = @import("raygui");
const equation_mod = @import("equation.zig");

const Equation = equation_mod.Equation;

pub const UI = struct {
    sidebar_width: i32,
    colors: []const rl.Color,

    pub fn init(sidebar_width: i32, colors: []const rl.Color) UI {
        return .{
            .sidebar_width = sidebar_width,
            .colors = colors,
        };
    }

    pub fn renderSidebar(
        self: *UI,
        equations: *std.ArrayList(*Equation),
        allocator: std.mem.Allocator,
        screen_height: i32,
    ) !void {
        // Draw sidebar background
        const sidebar_x: i32 = 0;
        rl.drawRectangle(sidebar_x, 0, self.sidebar_width, screen_height, rl.Color.init(240, 240, 240, 255));

        // Sidebar header
        _ = rg.label(.init(10, 20, 230, 20), "Equations:");

        // Add equation button
        if (rg.button(.init(10, 50, 230, 30), "+ Add Equation")) {
            const new_eq = try allocator.create(Equation);
            const color_idx = equations.items.len % self.colors.len;
            new_eq.* = Equation.init(self.colors[color_idx]);
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
};
