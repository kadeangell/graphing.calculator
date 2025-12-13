const std = @import("std");
const rl = @import("raylib");
const rg = @import("raygui");
const equation_mod = @import("equation.zig");

const Equation = equation_mod.Equation;

pub const UI = struct {
    sidebar_width: i32,
    colors: []const rl.Color,
    color_picker_open: ?usize, // Which equation's color picker is open (index)
    hex_input: [8:0]u8, // Buffer for hex color input
    hex_edit_mode: bool, // Whether hex input is being edited

    pub fn init(sidebar_width: i32, colors: []const rl.Color) UI {
        return .{
            .sidebar_width = sidebar_width,
            .colors = colors,
            .color_picker_open = null,
            .hex_input = std.mem.zeroes([8:0]u8),
            .hex_edit_mode = false,
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

            // Color picker button (small circle on the left)
            const color_circle_x: f32 = 15;
            const color_circle_y: f32 = eq_y + 15;
            rl.drawCircle(@intFromFloat(color_circle_x), @intFromFloat(color_circle_y), 7, eq.color);
            rl.drawCircleLines(@intFromFloat(color_circle_x), @intFromFloat(color_circle_y), 7, rl.Color.dark_gray);

            // Check if color circle was clicked
            const mouse_pos = rl.getMousePosition();
            const dist_x = mouse_pos.x - color_circle_x;
            const dist_y = mouse_pos.y - color_circle_y;
            const dist = @sqrt(dist_x * dist_x + dist_y * dist_y);

            if (dist < 7 and rl.isMouseButtonPressed(rl.MouseButton.left)) {
                // Toggle color picker for this equation
                if (self.color_picker_open) |open_idx| {
                    if (open_idx == i) {
                        self.color_picker_open = null; // Close if already open
                    } else {
                        self.color_picker_open = i; // Open for this equation
                    }
                } else {
                    self.color_picker_open = i;
                    // Initialize hex input with current color
                    const r = eq.color.r;
                    const g = eq.color.g;
                    const b = eq.color.b;
                    const hex_str = try std.fmt.bufPrint(&self.hex_input, "{X:0>2}{X:0>2}{X:0>2}", .{r, g, b});
                    self.hex_input[hex_str.len] = 0;
                }
            }

            // Handle clipboard operations when textbox is focused
            if (eq.edit_mode) {
                // Check for Cmd+C or Ctrl+C (copy)
                const is_modifier = rl.isKeyDown(rl.KeyboardKey.left_super) or
                                   rl.isKeyDown(rl.KeyboardKey.right_super) or
                                   rl.isKeyDown(rl.KeyboardKey.left_control) or
                                   rl.isKeyDown(rl.KeyboardKey.right_control);

                if (is_modifier and rl.isKeyPressed(rl.KeyboardKey.c)) {
                    // Copy current equation text to clipboard
                    var text_len: usize = 0;
                    while (text_len < eq.function.len and eq.function[text_len] != 0) {
                        text_len += 1;
                    }
                    if (text_len > 0) {
                        const text = eq.function[0..text_len :0];
                        rl.setClipboardText(text);
                    }
                }

                // Check for Cmd+V or Ctrl+V (paste)
                if (is_modifier and rl.isKeyPressed(rl.KeyboardKey.v)) {
                    // Get clipboard text
                    const clipboard_text = rl.getClipboardText();
                    // Find length and strip newlines
                    var dest_idx: usize = 0;
                    var src_idx: usize = 0;
                    while (src_idx < clipboard_text.len and clipboard_text[src_idx] != 0 and dest_idx < 255) {
                        const ch = clipboard_text[src_idx];
                        // Skip newlines and carriage returns
                        if (ch != '\n' and ch != '\r') {
                            eq.function[dest_idx] = ch;
                            dest_idx += 1;
                        }
                        src_idx += 1;
                    }
                    if (dest_idx > 0) {
                        eq.function[dest_idx] = 0;
                        // Parse immediately on paste
                        eq.updateAST(allocator, true);
                    }
                }
            }

            // Equation text box (shifted right for color circle)
            if (rg.textBox(.init(30, eq_y, 170, 30), &eq.function, 256, eq.edit_mode)) {
                eq.edit_mode = !eq.edit_mode;
                if (!eq.edit_mode) {
                    // Exiting edit mode - do final parse with error reporting
                    eq.updateAST(allocator, false);
                }
            }

            // Delete button with trash icon (adjusted position)
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
                    _ = rg.label(.init(30, eq_y + 32, 170, 15), err_str);
                }
            }

            // Show color picker if open for this equation
            if (self.color_picker_open) |open_idx| {
                if (open_idx == i) {
                    try self.renderColorPicker(eq, eq_y);
                }
            }

            eq_y += 40;
            i += 1;
        }
    }

    fn renderColorPicker(self: *UI, eq: *Equation, y_offset: f32) !void {
        // Color picker panel (smaller and more compact)
        const picker_x: f32 = 30;
        const picker_y: f32 = y_offset + 35;
        const picker_width: f32 = 200;
        const picker_height: f32 = 80;

        // Draw background
        rl.drawRectangle(
            @intFromFloat(picker_x),
            @intFromFloat(picker_y),
            @intFromFloat(picker_width),
            @intFromFloat(picker_height),
            rl.Color.init(255, 255, 255, 255)
        );
        rl.drawRectangleLines(
            @intFromFloat(picker_x),
            @intFromFloat(picker_y),
            @intFromFloat(picker_width),
            @intFromFloat(picker_height),
            rl.Color.dark_gray
        );

        // Default color palette as circles (first row)
        var color_x: f32 = picker_x + 15;
        const color_y: f32 = picker_y + 15;

        for (self.colors) |color| {
            rl.drawCircle(@intFromFloat(color_x), @intFromFloat(color_y), 8, color);
            rl.drawCircleLines(@intFromFloat(color_x), @intFromFloat(color_y), 8, rl.Color.dark_gray);

            // Check if clicked
            const mouse_pos = rl.getMousePosition();
            const dist_x = mouse_pos.x - color_x;
            const dist_y = mouse_pos.y - color_y;
            const dist = @sqrt(dist_x * dist_x + dist_y * dist_y);

            if (dist < 8 and rl.isMouseButtonPressed(rl.MouseButton.left)) {
                eq.color = color;
                self.color_picker_open = null; // Close picker after selection
            }

            color_x += 25;
        }

        // Hex color input with label
        _ = rg.label(.init(picker_x + 5, picker_y + 40, 30, 20), "Hex:");

        // Hex input field - toggleable edit mode
        if (rg.textBox(.init(picker_x + 40, picker_y + 40, 90, 25), &self.hex_input, 8, self.hex_edit_mode)) {
            self.hex_edit_mode = !self.hex_edit_mode;

            // When exiting edit mode, try to parse the hex color
            if (!self.hex_edit_mode) {
                var hex_len: usize = 0;
                while (hex_len < self.hex_input.len and self.hex_input[hex_len] != 0) {
                    hex_len += 1;
                }

                if (hex_len >= 6) {
                    const hex_str = self.hex_input[0..hex_len];
                    // Remove # if present
                    const hex_start: usize = if (hex_str[0] == '#') 1 else 0;

                    if (hex_len - hex_start >= 6) {
                        const rgb_hex = hex_str[hex_start..hex_start + 6];
                        if (std.fmt.parseInt(u32, rgb_hex, 16)) |rgb| {
                            eq.color = rl.Color.init(
                                @intCast((rgb >> 16) & 0xFF),
                                @intCast((rgb >> 8) & 0xFF),
                                @intCast(rgb & 0xFF),
                                255
                            );
                            self.color_picker_open = null; // Close after applying color
                        } else |_| {}
                    }
                }
            }
        }
    }
};
