const std = @import("std");
const rl = @import("raylib");
const tokenizer_mod = @import("tokenizer.zig");
const parser_mod = @import("parser.zig");

// Re-export commonly used types
pub const Token = tokenizer_mod.Token;
pub const Tokenizer = tokenizer_mod.Tokenizer;
pub const ASTNode = parser_mod.ASTNode;
pub const EquationType = parser_mod.EquationType;
pub const EquationAST = parser_mod.EquationAST;
pub const ParseError = parser_mod.ParseError;
pub const parseFullEquation = parser_mod.parseFullEquation;
pub const collectVariables = parser_mod.collectVariables;

// Simple cache tracking (no textures - just skip re-rendering)
pub const EquationCache = struct {
    last_zoom: f32,
    last_pan_x: f32,
    last_pan_y: f32,
    last_text_hash: u64,
    rendered_once: bool,

    pub fn init() EquationCache {
        return .{
            .last_zoom = 0,
            .last_pan_x = 0,
            .last_pan_y = 0,
            .last_text_hash = 0,
            .rendered_once = false,
        };
    }

    pub fn deinit(_: *EquationCache) void {}

    pub fn needsUpdate(self: *EquationCache, viewport: anytype, text_hash: u64) bool {
        if (!self.rendered_once) return true;
        if (text_hash != self.last_text_hash) return true;

        const zoom_changed = @abs(self.last_zoom - viewport.zoom) > 0.001;
        const pan_x_changed = @abs(self.last_pan_x - viewport.pan_x) > 0.01;
        const pan_y_changed = @abs(self.last_pan_y - viewport.pan_y) > 0.01;

        return zoom_changed or pan_x_changed or pan_y_changed;
    }

    pub fn markUpdated(self: *EquationCache, viewport: anytype, text_hash: u64) void {
        self.last_zoom = viewport.zoom;
        self.last_pan_x = viewport.pan_x;
        self.last_pan_y = viewport.pan_y;
        self.last_text_hash = text_hash;
        self.rendered_once = true;
    }
};

pub const Equation = struct {
    function: [256:0]u8,
    edit_mode: bool,
    color: rl.Color,
    equation_ast: ?EquationAST,
    equation_type: EquationType,
    allocator: ?std.mem.Allocator,
    has_error: bool,
    error_msg: [128:0]u8,
    cache: EquationCache, // NEW: Render cache

    pub fn init(color: rl.Color) Equation {
        return .{
            .function = std.mem.zeroes([256:0]u8),
            .edit_mode = false,
            .color = color,
            .equation_ast = null,
            .equation_type = .function_implicit,
            .allocator = null,
            .has_error = false,
            .error_msg = std.mem.zeroes([128:0]u8),
            .cache = EquationCache.init(),
        };
    }

    pub fn deinit(self: *Equation) void {
        if (self.equation_ast) |*eq_ast| {
            if (self.allocator) |alloc| {
                eq_ast.deinit(alloc);
            }
        }
        self.cache.deinit();
    }

    pub fn setError(self: *Equation, msg: []const u8) void {
        self.has_error = true;
        const len = @min(msg.len, 127);
        @memcpy(self.error_msg[0..len], msg[0..len]);
        self.error_msg[len] = 0;
    }

    pub fn clearError(self: *Equation) void {
        self.has_error = false;
        self.error_msg = std.mem.zeroes([128:0]u8);
    }

    pub fn updateAST(self: *Equation, allocator: std.mem.Allocator, optimistic: bool) void {
        // Get the function string
        var func_len: usize = 0;
        while (func_len < self.function.len and self.function[func_len] != 0) {
            func_len += 1;
        }

        if (func_len == 0) {
            // Clear old AST if it exists
            if (self.equation_ast) |*eq_ast| {
                if (self.allocator) |alloc| {
                    eq_ast.deinit(alloc);
                }
            }
            self.equation_ast = null;
            self.clearError();
            return;
        }

        const func_str = self.function[0..func_len];

        // Try to parse as full equation
        if (parseFullEquation(func_str, allocator)) |eq_ast| {
            // Parsing succeeded - clear old AST and use new one
            if (self.equation_ast) |*old_ast| {
                if (self.allocator) |alloc| {
                    old_ast.deinit(alloc);
                }
            }
            self.equation_ast = eq_ast;
            self.equation_type = eq_ast;  // Extract type from union
            self.allocator = allocator;
            self.clearError();
        } else |err| {
            if (optimistic) {
                // Optimistic mode: keep old AST, don't show error
                // User is probably still typing
                return;
            } else {
                // Not optimistic: clear AST and set error
                if (self.equation_ast) |*eq_ast| {
                    if (self.allocator) |alloc| {
                        eq_ast.deinit(alloc);
                    }
                }
                self.equation_ast = null;
                const err_name = @errorName(err);
                self.setError(err_name);
            }
        }
    }

    pub fn evaluate(self: *const Equation, variables: std.StringHashMap(f32)) !f32 {
        if (self.has_error) return error.EquationHasError;

        const eq_ast = self.equation_ast orelse return error.NoAST;

        return switch (eq_ast) {
            .assignment => |a| try a.value.evaluate(variables),
            .function_explicit => |f| try f.body.evaluate(variables),
            .function_implicit => |f| try f.body.evaluate(variables),
            .constraint_implicit => error.CannotEvaluateConstraint,  // Need special handling
        };
    }

    pub fn evaluateConstraint(self: *const Equation, variables: std.StringHashMap(f32), solve_for: []const u8) !f32 {
        if (self.has_error) return error.EquationHasError;

        const eq_ast = self.equation_ast orelse return error.NoAST;

        if (eq_ast != .constraint_implicit) return error.NotAConstraint;

        const c = eq_ast.constraint_implicit;

        // Simple case: solve for variable on one side
        // If left side is just the variable we're solving for, evaluate right side
        if (c.left == .variable and std.mem.eql(u8, c.left.variable, solve_for)) {
            return try c.right.evaluate(variables);
        }

        // If right side is just the variable, evaluate left side
        if (c.right == .variable and std.mem.eql(u8, c.right.variable, solve_for)) {
            return try c.left.evaluate(variables);
        }

        // More complex cases would need numerical solving
        return error.CannotSolveConstraint;
    }

    pub fn checkConstraintSatisfied(self: *const Equation, variables: std.StringHashMap(f32), tolerance: f32) !bool {
        if (self.has_error) return error.EquationHasError;

        const eq_ast = self.equation_ast orelse return error.NoAST;

        if (eq_ast != .constraint_implicit) return error.NotAConstraint;

        const c = eq_ast.constraint_implicit;

        // Evaluate both sides
        const left_val = c.left.evaluate(variables) catch return false;
        const right_val = c.right.evaluate(variables) catch return false;

        // Check if they're equal within tolerance
        return @abs(left_val - right_val) < tolerance;
    }

    pub fn getLatex(self: *const Equation, allocator: std.mem.Allocator) ![]u8 {
        const eq_ast = self.equation_ast orelse return error.NoAST;

        return switch (eq_ast) {
            .assignment => |a| blk: {
                const value_latex = try a.value.toLatex(allocator);
                defer allocator.free(value_latex);
                break :blk try std.fmt.allocPrint(allocator, "{s} = {s}", .{a.var_name, value_latex});
            },
            .function_explicit => |f| blk: {
                const body_latex = try f.body.toLatex(allocator);
                defer allocator.free(body_latex);
                // Format params
                var params_str = std.ArrayList(u8){};
                defer params_str.deinit(allocator);
                for (f.params, 0..) |param, i| {
                    try params_str.appendSlice(allocator, param);
                    if (i < f.params.len - 1) try params_str.appendSlice(allocator, ", ");
                }
                const params_owned = try params_str.toOwnedSlice(allocator);
                defer allocator.free(params_owned);
                break :blk try std.fmt.allocPrint(allocator, "{s}({s}) = {s}", .{f.func_name, params_owned, body_latex});
            },
            .function_implicit => |f| try f.body.toLatex(allocator),
            .constraint_implicit => |c| blk: {
                const left_latex = try c.left.toLatex(allocator);
                defer allocator.free(left_latex);
                const right_latex = try c.right.toLatex(allocator);
                defer allocator.free(right_latex);
                break :blk try std.fmt.allocPrint(allocator, "{s} = {s}", .{left_latex, right_latex});
            },
        };
    }
};

pub const DependencyGraph = struct {
    allocator: std.mem.Allocator,
    // Map from equation index to list of equation indices it depends on
    dependencies: std.AutoHashMap(usize, std.ArrayList(usize)),

    pub fn init(allocator: std.mem.Allocator) DependencyGraph {
        return .{
            .allocator = allocator,
            .dependencies = std.AutoHashMap(usize, std.ArrayList(usize)).init(allocator),
        };
    }

    pub fn deinit(self: *DependencyGraph) void {
        var it = self.dependencies.valueIterator();
        while (it.next()) |list| {
            list.deinit(self.allocator);
        }
        self.dependencies.deinit();
    }

    pub fn build(self: *DependencyGraph, equations: []const *const Equation) !void {
        // Clear existing dependencies
        var it = self.dependencies.valueIterator();
        while (it.next()) |list| {
            list.deinit(self.allocator);
        }
        self.dependencies.clearRetainingCapacity();

        // Build map of variable name → equation index that defines it
        var var_definitions = std.StringHashMap(usize).init(self.allocator);
        defer var_definitions.deinit();

        // First pass: collect all variable definitions
        for (equations, 0..) |eq, i| {
            if (eq.equation_ast) |eq_ast| {
                const defined_vars = try eq_ast.getDefinedVars(self.allocator);
                defer self.allocator.free(defined_vars);

                for (defined_vars) |var_name| {
                    // Last definition wins
                    try var_definitions.put(var_name, i);
                }
            }
        }

        // Second pass: build dependency edges
        for (equations, 0..) |eq, i| {
            if (eq.equation_ast) |eq_ast| {
                const referenced_vars = try eq_ast.getReferencedVars(self.allocator);
                defer self.allocator.free(referenced_vars);

                var deps = std.ArrayList(usize){};

                for (referenced_vars) |var_name| {
                    // Skip built-in variables (x, y)
                    if (std.mem.eql(u8, var_name, "x") or std.mem.eql(u8, var_name, "y")) {
                        continue;
                    }

                    // Find which equation defines this variable
                    if (var_definitions.get(var_name)) |def_idx| {
                        if (def_idx != i) { // Don't depend on self
                            // Check if already in deps
                            var found = false;
                            for (deps.items) |existing| {
                                if (existing == def_idx) {
                                    found = true;
                                    break;
                                }
                            }
                            if (!found) {
                                try deps.append(self.allocator, def_idx);
                            }
                        }
                    }
                }

                try self.dependencies.put(i, deps);
            }
        }
    }

    pub fn topologicalSort(self: *DependencyGraph, num_equations: usize) ![]usize {
        // Kahn's algorithm for topological sort
        var result = std.ArrayList(usize){};
        defer result.deinit(self.allocator);

        // Calculate in-degree for each node
        // in_degree[i] = number of equations that i depends on
        var in_degree = try self.allocator.alloc(usize, num_equations);
        defer self.allocator.free(in_degree);
        @memset(in_degree, 0);

        // If dependencies[i] = [X, Y], then i depends on X and Y
        // So i has in-degree = len(dependencies[i])
        for (0..num_equations) |i| {
            if (self.dependencies.get(i)) |deps| {
                in_degree[i] = deps.items.len;
            }
        }

        // Find all nodes with in-degree 0 (no dependencies)
        var queue = std.ArrayList(usize){};
        defer queue.deinit(self.allocator);

        for (0..num_equations) |i| {
            if (in_degree[i] == 0) {
                try queue.append(self.allocator, i);
            }
        }

        // Process queue
        while (queue.items.len > 0) {
            const node = queue.orderedRemove(0);
            try result.append(self.allocator, node);

            // Find all equations that depend on this node
            for (0..num_equations) |i| {
                if (self.dependencies.get(i)) |deps| {
                    for (deps.items) |dep| {
                        if (dep == node) {
                            // Equation i depends on node, so decrement i's in-degree
                            in_degree[i] -= 1;
                            if (in_degree[i] == 0) {
                                try queue.append(self.allocator, i);
                            }
                            break;
                        }
                    }
                }
            }
        }

        // Check if all nodes were processed
        if (result.items.len != num_equations) {
            return error.CircularDependency;
        }

        return try result.toOwnedSlice(self.allocator);
    }
};

// ============================================================================
// TESTS
// ============================================================================

test "dependency graph: simple chain" {
    const allocator = std.testing.allocator;

    var eq1 = Equation.init(rl.Color.red);
    var eq2 = Equation.init(rl.Color.blue);

    @memcpy(eq1.function[0..7], "x_1 = 5");
    eq1.updateAST(allocator, false);

    @memcpy(eq2.function[0..11], "x_2 = x_1+2");
    eq2.updateAST(allocator, false);

    defer eq1.deinit();
    defer eq2.deinit();

    const equations = [_]*const Equation{ &eq1, &eq2 };

    var graph = DependencyGraph.init(allocator);
    defer graph.deinit();

    try graph.build(&equations);

    const order = try graph.topologicalSort(2);
    defer allocator.free(order);

    try std.testing.expect(order[0] == 0);
    try std.testing.expect(order[1] == 1);
}

test "evaluate: constraint solving for x" {
    const allocator = std.testing.allocator;

    var eq = Equation.init(rl.Color.red);
    defer eq.deinit();

    @memcpy(eq.function[0..7], "x = y^2");
    eq.updateAST(allocator, false);

    try std.testing.expect(eq.equation_type == .constraint_implicit);

    var vars = std.StringHashMap(f32).init(allocator);
    defer vars.deinit();
    try vars.put("y", 3.0);

    const result = try eq.evaluateConstraint(vars, "x");
    try std.testing.expectApproxEqAbs(result, 9.0, 0.0001);
}

