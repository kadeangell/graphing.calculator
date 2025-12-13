const std = @import("std");
const rl = @import("raylib");
const rm = @import("raylib").math;
const tok = @import("tokenizer.zig");
const equ = @import("equation.zig");
const Token = tok.Token;
const Tokenizer = tok.Tokenizer;
const isVariablePattern = tok.isVariablePattern;
const Equation = equ.Equation;
const DependencyGraph = equ.DependencyGraph;

// AST Node types
pub const ASTNode = union(enum) {
    number: f32,
    variable: []const u8, // 'x', 'y', 'x_1', 'y_2', etc.
    binary_op: *BinaryOp,
    unary_op: *UnaryOp,
    function_call: *FunctionCall,

    pub const BinaryOpType = enum { add, sub, mul, div, pow, mod };
    pub const UnaryOpType = enum { neg, pos };

    pub const BinaryOp = struct {
        op: BinaryOpType,
        left: ASTNode,
        right: ASTNode,
    };

    pub const UnaryOp = struct {
        op: UnaryOpType,
        operand: ASTNode,
    };

    pub const FunctionCall = struct {
        name: []const u8,
        args: []ASTNode,
    };

    pub fn deinit(self: *ASTNode, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .binary_op => |bo| {
                bo.left.deinit(allocator);
                bo.right.deinit(allocator);
                allocator.destroy(bo);
            },
            .unary_op => |uo| {
                uo.operand.deinit(allocator);
                allocator.destroy(uo);
            },
            .function_call => |fc| {
                for (fc.args) |*arg| {
                    arg.deinit(allocator);
                }
                allocator.free(fc.args);
                allocator.destroy(fc);
            },
            else => {},
        }
    }

    pub fn evaluate(self: ASTNode, variables: std.StringHashMap(f32)) !f32 {
        return switch (self) {
            .number => |n| n,
            .variable => |v| blk: {
                break :blk variables.get(v) orelse error.UndefinedVariable;
            },
            .binary_op => |bo| blk: {
                const left = try bo.left.evaluate(variables);
                const right = try bo.right.evaluate(variables);
                break :blk switch (bo.op) {
                    .add => left + right,
                    .sub => left - right,
                    .mul => left * right,
                    .div => left / right,
                    .pow => std.math.pow(f32, left, right),
                    .mod => @mod(left, right),
                };
            },
            .unary_op => |uo| blk: {
                const val = try uo.operand.evaluate(variables);
                break :blk switch (uo.op) {
                    .neg => -val,
                    .pos => val,
                };
            },
            .function_call => |fc| blk: {
                // Single-argument functions
                if (std.mem.eql(u8, fc.name, "sin") or
                    std.mem.eql(u8, fc.name, "cos") or
                    std.mem.eql(u8, fc.name, "tan") or
                    std.mem.eql(u8, fc.name, "sqrt") or
                    std.mem.eql(u8, fc.name, "abs") or
                    std.mem.eql(u8, fc.name, "ln") or
                    std.mem.eql(u8, fc.name, "log") or
                    std.mem.eql(u8, fc.name, "exp") or
                    std.mem.eql(u8, fc.name, "asin") or
                    std.mem.eql(u8, fc.name, "acos") or
                    std.mem.eql(u8, fc.name, "atan") or
                    std.mem.eql(u8, fc.name, "sinh") or
                    std.mem.eql(u8, fc.name, "cosh") or
                    std.mem.eql(u8, fc.name, "tanh") or
                    std.mem.eql(u8, fc.name, "floor") or
                    std.mem.eql(u8, fc.name, "ceil") or
                    std.mem.eql(u8, fc.name, "round")) {

                    if (fc.args.len != 1) return error.WrongArgumentCount;
                    const arg = try fc.args[0].evaluate(variables);

                    if (std.mem.eql(u8, fc.name, "sin")) break :blk @sin(arg);
                    if (std.mem.eql(u8, fc.name, "cos")) break :blk @cos(arg);
                    if (std.mem.eql(u8, fc.name, "tan")) break :blk @tan(arg);
                    if (std.mem.eql(u8, fc.name, "sqrt")) break :blk @sqrt(arg);
                    if (std.mem.eql(u8, fc.name, "abs")) break :blk @abs(arg);
                    if (std.mem.eql(u8, fc.name, "ln")) break :blk @log(arg);
                    if (std.mem.eql(u8, fc.name, "log")) break :blk @log10(arg);
                    if (std.mem.eql(u8, fc.name, "exp")) break :blk @exp(arg);
                    if (std.mem.eql(u8, fc.name, "asin")) break :blk std.math.asin(arg);
                    if (std.mem.eql(u8, fc.name, "acos")) break :blk std.math.acos(arg);
                    if (std.mem.eql(u8, fc.name, "atan")) break :blk std.math.atan(arg);
                    if (std.mem.eql(u8, fc.name, "sinh")) break :blk std.math.sinh(arg);
                    if (std.mem.eql(u8, fc.name, "cosh")) break :blk std.math.cosh(arg);
                    if (std.mem.eql(u8, fc.name, "tanh")) break :blk std.math.tanh(arg);
                    if (std.mem.eql(u8, fc.name, "floor")) break :blk @floor(arg);
                    if (std.mem.eql(u8, fc.name, "ceil")) break :blk @ceil(arg);
                    if (std.mem.eql(u8, fc.name, "round")) break :blk @round(arg);
                }

                // Two-argument functions
                if (std.mem.eql(u8, fc.name, "min") or std.mem.eql(u8, fc.name, "max")) {
                    if (fc.args.len != 2) return error.WrongArgumentCount;
                    const a = try fc.args[0].evaluate(variables);
                    const b = try fc.args[1].evaluate(variables);

                    if (std.mem.eql(u8, fc.name, "min")) break :blk @min(a, b);
                    if (std.mem.eql(u8, fc.name, "max")) break :blk @max(a, b);
                }

                break :blk error.UnknownFunction;
            },
        };
    }

    pub fn toLatex(self: ASTNode, allocator: std.mem.Allocator) ![]u8 {
        return switch (self) {
            .number => |n| std.fmt.allocPrint(allocator, "{d}", .{n}),
            .variable => |v| std.fmt.allocPrint(allocator, "{s}", .{v}),
            .binary_op => |bo| blk: {
                const left = try bo.left.toLatex(allocator);
                defer allocator.free(left);
                const right = try bo.right.toLatex(allocator);
                defer allocator.free(right);

                const op_str = switch (bo.op) {
                    .add => "+",
                    .sub => "-",
                    .mul => "\\cdot ",
                    .div => "/",
                    .pow => "^",
                    .mod => "\\bmod ",
                };

                if (bo.op == .div) {
                    break :blk std.fmt.allocPrint(allocator, "\\frac{{{s}}}{{{s}}}", .{left, right});
                } else if (bo.op == .pow) {
                    break :blk std.fmt.allocPrint(allocator, "{{{s}}}^{{{s}}}", .{left, right});
                } else {
                    break :blk std.fmt.allocPrint(allocator, "({s} {s} {s})", .{left, op_str, right});
                }
            },
            .unary_op => |uo| blk: {
                const operand = try uo.operand.toLatex(allocator);
                defer allocator.free(operand);
                const op_str = switch (uo.op) {
                    .neg => "-",
                    .pos => "+",
                };
                break :blk std.fmt.allocPrint(allocator, "{s}{s}", .{op_str, operand});
            },
            .function_call => |fc| blk: {
                const arg = try fc.args[0].toLatex(allocator);
                defer allocator.free(arg);
                break :blk std.fmt.allocPrint(allocator, "\\{s}({s})", .{fc.name, arg});
            },
        };
    }
};

// Equation types
pub const EquationType = enum {
    assignment,           // x_1 = 5
    function_explicit,    // f(x) = x^2
    function_implicit,    // x^2 (assume f(x) = ...)
    constraint_implicit,  // x = y^2
};

// Top-level equation AST
pub const EquationAST = union(EquationType) {
    assignment: struct {
        var_name: []const u8,
        value: ASTNode,
    },
    function_explicit: struct {
        func_name: []const u8,
        params: [][]const u8,  // parameter names like ["x"] or ["x", "y"]
        body: ASTNode,
    },
    function_implicit: struct {
        body: ASTNode,
    },
    constraint_implicit: struct {
        left: ASTNode,
        right: ASTNode,
    },

    pub fn deinit(self: *EquationAST, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .assignment => |*a| {
                a.value.deinit(allocator);
            },
            .function_explicit => |*f| {
                f.body.deinit(allocator);
                allocator.free(f.params);
            },
            .function_implicit => |*f| {
                f.body.deinit(allocator);
            },
            .constraint_implicit => |*c| {
                c.left.deinit(allocator);
                c.right.deinit(allocator);
            },
        }
    }

    pub fn getDefinedVars(self: EquationAST, allocator: std.mem.Allocator) ![][]const u8 {
        return switch (self) {
            .assignment => |a| blk: {
                var list = std.ArrayList([]const u8){};
                defer list.deinit(allocator);
                try list.append(allocator, a.var_name);
                break :blk try list.toOwnedSlice(allocator);
            },
            .function_explicit => |f| blk: {
                var list = std.ArrayList([]const u8){};
                defer list.deinit(allocator);
                try list.append(allocator, f.func_name);
                break :blk try list.toOwnedSlice(allocator);
            },
            .function_implicit, .constraint_implicit => blk: {
                // Implicit functions/constraints don't define variables
                break :blk try allocator.alloc([]const u8, 0);
            },
        };
    }

    pub fn getReferencedVars(self: EquationAST, allocator: std.mem.Allocator) ![][]const u8 {
        return switch (self) {
            .assignment => |a| try collectVariables(a.value, allocator),
            .function_explicit => |f| try collectVariables(f.body, allocator),
            .function_implicit => |f| try collectVariables(f.body, allocator),
            .constraint_implicit => |c| blk: {
                const left_vars = try collectVariables(c.left, allocator);
                defer allocator.free(left_vars);
                const right_vars = try collectVariables(c.right, allocator);
                defer allocator.free(right_vars);

                var all_vars = std.ArrayList([]const u8){};
                defer all_vars.deinit(allocator);
                try all_vars.appendSlice(allocator, left_vars);
                try all_vars.appendSlice(allocator, right_vars);
                break :blk try all_vars.toOwnedSlice(allocator);
            },
        };
    }
};

// Helper to collect all variables from an AST
fn collectVariables(node: ASTNode, allocator: std.mem.Allocator) ![][]const u8 {
    var vars = std.ArrayList([]const u8){};
    defer vars.deinit(allocator);
    try collectVariablesRecursive(node, &vars, allocator);
    return try vars.toOwnedSlice(allocator);
}

fn collectVariablesRecursive(node: ASTNode, vars: *std.ArrayList([]const u8), allocator: std.mem.Allocator) !void {
    switch (node) {
        .variable => |v| {
            // Check if already in list
            for (vars.items) |existing| {
                if (std.mem.eql(u8, existing, v)) return;
            }
            try vars.append(allocator, v);
        },
        .binary_op => |bo| {
            try collectVariablesRecursive(bo.left, vars, allocator);
            try collectVariablesRecursive(bo.right, vars, allocator);
        },
        .unary_op => |uo| {
            try collectVariablesRecursive(uo.operand, vars, allocator);
        },
        .function_call => |fc| {
            for (fc.args) |arg| {
                try collectVariablesRecursive(arg, vars, allocator);
            }
        },
        .number => {},
    }
}

// Parser error set
pub const ParseError = error{
    InvalidCharacter,
    ExpectedClosingParen,
    UnexpectedIdentifier,
    UnexpectedToken,
    UnexpectedEquals,
    InvalidFloat,
    OutOfMemory,
    CircularDependency,
    UndefinedVariable,
    InvalidFunctionSignature,
    EquationHasError,
    NoAST,
    CannotEvaluateConstraint,
    NotAConstraint,
    CannotSolveConstraint,
    WrongArgumentCount,
    UnknownFunction,
    InvalidExpression,
};
// Recursive descent parser
pub const Parser = struct {
    tokenizer: Tokenizer,
    current_token: Token,
    allocator: std.mem.Allocator,

    pub fn init(input: []const u8, allocator: std.mem.Allocator) ParseError!Parser {
        var tokenizer = Tokenizer.init(input);
        const first_token = try tokenizer.next();
        return .{
            .tokenizer = tokenizer,
            .current_token = first_token,
            .allocator = allocator,
        };
    }

    fn advance(self: *Parser) ParseError!void {
        self.current_token = try self.tokenizer.next();
    }

    pub fn parse(self: *Parser) ParseError!ASTNode {
        return try self.parseExpression();
    }

    fn parseExpression(self: *Parser) ParseError!ASTNode {
        return try self.parseAddSub();
    }

    fn parseAddSub(self: *Parser) ParseError!ASTNode {
        var left = try self.parseMulDiv();

        while (true) {
            const op: ?ASTNode.BinaryOpType = switch (self.current_token) {
                .operator => |o| if (o == '+') .add else if (o == '-') .sub else null,
                else => null,
            };

            if (op == null) break;

            try self.advance();
            const right = try self.parseMulDiv();

            const binary_op = try self.allocator.create(ASTNode.BinaryOp);
            binary_op.* = .{
                .op = op.?,
                .left = left,
                .right = right,
            };
            left = ASTNode{ .binary_op = binary_op };
        }

        return left;
    }

    fn parseMulDiv(self: *Parser) ParseError!ASTNode {
        var left = try self.parsePower();

        while (true) {
            const op: ?ASTNode.BinaryOpType = switch (self.current_token) {
                .operator => |o| if (o == '*') .mul else if (o == '/') .div else if (o == '%') .mod else null,
                else => null,
            };

            if (op == null) break;

            try self.advance();
            const right = try self.parsePower();

            const binary_op = try self.allocator.create(ASTNode.BinaryOp);
            binary_op.* = .{
                .op = op.?,
                .left = left,
                .right = right,
            };
            left = ASTNode{ .binary_op = binary_op };
        }

        return left;
    }

    fn parsePower(self: *Parser) ParseError!ASTNode {
        const left = try self.parseUnary();

        if (self.current_token == .operator and self.current_token.operator == '^') {
            try self.advance();
            const right = try self.parsePower(); // Right associative

            const binary_op = try self.allocator.create(ASTNode.BinaryOp);
            binary_op.* = .{
                .op = .pow,
                .left = left,
                .right = right,
            };
            return ASTNode{ .binary_op = binary_op };
        }

        return left;
    }

    fn parseUnary(self: *Parser) ParseError!ASTNode {
        if (self.current_token == .operator) {
            const op_char = self.current_token.operator;
            if (op_char == '-' or op_char == '+') {
                try self.advance();
                const operand = try self.parseUnary();

                const unary_op = try self.allocator.create(ASTNode.UnaryOp);
                unary_op.* = .{
                    .op = if (op_char == '-') .neg else .pos,
                    .operand = operand,
                };
                return ASTNode{ .unary_op = unary_op };
            }
        }

        return try self.parsePrimary();
    }

    fn parsePrimary(self: *Parser) ParseError!ASTNode {
        switch (self.current_token) {
            .number => |n| {
                try self.advance();
                return ASTNode{ .number = n };
            },
            .variable => |v| {
                try self.advance();
                return ASTNode{ .variable = v };
            },
            .equals => return error.UnexpectedEquals,
            .identifier => |name| {
                try self.advance();
                // Function call
                if (self.current_token == .lparen) {
                    try self.advance(); // consume '('

                    var args = std.ArrayList(ASTNode){};
                    defer args.deinit(self.allocator);

                    // Parse arguments
                    if (self.current_token != .rparen) {
                        while (true) {
                            const arg = try self.parseExpression();
                            try args.append(self.allocator, arg);

                            if (self.current_token == .comma) {
                                try self.advance();
                            } else {
                                break;
                            }
                        }
                    }

                    if (self.current_token != .rparen) {
                        return error.ExpectedClosingParen;
                    }
                    try self.advance(); // consume ')'

                    const func_call = try self.allocator.create(ASTNode.FunctionCall);
                    func_call.* = .{
                        .name = name,
                        .args = try args.toOwnedSlice(self.allocator),
                    };
                    return ASTNode{ .function_call = func_call };
                }
                return error.UnexpectedIdentifier;
            },
            .lparen => {
                try self.advance(); // consume '('
                const expr = try self.parseExpression();
                if (self.current_token != .rparen) {
                    return error.ExpectedClosingParen;
                }
                try self.advance(); // consume ')'
                return expr;
            },
            else => return error.UnexpectedToken,
        }
    }
};

pub fn parseEquation(input: []const u8, allocator: std.mem.Allocator) ParseError!ASTNode {
    var parser = try Parser.init(input, allocator);
    return try parser.parse();
}

// Parse a full equation (with = support)
pub fn parseFullEquation(input: []const u8, allocator: std.mem.Allocator) ParseError!EquationAST {
    // First, check if there's an '=' in the input
    const equals_pos = std.mem.indexOf(u8, input, "=");

    if (equals_pos == null) {
        // No '=' → implicit function
        const body = try parseEquation(input, allocator);
        return EquationAST{
            .function_implicit = .{ .body = body },
        };
    }

    // Split on '='
    const left_str = std.mem.trim(u8, input[0..equals_pos.?], " \t\n\r");
    const right_str = std.mem.trim(u8, input[equals_pos.? + 1..], " \t\n\r");

    // Parse the right side first
    var right_ast = try parseEquation(right_str, allocator);

    // Now determine what the left side is
    // Try to parse it as an expression
    var left_ast = parseEquation(left_str, allocator) catch |err| {
        // If left side fails to parse, it might be invalid
        right_ast.deinit(allocator);
        return err;
    };

    // Check what kind of equation this is based on the left side
    switch (left_ast) {
        .variable => |var_name| {
            // Simple variable on left
            // Check if right side contains other variables
            const right_vars = try collectVariables(right_ast, allocator);
            defer allocator.free(right_vars);

            if (right_vars.len == 0) {
                // Right side only has numbers/constants → assignment
                return EquationAST{
                    .assignment = .{
                        .var_name = var_name,
                        .value = right_ast,
                    },
                };
            }

            // Check for special case: y = f(x) or x = g(y)
            // If left is y and right only has x → treat as implicit function
            // If left is x and right only has y → constraint
            const is_y_equals_fx = std.mem.eql(u8, var_name, "y") and
                                  right_vars.len == 1 and
                                  std.mem.eql(u8, right_vars[0], "x");

            const is_x_equals_gy = std.mem.eql(u8, var_name, "x") and
                                  right_vars.len == 1 and
                                  std.mem.eql(u8, right_vars[0], "y");

            if (is_y_equals_fx) {
                // y = f(x) → treat as implicit function
                return EquationAST{
                    .function_implicit = .{
                        .body = right_ast,
                    },
                };
            } else if (is_x_equals_gy) {
                // x = g(y) → constraint
                return EquationAST{
                    .constraint_implicit = .{
                        .left = left_ast,
                        .right = right_ast,
                    },
                };
            } else {
                // Other cases → treat as constraint
                return EquationAST{
                    .constraint_implicit = .{
                        .left = left_ast,
                        .right = right_ast,
                    },
                };
            }
        },
        .function_call => |fc| {
            // Check if it's a built-in function or user-defined
            const is_builtin = std.mem.eql(u8, fc.name, "sin") or
                              std.mem.eql(u8, fc.name, "cos") or
                              std.mem.eql(u8, fc.name, "tan") or
                              std.mem.eql(u8, fc.name, "sqrt") or
                              std.mem.eql(u8, fc.name, "abs") or
                              std.mem.eql(u8, fc.name, "ln") or
                              std.mem.eql(u8, fc.name, "log") or
                              std.mem.eql(u8, fc.name, "exp") or
                              std.mem.eql(u8, fc.name, "asin") or
                              std.mem.eql(u8, fc.name, "acos") or
                              std.mem.eql(u8, fc.name, "atan") or
                              std.mem.eql(u8, fc.name, "sinh") or
                              std.mem.eql(u8, fc.name, "cosh") or
                              std.mem.eql(u8, fc.name, "tanh") or
                              std.mem.eql(u8, fc.name, "floor") or
                              std.mem.eql(u8, fc.name, "ceil") or
                              std.mem.eql(u8, fc.name, "round") or
                              std.mem.eql(u8, fc.name, "min") or
                              std.mem.eql(u8, fc.name, "max");

            if (is_builtin) {
                // Built-in function on left → constraint
                return EquationAST{
                    .constraint_implicit = .{
                        .left = left_ast,
                        .right = right_ast,
                    },
                };
            }

            // User-defined function on left → explicit function: f(x) = x^2
            // Extract parameter names from the function call args
            var params = std.ArrayList([]const u8){};
            defer params.deinit(allocator);

            for (fc.args) |arg| {
                if (arg == .variable) {
                    try params.append(allocator, arg.variable);
                } else {
                    // Parameters must be simple variables
                    left_ast.deinit(allocator);
                    right_ast.deinit(allocator);
                    return error.InvalidFunctionSignature;
                }
            }

            const param_slice = try params.toOwnedSlice(allocator);

            // Clean up the left AST (we've extracted what we need)
            allocator.destroy(fc);

            return EquationAST{
                .function_explicit = .{
                    .func_name = fc.name,
                    .params = param_slice,
                    .body = right_ast,
                },
            };
        },
        else => {
            // Expression on left → implicit constraint: x = y^2 or x^2 = y
            return EquationAST{
                .constraint_implicit = .{
                    .left = left_ast,
                    .right = right_ast,
                },
            };
        },
    }
}

// ============================================================================
// TESTS  
// ============================================================================

test "parser: simple addition" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("1 + 2", allocator);
    defer ast.deinit(allocator);

    try std.testing.expect(ast == .binary_op);
    try std.testing.expect(ast.binary_op.op == .add);
}

test "parser: multiplication precedence" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("1 + 2 * 3", allocator);
    defer ast.deinit(allocator);

    // Should parse as: 1 + (2 * 3)
    try std.testing.expect(ast == .binary_op);
    try std.testing.expect(ast.binary_op.op == .add);
    try std.testing.expect(ast.binary_op.right == .binary_op);
    try std.testing.expect(ast.binary_op.right.binary_op.op == .mul);
}

test "parser: power right associativity" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("2 ^ 3 ^ 2", allocator);
    defer ast.deinit(allocator);

    // Should parse as: 2 ^ (3 ^ 2) = 2 ^ 9 = 512
    try std.testing.expect(ast == .binary_op);
    try std.testing.expect(ast.binary_op.op == .pow);
    try std.testing.expect(ast.binary_op.right == .binary_op);
    try std.testing.expect(ast.binary_op.right.binary_op.op == .pow);
}

test "parser: parentheses" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("(1 + 2) * 3", allocator);
    defer ast.deinit(allocator);

    // Should parse as: (1 + 2) * 3
    try std.testing.expect(ast == .binary_op);
    try std.testing.expect(ast.binary_op.op == .mul);
    try std.testing.expect(ast.binary_op.left == .binary_op);
    try std.testing.expect(ast.binary_op.left.binary_op.op == .add);
}

test "parser: implicit multiplication" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("2x", allocator);
    defer ast.deinit(allocator);

    try std.testing.expect(ast == .binary_op);
    try std.testing.expect(ast.binary_op.op == .mul);
    try std.testing.expect(ast.binary_op.left == .number);
    try std.testing.expect(ast.binary_op.right == .variable);
}

test "parser: unary minus" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("-x", allocator);
    defer ast.deinit(allocator);

    try std.testing.expect(ast == .unary_op);
    try std.testing.expect(ast.unary_op.op == .neg);
    try std.testing.expect(ast.unary_op.operand == .variable);
}

test "parser: function call" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("sin(x)", allocator);
    defer ast.deinit(allocator);

    try std.testing.expect(ast == .function_call);
    try std.testing.expect(std.mem.eql(u8, ast.function_call.name, "sin"));
    try std.testing.expect(ast.function_call.args.len == 1);
}

test "evaluation: simple arithmetic" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("2 + 3 * 4", allocator);
    defer ast.deinit(allocator);

    var vars = std.StringHashMap(f32).init(allocator);
    defer vars.deinit();

    const result = try ast.evaluate(vars);
    try std.testing.expectApproxEqAbs(result, 14.0, 0.0001);
}

test "evaluation: with variable" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("2 * x + 1", allocator);
    defer ast.deinit(allocator);

    var vars = std.StringHashMap(f32).init(allocator);
    defer vars.deinit();
    try vars.put("x", 5.0);

    const result = try ast.evaluate(vars);
    try std.testing.expectApproxEqAbs(result, 11.0, 0.0001);
}

test "evaluation: implicit multiplication" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("2x + 3", allocator);
    defer ast.deinit(allocator);

    var vars = std.StringHashMap(f32).init(allocator);
    defer vars.deinit();
    try vars.put("x", 4.0);

    const result = try ast.evaluate(vars);
    try std.testing.expectApproxEqAbs(result, 11.0, 0.0001);
}

test "evaluation: power operation" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("2 ^ 3", allocator);
    defer ast.deinit(allocator);

    var vars = std.StringHashMap(f32).init(allocator);
    defer vars.deinit();

    const result = try ast.evaluate(vars);
    try std.testing.expectApproxEqAbs(result, 8.0, 0.0001);
}

test "evaluation: trig functions" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("sin(0)", allocator);
    defer ast.deinit(allocator);

    var vars = std.StringHashMap(f32).init(allocator);
    defer vars.deinit();

    const result = try ast.evaluate(vars);
    try std.testing.expectApproxEqAbs(result, 0.0, 0.0001);
}

test "evaluation: complex expression" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("2x^2 + 3x + 1", allocator);
    defer ast.deinit(allocator);

    var vars = std.StringHashMap(f32).init(allocator);
    defer vars.deinit();
    try vars.put("x", 2.0);

    // 2(4) + 3(2) + 1 = 8 + 6 + 1 = 15
    const result = try ast.evaluate(vars);
    try std.testing.expectApproxEqAbs(result, 15.0, 0.0001);
}

test "evaluation: nested parentheses with implicit mult" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("2(x+1)(x-1)", allocator);
    defer ast.deinit(allocator);

    var vars = std.StringHashMap(f32).init(allocator);
    defer vars.deinit();
    try vars.put("x", 3.0);

    // 2(3+1)(3-1) = 2(4)(2) = 16
    const result = try ast.evaluate(vars);
    try std.testing.expectApproxEqAbs(result, 16.0, 0.0001);
}

test "error: invalid character" {
    const allocator = std.testing.allocator;

    const result = parseEquation("2 @ 3", allocator);
    try std.testing.expectError(error.InvalidCharacter, result);
}

test "error: missing closing paren" {
    const allocator = std.testing.allocator;

    const result = parseEquation("(2 + 3", allocator);
    try std.testing.expectError(error.ExpectedClosingParen, result);
}

test "error: undefined variable" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("x + y", allocator);
    defer ast.deinit(allocator);

    var vars = std.StringHashMap(f32).init(allocator);
    defer vars.deinit();
    try vars.put("x", 1.0);
    // y is not defined

    const result = ast.evaluate(vars);
    try std.testing.expectError(error.UndefinedVariable, result);
}

test "latex: simple expression" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("x + 2", allocator);
    defer ast.deinit(allocator);

    const latex = try ast.toLatex(allocator);
    defer allocator.free(latex);

    // Should contain x and 2
    try std.testing.expect(std.mem.indexOf(u8, latex, "x") != null);
    try std.testing.expect(std.mem.indexOf(u8, latex, "2") != null);
}

test "latex: fraction" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("x / 2", allocator);
    defer ast.deinit(allocator);

    const latex = try ast.toLatex(allocator);
    defer allocator.free(latex);

    // Should use \frac
    try std.testing.expect(std.mem.indexOf(u8, latex, "\\frac") != null);
}

test "latex: power" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("x ^ 2", allocator);
    defer ast.deinit(allocator);

    const latex = try ast.toLatex(allocator);
    defer allocator.free(latex);

    // Should use ^ for power
    try std.testing.expect(std.mem.indexOf(u8, latex, "^") != null);
}

// ============================================================================
// NEW TESTS FOR EQUATION SYSTEM
// ============================================================================

test "tokenizer: multi-char variable x_1" {
    var tokenizer = Tokenizer.init("x_1");

    const t1 = try tokenizer.next();
    try std.testing.expect(t1 == .variable);
    try std.testing.expect(std.mem.eql(u8, t1.variable, "x_1"));
}

test "tokenizer: equals token" {
    var tokenizer = Tokenizer.init("x=5");

    const t1 = try tokenizer.next();
    try std.testing.expect(t1 == .variable);

    const t2 = try tokenizer.next();
    try std.testing.expect(t2 == .equals);

    const t3 = try tokenizer.next();
    try std.testing.expect(t3 == .number);
}

test "isVariablePattern: single letter" {
    try std.testing.expect(isVariablePattern("x") == true);
    try std.testing.expect(isVariablePattern("y") == true);
    try std.testing.expect(isVariablePattern("z") == true);
}

test "isVariablePattern: underscore notation" {
    try std.testing.expect(isVariablePattern("x_1") == true);
    try std.testing.expect(isVariablePattern("y_2") == true);
    try std.testing.expect(isVariablePattern("theta_1") == true);
}

test "isVariablePattern: not variables" {
    try std.testing.expect(isVariablePattern("sin") == false);
    try std.testing.expect(isVariablePattern("pi") == false);
    try std.testing.expect(isVariablePattern("x_") == false);  // Missing number
    try std.testing.expect(isVariablePattern("_1") == false);  // Starts with underscore
    try std.testing.expect(isVariablePattern("x__1") == false);  // Double underscore
}

test "parse full equation: implicit function" {
    const allocator = std.testing.allocator;

    var eq_ast = try parseFullEquation("x^2", allocator);
    defer eq_ast.deinit(allocator);

    try std.testing.expect(eq_ast == .function_implicit);
}

test "parse full equation: assignment" {
    const allocator = std.testing.allocator;

    var eq_ast = try parseFullEquation("x_1 = 5", allocator);
    defer eq_ast.deinit(allocator);

    try std.testing.expect(eq_ast == .assignment);
    try std.testing.expect(std.mem.eql(u8, eq_ast.assignment.var_name, "x_1"));
}

test "parse full equation: implicit constraint" {
    const allocator = std.testing.allocator;

    var eq_ast = try parseFullEquation("x = y^2", allocator);
    defer eq_ast.deinit(allocator);

    try std.testing.expect(eq_ast == .constraint_implicit);
}

test "dependency graph: simple chain" {
    const allocator = std.testing.allocator;

    // Create test equations
    var eq1 = Equation.init(rl.Color.red);
    var eq2 = Equation.init(rl.Color.blue);

    // x_1 = 5
    @memcpy(eq1.function[0..7], "x_1 = 5");
    eq1.updateAST(allocator, false);

    // x_2 = x_1 + 2
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

    // eq1 should come before eq2
    try std.testing.expect(order[0] == 0);
    try std.testing.expect(order[1] == 1);
}

test "evaluation: multi-char variable" {
    const allocator = std.testing.allocator;

    var ast = try parseEquation("x_1 + 5", allocator);
    defer ast.deinit(allocator);

    var vars = std.StringHashMap(f32).init(allocator);
    defer vars.deinit();
    try vars.put("x_1", 10.0);

    const result = try ast.evaluate(vars);
    try std.testing.expectApproxEqAbs(result, 15.0, 0.0001);
}

// ============================================================================
// COMPLEX EQUATION TESTS
// ============================================================================

test "parse: complex constraint y^2 = sin(x)" {
    const allocator = std.testing.allocator;

    var eq_ast = try parseFullEquation("y^2 = sin(x)", allocator);
    defer eq_ast.deinit(allocator);

    try std.testing.expect(eq_ast == .constraint_implicit);

    // Verify structure
    const c = eq_ast.constraint_implicit;
    try std.testing.expect(c.left == .binary_op);
    try std.testing.expect(c.left.binary_op.op == .pow);
    try std.testing.expect(c.right == .function_call);
}

test "parse: circle equation x^2 + y^2 = 25" {
    const allocator = std.testing.allocator;

    var eq_ast = try parseFullEquation("x^2 + y^2 = 25", allocator);
    defer eq_ast.deinit(allocator);

    try std.testing.expect(eq_ast == .constraint_implicit);

    // Left side should be addition of two powers
    const c = eq_ast.constraint_implicit;
    try std.testing.expect(c.left == .binary_op);
    try std.testing.expect(c.left.binary_op.op == .add);
}

test "parse: complex assignment with trig" {
    const allocator = std.testing.allocator;

    var eq_ast = try parseFullEquation("x_1 = sin(x) + cos(y)", allocator);
    defer eq_ast.deinit(allocator);

    // Should be a constraint because right side has both x and y
    try std.testing.expect(eq_ast == .constraint_implicit);
}

test "parse: assignment with another variable" {
    const allocator = std.testing.allocator;

    var eq_ast = try parseFullEquation("y_1 = x_1 * 2", allocator);
    defer eq_ast.deinit(allocator);

    // y_1 uses x_1, so it's a constraint
    try std.testing.expect(eq_ast == .constraint_implicit);
}

test "parse: pure constant assignment" {
    const allocator = std.testing.allocator;

    var eq_ast = try parseFullEquation("x_1 = 5.5", allocator);
    defer eq_ast.deinit(allocator);

    try std.testing.expect(eq_ast == .assignment);
    try std.testing.expect(std.mem.eql(u8, eq_ast.assignment.var_name, "x_1"));
}

test "parse: assignment with expression" {
    const allocator = std.testing.allocator;

    var eq_ast = try parseFullEquation("x_1 = 2 * pi + 3", allocator);
    defer eq_ast.deinit(allocator);

    try std.testing.expect(eq_ast == .assignment);
}

test "parse: nested function constraint" {
    const allocator = std.testing.allocator;

    var eq_ast = try parseFullEquation("sin(x) = cos(y)", allocator);
    defer eq_ast.deinit(allocator);

    try std.testing.expect(eq_ast == .constraint_implicit);
}

test "parse: implicit multiplication in constraint" {
    const allocator = std.testing.allocator;

    var eq_ast = try parseFullEquation("2x = 3y", allocator);
    defer eq_ast.deinit(allocator);

    try std.testing.expect(eq_ast == .constraint_implicit);

    // Both sides should have multiplication
    const c = eq_ast.constraint_implicit;
    try std.testing.expect(c.left == .binary_op);
    try std.testing.expect(c.left.binary_op.op == .mul);
    try std.testing.expect(c.right == .binary_op);
    try std.testing.expect(c.right.binary_op.op == .mul);
}

test "evaluate: constraint solving for x" {
    const allocator = std.testing.allocator;

    var eq = Equation.init(rl.Color.red);
    defer eq.deinit();

    // x = y^2
    @memcpy(eq.function[0..7], "x = y^2");
    eq.updateAST(allocator, false);

    try std.testing.expect(eq.equation_type == .constraint_implicit);

    var vars = std.StringHashMap(f32).init(allocator);
    defer vars.deinit();
    try vars.put("y", 3.0);

    const result = try eq.evaluateConstraint(vars, "x");
    try std.testing.expectApproxEqAbs(result, 9.0, 0.0001);
}

test "parse: complex polynomial" {
    const allocator = std.testing.allocator;

    var eq_ast = try parseFullEquation("x^3 - 2x^2 + 3x - 1", allocator);
    defer eq_ast.deinit(allocator);

    try std.testing.expect(eq_ast == .function_implicit);
}

test "parse: rational function" {
    const allocator = std.testing.allocator;

    var eq_ast = try parseFullEquation("(x + 1) / (x - 1)", allocator);
    defer eq_ast.deinit(allocator);

    try std.testing.expect(eq_ast == .function_implicit);
}

// ============================================================================
// MALFORMED EQUATION TESTS
// ============================================================================

test "error: empty function call sin()" {
    const allocator = std.testing.allocator;

    // sin() with no arguments parses successfully
    var ast = try parseEquation("sin()", allocator);
    defer ast.deinit(allocator);

    try std.testing.expect(ast == .function_call);
    try std.testing.expect(ast.function_call.args.len == 0);

    // But evaluation should fail with WrongArgumentCount error
    var vars = std.StringHashMap(f32).init(allocator);
    defer vars.deinit();

    const result = ast.evaluate(vars);
    try std.testing.expectError(error.WrongArgumentCount, result);
}

test "error: unmatched parentheses in equation" {
    const allocator = std.testing.allocator;

    const result = parseFullEquation("x = (y + 1", allocator);
    try std.testing.expectError(error.ExpectedClosingParen, result);
}

test "error: invalid operator sequence" {
    const allocator = std.testing.allocator;

    const result = parseFullEquation("x = ++y", allocator);
    // Should parse but might give unexpected results - this is actually valid (unary plus twice)
    if (result) |ast| {
        var eq_ast = ast;
        defer eq_ast.deinit(allocator);
    } else |_| {}
}

test "error: division by zero detection" {
    const allocator = std.testing.allocator;

    var eq = Equation.init(rl.Color.red);
    defer eq.deinit();

    @memcpy(eq.function[0..7], "x_1 = 1");
    eq.updateAST(allocator, false);

    var vars = std.StringHashMap(f32).init(allocator);
    defer vars.deinit();

    // Evaluating 1/0 should return inf, not error
    var ast2 = try parseEquation("1 / 0", allocator);
    defer ast2.deinit(allocator);

    const result = try ast2.evaluate(vars);
    try std.testing.expect(std.math.isInf(result));
}

test "error: function with wrong arg count" {
    const allocator = std.testing.allocator;

    // max expects 2 args, this has only 1
    var ast = try parseEquation("max(5)", allocator);
    defer ast.deinit(allocator);

    var vars = std.StringHashMap(f32).init(allocator);
    defer vars.deinit();

    // Evaluation should fail with WrongArgumentCount
    const result = ast.evaluate(vars);
    try std.testing.expectError(error.WrongArgumentCount, result);
}

test "error: just equals sign" {
    const allocator = std.testing.allocator;

    const result = parseFullEquation("=", allocator);
    try std.testing.expectError(error.UnexpectedToken, result);
}

test "error: multiple equals signs" {
    const allocator = std.testing.allocator;

    // Currently this will just split on the first =
    // "x = y = 5" becomes "x" = "y = 5", which parses y=5 as expression
    var eq_ast = try parseFullEquation("x = y = 5", allocator);
    defer eq_ast.deinit(allocator);

    // This currently works but might not be desired behavior
    // It treats it as x = (y = 5) where "y = 5" fails to parse as expression
}

