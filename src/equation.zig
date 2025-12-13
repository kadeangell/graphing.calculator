const std = @import("std");
const rl = @import("raylib");
const rm = @import("raylib").math;

// AST Node types
pub const ASTNode = union(enum) {
    number: f32,
    variable: u8, // 'x', 'y', 't', 'r', etc.
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
                const var_name = &[_]u8{v};
                break :blk variables.get(var_name) orelse error.UndefinedVariable;
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
                if (std.mem.eql(u8, fc.name, "sin")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk @sin(arg);
                } else if (std.mem.eql(u8, fc.name, "cos")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk @cos(arg);
                } else if (std.mem.eql(u8, fc.name, "tan")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk @tan(arg);
                } else if (std.mem.eql(u8, fc.name, "sqrt")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk @sqrt(arg);
                } else if (std.mem.eql(u8, fc.name, "abs")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk @abs(arg);
                } else if (std.mem.eql(u8, fc.name, "ln")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk @log(arg);
                } else if (std.mem.eql(u8, fc.name, "log")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk @log10(arg);
                } else if (std.mem.eql(u8, fc.name, "exp")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk @exp(arg);
                } else if (std.mem.eql(u8, fc.name, "asin")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk std.math.asin(arg);
                } else if (std.mem.eql(u8, fc.name, "acos")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk std.math.acos(arg);
                } else if (std.mem.eql(u8, fc.name, "atan")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk std.math.atan(arg);
                } else if (std.mem.eql(u8, fc.name, "sinh")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk std.math.sinh(arg);
                } else if (std.mem.eql(u8, fc.name, "cosh")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk std.math.cosh(arg);
                } else if (std.mem.eql(u8, fc.name, "tanh")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk std.math.tanh(arg);
                } else if (std.mem.eql(u8, fc.name, "floor")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk @floor(arg);
                } else if (std.mem.eql(u8, fc.name, "ceil")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk @ceil(arg);
                } else if (std.mem.eql(u8, fc.name, "round")) {
                    const arg = try fc.args[0].evaluate(variables);
                    break :blk @round(arg);
                } else if (std.mem.eql(u8, fc.name, "min")) {
                    const a = try fc.args[0].evaluate(variables);
                    const b = try fc.args[1].evaluate(variables);
                    break :blk @min(a, b);
                } else if (std.mem.eql(u8, fc.name, "max")) {
                    const a = try fc.args[0].evaluate(variables);
                    const b = try fc.args[1].evaluate(variables);
                    break :blk @max(a, b);
                }
                break :blk error.UnknownFunction;
            },
        };
    }

    pub fn toLatex(self: ASTNode, allocator: std.mem.Allocator) ![]u8 {
        return switch (self) {
            .number => |n| std.fmt.allocPrint(allocator, "{d}", .{n}),
            .variable => |v| std.fmt.allocPrint(allocator, "{c}", .{v}),
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

pub const Equation = struct {
    function: [256:0]u8,
    edit_mode: bool,
    color: rl.Color,
    ast: ?ASTNode,
    allocator: ?std.mem.Allocator,
    has_error: bool,
    error_msg: [128:0]u8,

    pub fn init(color: rl.Color) Equation {
        return .{
            .function = std.mem.zeroes([256:0]u8),
            .edit_mode = false,
            .color = color,
            .ast = null,
            .allocator = null,
            .has_error = false,
            .error_msg = std.mem.zeroes([128:0]u8),
        };
    }

    pub fn deinit(self: *Equation) void {
        if (self.ast) |*ast| {
            if (self.allocator) |alloc| {
                ast.deinit(alloc);
            }
        }
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
            if (self.ast) |*ast| {
                if (self.allocator) |alloc| {
                    ast.deinit(alloc);
                }
            }
            self.ast = null;
            self.clearError();
            return;
        }

        const func_str = self.function[0..func_len];

        // Try to parse
        if (parseEquation(func_str, allocator)) |ast| {
            // Parsing succeeded - clear old AST and use new one
            if (self.ast) |*old_ast| {
                if (self.allocator) |alloc| {
                    old_ast.deinit(alloc);
                }
            }
            self.ast = ast;
            self.allocator = allocator;
            self.clearError();
        } else |err| {
            if (optimistic) {
                // Optimistic mode: keep old AST, don't show error
                // User is probably still typing
                return;
            } else {
                // Not optimistic: clear AST and set error
                if (self.ast) |*ast| {
                    if (self.allocator) |alloc| {
                        ast.deinit(alloc);
                    }
                }
                self.ast = null;
                const err_name = @errorName(err);
                self.setError(err_name);
            }
        }
    }

    pub fn evaluate(self: *const Equation, x_val: f32, allocator: std.mem.Allocator) !f32 {
        if (self.has_error) return error.EquationHasError;

        const ast = self.ast orelse return error.NoAST;

        var vars = std.StringHashMap(f32).init(allocator);
        defer vars.deinit();
        try vars.put("x", x_val);

        return try ast.evaluate(vars);
    }

    pub fn getLatex(self: *const Equation, allocator: std.mem.Allocator) ![]u8 {
        const ast = self.ast orelse return error.NoAST;
        return try ast.toLatex(allocator);
    }
};

// Tokenizer
pub const Token = union(enum) {
    number: f32,
    variable: u8,
    operator: u8, // +, -, *, /, ^, %
    lparen,
    rparen,
    comma,
    identifier: []const u8,
    eof,
};

pub const Tokenizer = struct {
    input: []const u8,
    pos: usize,
    prev_token: ?Token,
    pending_mult: bool,

    pub fn init(input: []const u8) Tokenizer {
        return .{
            .input = input,
            .pos = 0,
            .prev_token = null,
            .pending_mult = false,
        };
    }

    fn peek(self: *Tokenizer) ?u8 {
        if (self.pos >= self.input.len) return null;
        return self.input[self.pos];
    }

    fn advance(self: *Tokenizer) ?u8 {
        if (self.pos >= self.input.len) return null;
        const c = self.input[self.pos];
        self.pos += 1;
        return c;
    }

    fn skipWhitespace(self: *Tokenizer) void {
        while (self.peek()) |c| {
            if (c != ' ' and c != '\t' and c != '\n' and c != '\r') break;
            _ = self.advance();
        }
    }

    fn shouldInsertMultiplication(prev: Token, next_char: u8) bool {
        return switch (prev) {
            // Number followed by letter or opening paren (but not another number)
            .number => std.ascii.isAlphabetic(next_char) or next_char == '(',
            // Variable followed by opening paren, letter, or number
            .variable => next_char == '(' or std.ascii.isAlphabetic(next_char) or std.ascii.isDigit(next_char),
            // Closing paren followed by opening paren, letter, or number
            .rparen => next_char == '(' or std.ascii.isAlphabetic(next_char) or std.ascii.isDigit(next_char),
            else => false,
        };
    }

    pub fn next(self: *Tokenizer) ParseError!Token {
        const had_pending = self.pending_mult;
        if (self.pending_mult) {
            self.pending_mult = false;
        }

        self.skipWhitespace();

        const c = self.peek() orelse {
            self.prev_token = .eof;
            return .eof;
        };

        // Check if we need to insert implicit multiplication
        // Don't check if we just processed a pending multiplication
        if (!had_pending and self.prev_token != null) {
            const prev = self.prev_token.?;
            if (prev != .operator and shouldInsertMultiplication(prev, c)) {
                self.pending_mult = true;
                const mult_token = Token{ .operator = '*' };
                self.prev_token = mult_token;
                return mult_token;
            }
        }

        // Parse the actual token
        const token: Token = if (std.ascii.isDigit(c) or c == '.') blk: {
            // Numbers
            const start = self.pos;
            while (self.peek()) |ch| {
                if (!std.ascii.isDigit(ch) and ch != '.') break;
                _ = self.advance();
            }
            const num_str = self.input[start..self.pos];
            const num = try std.fmt.parseFloat(f32, num_str);
            break :blk Token{ .number = num };
        } else if (std.ascii.isAlphabetic(c)) blk: {
            // Variables and identifiers
            const start = self.pos;
            while (self.peek()) |ch| {
                if (!std.ascii.isAlphanumeric(ch)) break;
                _ = self.advance();
            }
            const ident = self.input[start..self.pos];

            // Check for constants first (before single letter check)
            if (std.mem.eql(u8, ident, "pi")) {
                break :blk Token{ .number = std.math.pi };
            } else if (std.mem.eql(u8, ident, "e")) {
                break :blk Token{ .number = std.math.e };
            }

            // Single letter variables
            if (ident.len == 1) {
                break :blk Token{ .variable = ident[0] };
            }

            break :blk Token{ .identifier = ident };
        } else switch (c) {
            '+', '-', '*', '/', '^', '%' => blk: {
                _ = self.advance();
                break :blk Token{ .operator = c };
            },
            '(' => blk: {
                _ = self.advance();
                break :blk .lparen;
            },
            ')' => blk: {
                _ = self.advance();
                break :blk .rparen;
            },
            ',' => blk: {
                _ = self.advance();
                break :blk .comma;
            },
            else => return error.InvalidCharacter,
        };

        // Store the token before returning
        self.prev_token = token;
        return token;
    }
};

// Parser error set
pub const ParseError = error{
    InvalidCharacter,
    ExpectedClosingParen,
    UnexpectedIdentifier,
    UnexpectedToken,
    InvalidFloat,
    OutOfMemory,
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

// Helper function to parse and create AST
pub fn parseEquation(input: []const u8, allocator: std.mem.Allocator) ParseError!ASTNode {
    var parser = try Parser.init(input, allocator);
    return try parser.parse();
}

// ============================================================================
// TESTS
// ============================================================================

test "tokenizer: basic tokens" {
    var tokenizer = Tokenizer.init("1 + 2");

    const t1 = try tokenizer.next();
    try std.testing.expect(t1 == .number and t1.number == 1.0);

    const t2 = try tokenizer.next();
    try std.testing.expect(t2 == .operator and t2.operator == '+');

    const t3 = try tokenizer.next();
    try std.testing.expect(t3 == .number and t3.number == 2.0);

    const t4 = try tokenizer.next();
    try std.testing.expect(t4 == .eof);
}

test "tokenizer: implicit multiplication - number and variable" {
    var tokenizer = Tokenizer.init("2x");

    const t1 = try tokenizer.next();
    try std.testing.expect(t1 == .number and t1.number == 2.0);

    const t2 = try tokenizer.next();
    try std.testing.expect(t2 == .operator and t2.operator == '*');

    const t3 = try tokenizer.next();
    try std.testing.expect(t3 == .variable and t3.variable == 'x');
}

test "tokenizer: implicit multiplication - number and parenthesis" {
    var tokenizer = Tokenizer.init("2(x)");

    const t1 = try tokenizer.next();
    try std.testing.expect(t1 == .number and t1.number == 2.0);

    const t2 = try tokenizer.next();
    try std.testing.expect(t2 == .operator and t2.operator == '*');

    const t3 = try tokenizer.next();
    try std.testing.expect(t3 == .lparen);
}

test "tokenizer: implicit multiplication - parenthesis combinations" {
    var tokenizer = Tokenizer.init("(x)(y)");

    _ = try tokenizer.next(); // (
    _ = try tokenizer.next(); // x
    _ = try tokenizer.next(); // )

    const mult = try tokenizer.next();
    try std.testing.expect(mult == .operator and mult.operator == '*');

    const lparen = try tokenizer.next();
    try std.testing.expect(lparen == .lparen);
}

test "tokenizer: constants" {
    var tokenizer1 = Tokenizer.init("pi");
    const t1 = try tokenizer1.next();
    try std.testing.expect(t1 == .number);
    try std.testing.expectApproxEqAbs(t1.number, std.math.pi, 0.0001);

    var tokenizer2 = Tokenizer.init("e");
    const t2 = try tokenizer2.next();
    try std.testing.expect(t2 == .number);
    try std.testing.expectApproxEqAbs(t2.number, std.math.e, 0.0001);
}

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