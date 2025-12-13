const std = @import("std");

// Helper function to check if identifier is a variable pattern
pub fn isVariablePattern(ident: []const u8) bool {
    if (ident.len == 0) return false;

    // Single letter: x, y, z, etc.
    if (ident.len == 1) return true;

    // Underscore pattern: x_1, y_2, theta_1, etc.
    // Format: letter(s) + '_' + digit(s)
    var found_underscore = false;
    var has_digits_after = false;

    for (ident, 0..) |ch, i| {
        if (ch == '_') {
            if (found_underscore or i == 0) return false; // Only one underscore, not at start
            found_underscore = true;
        } else if (found_underscore) {
            if (!std.ascii.isDigit(ch)) return false; // After underscore, only digits
            has_digits_after = true;
        } else {
            if (!std.ascii.isAlphabetic(ch)) return false; // Before underscore, only letters
        }
    }

    // Must have found underscore and had digits after it
    return found_underscore and has_digits_after;
}

// Token types
pub const Token = union(enum) {
    number: f32,
    variable: []const u8,  // Supports multi-char like x_1
    operator: u8, // +, -, *, /, ^, %
    lparen,
    rparen,
    comma,
    equals,
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
            .number => (std.ascii.isAlphabetic(next_char) or next_char == '(') and next_char != '=',
            .variable => (next_char == '(' or std.ascii.isAlphabetic(next_char) or std.ascii.isDigit(next_char)) and next_char != '=',
            .rparen => (next_char == '(' or std.ascii.isAlphabetic(next_char) or std.ascii.isDigit(next_char)) and next_char != '=',
            else => false,
        };
    }

    pub fn next(self: *Tokenizer) error{ InvalidCharacter, InvalidFloat }!Token {
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
            const start = self.pos;
            while (self.peek()) |ch| {
                if (!std.ascii.isDigit(ch) and ch != '.') break;
                _ = self.advance();
            }
            const num_str = self.input[start..self.pos];
            const num = std.fmt.parseFloat(f32, num_str) catch return error.InvalidFloat;
            break :blk Token{ .number = num };
        } else if (std.ascii.isAlphabetic(c)) blk: {
            const start = self.pos;
            while (self.peek()) |ch| {
                if (!std.ascii.isAlphanumeric(ch) and ch != '_') break;
                _ = self.advance();
            }
            const ident = self.input[start..self.pos];

            // Check for constants first
            if (std.mem.eql(u8, ident, "pi")) {
                break :blk Token{ .number = std.math.pi };
            } else if (std.mem.eql(u8, ident, "e")) {
                break :blk Token{ .number = std.math.e };
            }

            // Check if it's a variable pattern
            if (isVariablePattern(ident)) {
                break :blk Token{ .variable = ident };
            }

            // Otherwise it's a function name
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
            '=' => blk: {
                _ = self.advance();
                break :blk .equals;
            },
            else => return error.InvalidCharacter,
        };

        self.prev_token = token;
        return token;
    }
};

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
    try std.testing.expect(t3 == .variable and std.mem.eql(u8, t3.variable, "x"));
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
