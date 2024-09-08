package com.craftinginterpreters.lox;

import java.util.List;

import static com.craftinginterpreters.lox.TokenType.*;

/*
 * expression     → equality ;
 * equality       → comparison ( ( "!=" | "==" ) comparison )* ;
 * comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
 * term           → factor ( ( "-" | "+" ) factor )* ;
 * factor         → unary ( ( "/" | "*" ) unary )* ;
 * unary          → ( "!" | "-" ) unary
 *                | primary ;
 * primary        → NUMBER | STRING | "true" | "false" | "nil"
 *                | "(" expression ")" ;
*/

class Parser {
    private static class ParseError extends RuntimeException {
    }

    private final List<Token> tokens;
    private int current = 0;

    Parser(List<Token> tokens) {
        this.tokens = tokens;
    }

    Expr parse() {
        try {
            return expression();
        } catch (ParseError error) {
            return null;
        }
    }

    // Recursive descent, so the Parser operates from the "top" down, navigating
    // through the grammar from lowest precedence (expression) to highest
    // precedence (primary)
    // expression → equality ;
    private Expr expression() {
        return equality();
    }

    // equality → comparison ( ( "!=" | "==" ) comparison )* ;
    private Expr equality() {
        // comparison (1)
        Expr expr = comparison();

        // * - while loop for recursion through multiple equality exprs
        // ( ( "!=" | "==" ) comparison )*
        while (match(BANG_EQUAL, EQUAL_EQUAL)) {
            // ( "!=" | "==" )
            Token operator = previous();
            // comparison (2), recursing as many times as we continue to find
            // BANG_EQUAL (!=) or EQUAL_EQUAL (==)
            Expr right = comparison();
            expr = new Expr.Binary(expr, operator, right);
        }

        // Return completed expression
        return expr;
    }

    // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    private Expr comparison() {
        // term (1)
        Expr expr = term();

        // ( ( ">" | ">=" | "<" | "<=" ) term )*
        while (match(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
            // ( ">" | ">=" | "<" | "<=" )
            Token operator = previous();
            // term (2), recursing as many times as we continue to find
            // comparison operators
            Expr right = term();
            expr = new Expr.Binary(expr, operator, right);
        }

        // Return completed expression
        return expr;
    }

    // term → factor ( ( "-" | "+" ) factor )* ;
    private Expr term() {
        // factor (1)
        Expr expr = factor();

        // ( ( "-" | "+" ) factor )*
        while (match(MINUS, PLUS)) {
            // ( "-" | "+" )
            Token operator = previous();
            // factor (2), recursing as many times as we continue to find factor
            // operators
            Expr right = factor();
            expr = new Expr.Binary(expr, operator, right);
        }

        // Return completed expression
        return expr;
    }

    // factor → unary ( ( "/" | "*" ) unary )* ;
    private Expr factor() {
        // unary (1)
        Expr expr = unary();

        // ( ( "/" | "*" ) unary )*
        while (match(SLASH, STAR)) {
            // ( "/" | "*" )
            Token operator = previous();
            // unary (2), recursing as many times as we continue to find unary
            // operators
            Expr right = unary();
            expr = new Expr.Binary(expr, operator, right);
        }

        return expr;
    }

    // unary → ( "!" | "-" ) unary
    // | primary ;
    private Expr unary() {
        // ( "!" | "-" ) unary
        if (match(BANG, MINUS)) {
            // ( "!" | "-" )
            Token operator = previous();
            // unary (1), recursing as many times as we continue to find ! or -
            // e.g. !!true
            Expr right = unary();
            return new Expr.Unary(operator, right);
        }

        // Nothing of lower precedence matched, so this is a primary expression
        return primary();
    }

    // primary → NUMBER | STRING | "true" | "false" | "nil"
    // | "(" expression ")" ;
    private Expr primary() {
        // "false"
        if (match(FALSE))
            return new Expr.Literal(false);
        // "true"
        if (match(TRUE))
            return new Expr.Literal(true);
        // "nil"
        if (match(NIL))
            return new Expr.Literal(null);

        // NUMBER | STRING
        // e.g. 5 or "five"
        if (match(NUMBER, STRING)) {
            return new Expr.Literal(previous().literal);
        }

        // "("
        if (match(LEFT_PAREN)) {
            // expression (1)
            Expr expr = expression();
            // ")", if not found this is a syntax error
            consume(RIGHT_PAREN, "Expect ')' after expression.");
            // (expression)
            return new Expr.Grouping(expr);
        }

        throw error(peek(), "Expect expression.");
    }

    private boolean match(TokenType... types) {
        for (TokenType type : types) {
            if (check(type)) {
                advance();
                return true;
            }
        }

        return false;
    }

    private Token consume(TokenType type, String message) {
        if (check(type))
            return advance();

        throw error(peek(), message);
    }

    private boolean check(TokenType type) {
        if (isAtEnd())
            return false;

        return peek().type == type;
    }

    private Token advance() {
        if (!isAtEnd())
            current++;

        return previous();
    }

    private boolean isAtEnd() {
        return peek().type == EOF;
    }

    private Token peek() {
        return tokens.get(current);
    }

    private Token previous() {
        return tokens.get(current - 1);
    }

    private ParseError error(Token token, String message) {
        Lox.error(token, message);
        return new ParseError();
    }

    private void synchronize() {
        advance();

        while (!isAtEnd()) {
            if (previous().type == SEMICOLON)
                return;

            switch (peek().type) {
                case CLASS:
                case FUN:
                case VAR:
                case FOR:
                case IF:
                case WHILE:
                case PRINT:
                case RETURN:
                    return;
                // advance() statement moved into default case rather than being
                // below to satisfy the Java compiler about enum coverage
                default:
                    advance();
            }
        }
    }

}
