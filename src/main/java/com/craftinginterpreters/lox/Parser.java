package com.craftinginterpreters.lox;

import java.util.ArrayList;
import java.util.List;

import static com.craftinginterpreters.lox.TokenType.*;

/*
 * Parser class for expressing the Lox grammar rules defined below:
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

    /**
     * Parse all tokens and return the expression.
     *
     * @return the parsed expression
     */
    List<Stmt> parse() {
        List<Stmt> statements = new ArrayList<>();

        while (!isAtEnd()) {
            statements.add(statement());
        }

        return statements;
    }

    /**
     * The "topmost" and lowest precedence rule, directly matching into an
     * equality expression.
     *
     * The parser will traverse from the initial expression "down" as it finds
     * Tokens indicating rules of higher precedence using the "recursive
     * descent" technique.
     *
     * Grammar Rule:
     * expression → equality ;
     */
    private Expr expression() {
        return equality();
    }

    private Stmt statement() {
        if (match(PRINT))
            return printStatement();

        return expressionStatement();
    }

    private Stmt printStatement() {
        Expr value = expression();

        consume(SEMICOLON, "Expect ';' after value.");

        return new Stmt.Print(value);
    }

    private Stmt expressionStatement() {
        Expr expr = expression();

        consume(SEMICOLON, "Expect ';' after expression.");

        return new Stmt.Expression(expr);
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

    /**
     * Given a TokenType(s), this will check the current token against each one,
     * advancing to the next token if matched.
     *
     * @return whether or not the current token matched one of the supplied types.
     */
    private boolean match(TokenType... types) {
        for (TokenType type : types) {
            if (check(type)) {
                advance();
                return true;
            }
        }

        return false;
    }

    /**
     * Consume a specific type of Token, erroring if not found.
     *
     * The parser advances to the next Token, "consuming" the returned Token.
     *
     * This is used for things ie groupings where a "( expression" must have a
     * corresponding ")".
     *
     * @return consumed token if found.
     * @throws ParserError if the current Token does not match the TokenType.
     */
    private Token consume(TokenType type, String message) {
        if (check(type))
            return advance();

        throw error(peek(), message);
    }

    /**
     * Check if the current token is of the provided type.
     *
     * If at the end of the file, this will always return false.
     *
     * @returns whether or not the Token is the provided type.
     */
    private boolean check(TokenType type) {
        if (isAtEnd())
            return false;

        return peek().type == type;
    }

    /**
     * Advance to the next Token and return the now previous Token.
     *
     * @return the now previous Token after advancing to the next, or EOF if
     *         already at the end.
     */
    private Token advance() {
        if (!isAtEnd())
            current++;

        return previous();
    }

    /**
     * Whether or not we are at the End-of-File (EOF) Token.
     *
     * @return whether or not Parser is at EOF and finished parsing.
     */
    private boolean isAtEnd() {
        return peek().type == EOF;
    }

    /**
     * Retrieve the current Token being parsed, without advancing to the next.
     *
     * @return the current Token being parsed
     */
    private Token peek() {
        return tokens.get(current);
    }

    /**
     * Retrieve the previously parsed token, without advancement.
     *
     * @return the previously parsed (n - 1) Token.
     */
    private Token previous() {
        return tokens.get(current - 1);
    }

    /**
     * Report and return an error encountered while parsing.
     *
     * @return error encountered while parsing.
     */
    private ParseError error(Token token, String message) {
        Lox.error(token, message);
        return new ParseError();
    }

    /**
     * Attempt to synchronize the Parser after encountering an error.
     *
     * Synchronization in this case essentially means continuing to advance
     * through Tokens until we have (probably) found the next statement to parse.
     * This is done by looking for either the end of the current statement (;) or
     * the beginning of another statement (class/fn/var/etc). This is not
     * perfect, but allows for continuation of parsing subsequent lines if
     * possible in order to indicate more errors to the user.
     */
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
