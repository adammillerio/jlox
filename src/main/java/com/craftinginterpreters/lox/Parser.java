package com.craftinginterpreters.lox;

import java.util.ArrayList;
import java.util.Arrays;
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
        // program → declaration* EOF ;
        List<Stmt> statements = new ArrayList<>();

        while (!isAtEnd()) {
            statements.add(declaration());
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
     * expression → assignment ;
     */
    private Expr expression() {
        return assignment();
    }

    // declaration → classDecl | funDecl | varDecl | statement ;
    private Stmt declaration() {
        try {
            if (match(CLASS))
                return classDeclaration();

            // funDecl → "fun" function ;
            if (match(FUN))
                return function("function");

            if (match(VAR))
                return varDeclaration();

            return statement();
        } catch (ParseError error) {
            synchronize();
            return null;
        }
    }

    // classDecl → "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}" ;
    private Stmt classDeclaration() {
        // class Foo < Bar { add(a, b, c) { print a + b + c; } }
        // name = "Foo"
        Token name = consume(IDENTIFIER, "Expect class name.");

        // Get superclass if any
        // superclass = Bar
        Expr.Variable superclass = null;
        if (match(LESS)) {
            consume(IDENTIFIER, "Expect superclass name.");
            superclass = new Expr.Variable(previous());
        }

        consume(LEFT_BRACE, "Expect '{' before class body.");

        // Add all methods using function parser
        List<Stmt.Function> methods = new ArrayList<>();
        while (!check(RIGHT_BRACE) && !isAtEnd()) {
            methods.add(function("method"));
        }

        consume(RIGHT_BRACE, "Expect '}' after class body.");

        return new Stmt.Class(name, superclass, methods);
    }

    // statement → exprStatement | forStmt | ifStmt | printStmt
    // | whileStmt | block ;
    private Stmt statement() {
        if (match(FOR))
            return forStatement();
        if (match(IF))
            return ifStatement();
        if (match(PRINT))
            return printStatement();
        if (match(RETURN))
            return returnStatement();
        if (match(WHILE))
            return whileStatement();
        if (match(LEFT_BRACE))
            return new Stmt.Block(block());

        return expressionStatement();
    }

    // forStmt → "for" "(" ( varDecl | exprStmt | ";" )
    // expression? ";"
    // expression? ")" statement ;
    private Stmt forStatement() {
        // for (var i = 0; i < 10; i = i + 1) print i;
        consume(LEFT_PAREN, "Expect '(' after 'for'.");

        // Parse the initializer
        Stmt initializer;
        if (match(SEMICOLON)) {
            // ; (no initializer)
            initializer = null;
        } else if (match(VAR)) {
            // var i = 0;
            initializer = varDeclaration();
        } else {
            // i = 0;
            initializer = expressionStatement();
        }

        // Parse the condition
        // No condition by default if there is just a semicolon
        Expr condition = null;
        if (!check(SEMICOLON)) {
            // i < 10
            condition = expression();
        }
        consume(SEMICOLON, "Expect ';' after loop condition.");

        // Parse the increment
        // No increment by default if there is just the closing right paren
        Expr increment = null;
        if (!check(RIGHT_PAREN)) {
            // i = i + 1
            increment = expression();
        }
        consume(RIGHT_PAREN, "Expect ')' after for clauses.");

        // Parse the statement body
        // print i
        Stmt body = statement();

        // Desugar the for loop, breaking it down into existing statements
        // and expressions, rather than defining a new node in the AST
        // for (var i = 0; i < 10; i = i + 1) print i;
        // becomes
        // { var i = 0; while (i < 10) { print i; i = i + 1;} }
        // Desugar the increment
        if (increment != null) {
            // Enclose the body statement with a block which contains it
            // along with an expression which evaluates the increment
            body = new Stmt.Block(
                    Arrays.asList(
                            body,
                            new Stmt.Expression(increment)));
        }

        // Desugar the condition
        // Default to infinite loop if no condition (while true)
        if (condition == null)
            condition = new Expr.Literal(true);
        // Enclose the body statement in a While statement which evaluates
        // the condition each iteration
        body = new Stmt.While(condition, body);

        // Desugar the initializer
        if (initializer != null) {
            // Enclose the body in a block which evaluates the initializer
            // before running it
            body = new Stmt.Block(Arrays.asList(initializer, body));
        }

        return body;
    }

    // ifStmt → "if" "(" expression ")" statement
    // ( "else" statement )? ;
    private Stmt ifStatement() {
        consume(LEFT_PAREN, "Expect '(' after 'if'.");
        Expr condition = expression();
        consume(RIGHT_PAREN, "Expect ')' after if condition.");

        Stmt thenBranch = statement();
        Stmt elseBranch = null;
        if (match(ELSE)) {
            elseBranch = statement();
        }

        return new Stmt.If(condition, thenBranch, elseBranch);
    }

    // varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
    private Stmt varDeclaration() {
        Token name = consume(IDENTIFIER, "Expect variable name.");

        Expr initializer = null;
        if (match(EQUAL)) {
            initializer = expression();
        }

        consume(SEMICOLON, "Expect ';' after variable declaration.");

        return new Stmt.Var(name, initializer);
    }

    private Stmt whileStatement() {
        consume(LEFT_PAREN, "Expect '(' after 'while'.");
        Expr condition = expression();
        consume(RIGHT_PAREN, "Expect ')' after condition.");

        Stmt body = statement();

        return new Stmt.While(condition, body);
    }

    private Stmt printStatement() {
        Expr value = expression();

        consume(SEMICOLON, "Expect ';' after value.");

        return new Stmt.Print(value);
    }

    private Stmt returnStatement() {
        Token keyword = previous();

        // return;
        Expr value = null;
        if (!check(SEMICOLON)) {
            // If the next token is not the semicolon, parse the expression being
            // returned ie:
            // return 1;
            value = expression();
        }

        consume(SEMICOLON, "Expect ';' after return value.");
        return new Stmt.Return(keyword, value);
    }

    private Stmt expressionStatement() {
        Expr expr = expression();

        consume(SEMICOLON, "Expect ';' after expression.");

        return new Stmt.Expression(expr);
    }

    // function → IDENTIFIER "(" parameters? ")" block ;
    private Stmt.Function function(String kind) {
        // fun add(a, b, c) { print a + b + c; }
        // Parse the name of the function
        // name = add
        Token name = consume(IDENTIFIER, "Expect " + kind + " name.");

        consume(LEFT_PAREN, "Expect '(' after " + kind + " name.");

        // Parse all function parameters, up to 255
        // parameters = ["a", "b", "c"]
        // This includes the zero-parameter case
        List<Token> parameters = new ArrayList<>();
        if (!check(RIGHT_PAREN)) {
            do {
                if (parameters.size() >= 255) {
                    error(peek(), "Can't have more than 255 parameters.");
                }

                parameters.add(consume(IDENTIFIER, "Expect parameter name."));
            } while (match(COMMA));
        }

        consume(RIGHT_PAREN, "Expect ')' after parameters.");

        // Parse the function body block
        // body = [<print statement>]
        consume(LEFT_BRACE, "Expect '{' before " + kind + " body.");
        List<Stmt> body = block();

        return new Stmt.Function(name, parameters, body);
    }

    private List<Stmt> block() {
        List<Stmt> statements = new ArrayList<>();

        while (!check(RIGHT_BRACE) && !isAtEnd()) {
            statements.add(declaration());
        }

        consume(RIGHT_BRACE, "Expect '}' after block.");
        return statements;
    }

    // assignment → ( call "." )? IDENTIFIER "=" assignment
    // | logic_or ;
    private Expr assignment() {
        // IDENTIFIER (left hand side of the expr if an assignment)
        Expr expr = or();

        if (match(EQUAL)) {
            // "="
            Token equals = previous();
            // Recursively call assignment, which will return the value
            // of the right hand
            Expr value = assignment();

            if (expr instanceof Expr.Variable) {
                // Left hand is a variable
                // var foo = "bar"
                // Ensure that the left hand side is a valid assignment
                // target, then assign
                Token name = ((Expr.Variable) expr).name;
                return new Expr.Assign(name, value);
            } else if (expr instanceof Expr.Get) {
                // Left hand is one to many get expressions for property
                // access
                // var foo = Foo()
                // foo.bar.baz = "bing"
                // Transform this get expression into a set expression
                Expr.Get get = (Expr.Get) expr;
                return new Expr.Set(get.object, get.name, value);
            }

            error(equals, "Invalid assignment target.");
        }

        return expr;
    }

    // logic_or → logic_and ( "or" logic_and )* ;
    private Expr or() {
        Expr expr = and();

        while (match(OR)) {
            Token operator = previous();
            Expr right = and();
            expr = new Expr.Logical(expr, operator, right);
        }

        return expr;
    }

    // logic_and → equality ( "and" equality )* ;
    private Expr and() {
        Expr expr = equality();

        while (match(AND)) {
            Token operator = previous();
            Expr right = equality();
            expr = new Expr.Logical(expr, operator, right);
        }

        return expr;
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
    // | call ;
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

        // Nothing of lower precedence matched, check if this is a function call
        return call();
    }

    // arguments → expression ( "," expression )* ;
    // Technically the zero argument case in the call grammar is also here
    private Expr finishCall(Expr callee) {
        // Continue parsing comma separated call arguments until the right paren
        // matching the left which generated this call is found
        List<Expr> arguments = new ArrayList<>();
        if (!check(RIGHT_PAREN)) {
            do {
                if (arguments.size() >= 255) {
                    error(peek(), "Can't have more than 255 arguments.");
                }

                arguments.add(expression());
            } while (match(COMMA));
        }

        Token paren = consume(RIGHT_PAREN, "Expect ')' after arguments.");

        // Return call expression with parsed arguments added to the callee
        return new Expr.Call(callee, paren, arguments);
    }

    // call → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
    private Expr call() {
        // Parse the "left operand" of the function call (a primary expression)
        Expr expr = primary();

        // Continue consuming calls as long as there are left parens or
        // dots for property access
        while (true) {
            if (match(LEFT_PAREN)) {
                // add(a, b, c)
                expr = finishCall(expr);
            } else if (match(DOT)) {
                // foo.add(a, b, c)
                Token name = consume(IDENTIFIER, "Expect property name after '.'.");
                expr = new Expr.Get(expr, name);
            } else {
                break;
            }
        }

        return expr;
    }

    // primary → NUMBER | STRING | IDENTIFIER | "true" | "false" | "nil" | "this"
    // | "(" expression ")"
    // | "super" "." IDENTIFIER ;
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

        // "super" "." IDENTIFIER ;
        // "super" keyword, which identifies the direct superclass of the class
        // that it is being accessed in, and must be a method access
        // super.add()
        if (match(SUPER)) {
            Token keyword = previous();
            consume(DOT, "Expect '.' after 'super'.");
            // return super; <- Error, can't have bare super
            Token method = consume(IDENTIFIER,
                    "Expect superclass method name.");

            return new Expr.Super(keyword, method);
        }

        // THIS
        // "this" keyword, which identifies the instance currently being
        // accessed during a method call on a class
        if (match(THIS))
            return new Expr.This(previous());

        // IDENTIFIER
        // Any single identifier token which is the name of a variable
        // being accessed
        if (match(IDENTIFIER)) {
            return new Expr.Variable(previous());
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
