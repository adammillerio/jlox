package com.craftinginterpreters.lox;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.craftinginterpreters.lox.TokenType.*;

public class Scanner {
    // Map of reserved words to their TokenTypes, used when identifying
    // reserved words when parsing identifiers.
    private static final Map<String, TokenType> keywords;
    static {
        keywords = new HashMap<>();
        keywords.put("and", AND);
        keywords.put("class", CLASS);
        keywords.put("else", ELSE);
        keywords.put("false", FALSE);
        keywords.put("for", FOR);
        keywords.put("fun", FUN);
        keywords.put("if", IF);
        keywords.put("nil", NIL);
        keywords.put("or", OR);
        keywords.put("print", PRINT);
        keywords.put("return", RETURN);
        keywords.put("super", SUPER);
        keywords.put("this", THIS);
        keywords.put("true", TRUE);
        keywords.put("var", VAR);
        keywords.put("while", WHILE);
    }

    private final String source;
    private final List<Token> tokens = new ArrayList<>();
    private int start = 0;
    private int current = 0;
    private int line = 1;

    Scanner(String source) {
        this.source = source;
    }

    List<Token> scanTokens() {
        while (!isAtEnd()) {
            // We are at the beginning of the next lexeme, set the start
            // and current character pointers to be equal.
            start = current;
            scanToken();
        }

        // Add an End-Of-File (EOF) token.
        tokens.add(new Token(EOF, "", null, line));
        return tokens;
    }

    private boolean isAtEnd() {
        // Return whether or not the current character index is at or above
        // the length of the source being scanned.
        return current >= source.length();
    }

    private void scanToken() {
        // Retrieve the next character in the source and advance the
        // current counter.
        char c = advance();
        switch (c) {
            // Single character Lexemes.
            case '(':
                addToken(LEFT_PAREN);
                break;
            case ')':
                addToken(RIGHT_PAREN);
                break;
            case '{':
                addToken(LEFT_BRACE);
                break;
            case '}':
                addToken(RIGHT_BRACE);
                break;
            case ',':
                addToken(COMMA);
                break;
            case '.':
                addToken(DOT);
                break;
            case '-':
                addToken(MINUS);
                break;
            case '+':
                addToken(PLUS);
                break;
            case ';':
                addToken(SEMICOLON);
                break;
            case '*':
                addToken(STAR);
                break;
            // Two character Lexemes.
            case '!':
                // != or !
                addToken(match('=') ? BANG_EQUAL : BANG);
                break;
            case '=':
                // == or =
                addToken(match('=') ? EQUAL_EQUAL : EQUAL);
                break;
            case '<':
                // <= or <
                addToken(match('=') ? LESS_EQUAL : LESS);
                break;
            case '>':
                // >= or >
                addToken(match('=') ? GREATER_EQUAL : GREATER);
                break;
            // Division or Comment.
            case '/':
                // Second slash matched (//), line comment.
                if (match('/')) {
                    // A comment goes until the end of the line, so advance the
                    // index until we find it. Note that there is no call to
                    // addToken, so all of these characters will not be added
                    // as a Token since it is just comments.
                    while (peek() != '\n' && !isAtEnd())
                        advance();
                } else {
                    // No second slash (/), division operator.
                    addToken(SLASH);
                }
                break;
            // Ignore whitespace.
            case ' ':
            case '\r':
            case '\t':
                break;
            // Newline, increment line counter and continue to scan on the next
            // line.
            case '\n':
                line++;
                break;
            // Strings.
            case '"':
                string();
                break;
            // Unrecognized single character, error but keep scanning, just in
            // case there are other errors we have yet to detect.
            default:
                // Digit, begin processing number literal.
                if (isDigit(c)) {
                    number();
                    // Alphanumeric [a-Z_], begin parsing either an identifier of either
                    // a variable name (orchid) or a reserved word (or).
                } else if (isAlpha(c)) {
                    identifier();
                } else {
                    Lox.error(line, "Unexpected character.");
                }
                break;
        }
    }

    private void identifier() {
        // Advance current index so long as we see [a-Z_]
        while (isAlphaNumeric(peek()))
            advance();

        // Retrieve the scanned identifier.
        String text = source.substring(start, current);

        // Check if identifier is a reserved word (keyword).
        TokenType type = keywords.get(text);
        if (type == null)
            // Not reserved, string is just a user-defined identifier.
            type = IDENTIFIER;

        // Add scanned token.
        addToken(type);
    }

    private void number() {
        // Advance current index while continuing to scan for digits.
        while (isDigit(peek()))
            advance();

        // Look for a fractional part and a digit after it (ie .5).
        if (peek() == '.' && isDigit(peekNext())) {
            // Consume the "."
            advance();

            // Continue advancing current index to parse the fractional part.
            while (isDigit(peek()))
                advance();
        }

        // Parse the scanned number literal and add Token.
        // (1 is a line number)
        // 1 123.456
        // Token(TokenType.NUMBER, "123.456", 123.456, 1)
        addToken(NUMBER, Double.parseDouble(source.substring(start, current)));
    }

    private void string() {
        // Continue scanning until we find the closing double quote.
        while (peek() != '"' && !isAtEnd()) {
            // Newline, increment line counter and continue scanning.
            if (peek() == '\n')
                line++;

            advance();
        }

        // Scanned to the end of source without finding closing double quote.
        if (isAtEnd()) {
            Lox.error(line, "Unterminated string.");
            return;
        }

        // Advance to capture the closing " that was matched in peek().
        advance();

        // Trim the surrounding quotes and add the scanned string Token.
        // 1 "foobar"
        // Token(TokenType.STRING, "\"foobar\"", "foobar", 1)
        String value = source.substring(start + 1, current - 1);
        addToken(STRING, value);
    }

    private char advance() {
        // Increment the current counter and return the char at this
        // position in the source.
        return source.charAt(current++);
    }

    private boolean match(char expected) {
        if (isAtEnd())
            // End of file, so there is no character to match.
            return false;
        if (source.charAt(current) != expected)
            // Current (next) character does not match expected one.
            return false;

        // Current (next) character matches, increment current character index
        // so character is scanned into matching Token.
        current++;
        return true;
    }

    private char peek() {
        // Retrieve the next char, similar to advance, without consuming it.
        if (isAtEnd())
            return '\0';

        return source.charAt(current);
    }

    private char peekNext() {
        // Retrieve the character two ahead, similar to advance, without consuming.
        // This is as far ahead as this scanner will peek.
        if (current + 1 >= source.length())
            return '\0';

        return source.charAt(current + 1);
    }

    private boolean isAlpha(char c) {
        // Character is alphanumeric or underscore [a-Z_].
        return (c >= 'a' && c <= 'z') ||
                (c >= 'A' && c <= 'Z') ||
                c == '_';
    }

    private boolean isAlphaNumeric(char c) {
        return isAlpha(c) || isDigit(c);
    }

    private boolean isDigit(char c) {
        // Character is 0-9.
        return c >= '0' && c <= '9';
    }

    private void addToken(TokenType type) {
        // Add a token with no literal.
        addToken(type, null);
    }

    private void addToken(TokenType type, Object literal) {
        // Retrieve the Lexeme from the source using the start and current
        // values to substring the source.
        String text = source.substring(start, current);

        // Add the scanned Lexeme as a Token.
        tokens.add(new Token(type, text, literal, line));
    }
}
