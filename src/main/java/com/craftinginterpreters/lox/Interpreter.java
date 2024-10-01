package com.craftinginterpreters.lox;

import java.util.List;

class Interpreter implements
        Expr.Visitor<Object>,
        // Java doesn’t let you use lowercase “void” as a generic type argument for
        // obscure reasons having to do with type erasure and the stack. Instead,
        // there is a separate “Void” type specifically for this use. Sort of a
        // “boxed void”, like “Integer” is for “int”.
        Stmt.Visitor<Void> {
    private Environment environment = new Environment();

    /**
     * Interpret an expression and print the result to stdout.
     *
     * This is the entrypoint to the Lox interpreter.
     *
     * @param expression Scanned and Parsed expression to evaluate
     */
    void interpret(List<Stmt> statements) {
        try {
            for (Stmt statement : statements) {
                execute(statement);
            }
        } catch (RuntimeError error) {
            Lox.runtimeError(error);
        }
    }

    /**
     * Interpret a literal value, returning it directly by value.
     *
     * Because the value was eagerly stored while parsing, this just returns the
     * already parsed value for the literal expression.
     *
     * @param expr the literal expression to evaluate
     * @return the value of the literal expression
     */
    @Override
    public Object visitLiteralExpr(Expr.Literal expr) {
        return expr.value;
    }

    /**
     * Interpret a unary value.
     *
     * @param expr unary expression to interpret
     * @return the value of the unary expression
     */
    @Override
    public Object visitUnaryExpr(Expr.Unary expr) {
        // Evaluate the right side of the expression
        // ie the true in !true, where true is a literal expr
        Object right = evaluate(expr.right);

        switch (expr.operator.type) {
            case BANG:
                // Negate the right value
                // !true == false
                return !isTruthy(right);
            case MINUS:
                // Ensure the evaluated right value is a number, and make negative
                checkNumberOperand(expr.operator, right);
                return -(double) right;
            default:
                // Default case added to appease LSP.
                return null;
        }
    }

    @Override
    public Object visitVariableExpr(Expr.Variable expr) {
        return environment.get(expr.name);
    }

    /**
     * Check whether the operand to a given operator is a number.
     *
     * All numbers in Lox are Doubles, so this is just a check for whether or
     * not the provided operand is of type Double.
     *
     * @param operator Token for operation being performed
     * @param operand  operand value being checked for the operation
     * @throws RuntimeError if the operand is not a number (java Double)
     */
    private void checkNumberOperand(Token operator, Object operand) {
        if (operand instanceof Double)
            return;

        throw new RuntimeError(operator, "Operand must be a number.");
    }

    /**
     * Check whether the operands to a binary expression are both numbers.
     *
     * Both operands evaluated and checked at once, rather than left to right.
     *
     * @param operator Token for binary operation being performed
     * @param left     left value to binary operation
     * @param right    right value to binary operation
     * @throws RuntimeError if either the left or right operand is not a number
     */
    private void checkNumberOperands(Token operator, Object left, Object right) {
        // Both operands are checked at once
        if (left instanceof Double && right instanceof Double)
            return;

        throw new RuntimeError(operator, "Operands must be numbers.");
    }

    /**
     * Interpret a grouping expression.
     *
     * This invoke's the expression's visitor method, interpreting it and returning
     * the value. For example, (2 + 2) would visit the binary expression 2 + 2's
     * visitor, returning the result 4. This will recursively evaluate any other
     * subsequent groupings as needed.
     *
     * @param expr grouped expression to evaluate
     * @returns the result of the expression evaluation
     */
    @Override
    public Object visitGroupingExpr(Expr.Grouping expr) {
        return evaluate(expr.expression);
    }

    /**
     * Evaluate an expression, visiting and returning the value.
     *
     * @param expr expression to evaluate
     * @return the result of expression evaluation
     */
    private Object evaluate(Expr expr) {
        return expr.accept(this);
    }

    private void execute(Stmt stmt) {
        stmt.accept(this);
    }

    @Override
    public Void visitExpressionStmt(Stmt.Expression stmt) {
        evaluate(stmt.expression);

        // Required when using the "boxed void" (Void) type
        return null;
    }

    @Override
    public Void visitPrintStmt(Stmt.Print stmt) {
        Object value = evaluate(stmt.expression);
        System.out.println(stringify(value));

        // Required when using the "boxed void" (Void) type
        return null;
    }

    @Override
    public Void visitVarStmt(Stmt.Var stmt) {
        Object value = null;
        if (stmt.initializer != null) {
            value = evaluate(stmt.initializer);
        }

        environment.define(stmt.name.lexeme, value);
        return null;
    }

    /**
     * Interpret a binary expression.
     *
     * @param expr binary expression to interpret
     * @return the result of the binary expression
     * @throws RuntimeError if the operands to the binary expression are not the
     *                      correct type.
     */
    @Override
    public Object visitBinaryExpr(Expr.Binary expr) {
        Object left = evaluate(expr.left);
        Object right = evaluate(expr.right);

        switch (expr.operator.type) {
            // Comparison operators, these will return a Boolean
            case GREATER:
                checkNumberOperands(expr.operator, left, right);
                return (double) left > (double) right;
            case GREATER_EQUAL:
                checkNumberOperands(expr.operator, left, right);
                return (double) left >= (double) right;
            case LESS:
                checkNumberOperands(expr.operator, left, right);
                return (double) left < (double) right;
            case LESS_EQUAL:
                checkNumberOperands(expr.operator, left, right);
                return (double) left <= (double) right;
            // Equality operators, these will return a Boolean
            case BANG_EQUAL:
                return !isEqual(left, right);
            case EQUAL_EQUAL:
                return isEqual(left, right);
            // Arithmetic operators, these will return a Double or String.
            case MINUS:
                checkNumberOperands(expr.operator, left, right);
                return (double) left - (double) right;
            case PLUS:
                // Arithmetic
                // 2 + 2 = 4
                if (left instanceof Double && right instanceof Double) {
                    return (double) left + (double) right;
                }

                // String concatenation
                // "foo" + "bar" = "foobar"
                if (left instanceof String && right instanceof String) {
                    return (String) left + (String) right;
                }

                // Unrecognized addition operation
                throw new RuntimeError(
                        expr.operator,
                        "Operands must be two numbers or two strings.");
            case SLASH:
                checkNumberOperands(expr.operator, left, right);
                return (double) left / (double) right;
            case STAR:
                checkNumberOperands(expr.operator, left, right);
                return (double) left * (double) right;
            // Unreachable
            default:
                return null;
        }
    }

    /**
     * Return the "truthiness" of a given value.
     *
     * Truthiness in the case of Lox follows the Ruby pattern, where nil (null)
     * is false, and all other values (other than Boolean false) are true.
     *
     * @param object Lox object to evaluate
     * @return truthiness of Lox object
     */
    private boolean isTruthy(Object object) {
        // nil (null) == false
        if (object == null)
            return false;
        // object is a boolean already
        if (object instanceof Boolean)
            return (boolean) object;

        // All other values are truthy
        return true;
    }

    /**
     * Evaluate the equality of two Lox objects.
     *
     * This uses each Java type's .equals method underneath to evaluate the
     * equality of Lox objects.
     *
     * Note that for Double.equals, NaN == NaN, which does not match the IEEE
     * spec. This will also be true in Lox.
     *
     * @param a left operand
     * @param b right operand
     * @return whether or not a and b are equal
     */
    private boolean isEqual(Object a, Object b) {
        // nil == nil
        if (a == null && b == null)
            return true;
        // (nil == expression) == false
        if (a == null)
            return false;

        // Use Object.equals method
        return a.equals(b);
    }

    /**
     * Generate a string representation of a Lox object.
     *
     * @param object Lox object
     * @return Provided object represented as a string
     */
    private String stringify(Object object) {
        // null -> "nil"
        if (object == null)
            return "nil";

        if (object instanceof Double) {
            String text = object.toString();

            // "2.0" -> "2"
            if (text.endsWith(".0")) {
                text = text.substring(0, text.length() - 2);
            }

            return text;
        }

        return object.toString();
    }
}
