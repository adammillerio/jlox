package com.craftinginterpreters.lox;

class Interpreter implements Expr.Visitor<Object> {
    void interpret(Expr expression) {
        try {
            Object value = evaluate(expression);
            System.out.println(stringify(value));
        } catch (RuntimeError error) {
            Lox.runtimeError(error);
        }
    }

    @Override
    public Object visitLiteralExpr(Expr.Literal expr) {
        return expr.value;
    }

    @Override
    public Object visitUnaryExpr(Expr.Unary expr) {
        Object right = evaluate(expr.right);

        switch (expr.operator.type) {
            case BANG:
                return !isTruthy(right);
            case MINUS:
                checkNumberOperand(expr.operator, right);
                return -(double) right;
            default:
                // Default case added to appease LSP.
                return null;
        }
    }

    private void checkNumberOperand(Token operator, Object operand) {
        if (operand instanceof Double)
            return;

        throw new RuntimeError(operator, "Operand must be a number.");
    }

    private void checkNumberOperands(Token operator, Object left, Object right) {
        // Both operands are checked at once
        if (left instanceof Double && right instanceof Double)
            return;

        throw new RuntimeError(operator, "Operands must be numbers.");
    }

    @Override
    public Object visitGroupingExpr(Expr.Grouping expr) {
        return evaluate(expr.expression);
    }

    private Object evaluate(Expr expr) {
        return expr.accept(this);
    }

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

    private boolean isEqual(Object a, Object b) {
        // nil == nil
        if (a == null && b == null)
            return true;
        // (nil == expression) == false
        if (a == null)
            return false;

        // Use Object.equals method
        // Note that for Double.equals, NaN == NaN, which does not match the
        // IEEE spec, so this is also true in Lox
        return a.equals(b);
    }

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
