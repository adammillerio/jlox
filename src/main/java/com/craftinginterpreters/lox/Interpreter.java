package com.craftinginterpreters.lox;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class Interpreter implements
        Expr.Visitor<Object>,
        // Java doesn’t let you use lowercase “void” as a generic type argument for
        // obscure reasons having to do with type erasure and the stack. Instead,
        // there is a separate “Void” type specifically for this use. Sort of a
        // “boxed void”, like “Integer” is for “int”.
        Stmt.Visitor<Void> {

    // "Root" environment to hold global variables and scope, this is replaced
    // by the local environment when entering a new scope, so locally defined
    // state will shadow global state
    final Environment globals = new Environment();
    private Environment environment = globals;
    private final Map<Expr, Integer> locals = new HashMap<>();

    Interpreter() {
        // Native function clock(), which returns the current system time in
        // seconds as a double.
        globals.define("clock", new LoxCallable() {
            @Override
            public int arity() {
                return 0;
            }

            @Override
            public Object call(Interpreter interpreter, List<Object> arguments) {
                // Convert to seconds
                return (double) System.currentTimeMillis() / 1000.0;
            }

            @Override
            public String toString() {
                return "<native fn>";
            }
        });
    }

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

    @Override
    public Object visitLogicalExpr(Expr.Logical expr) {
        // Evaluate left hand of the logical expression
        Object left = evaluate(expr.left);

        if (expr.operator.type == TokenType.OR) {
            // Short circuit if it is an OR and the left is truthy
            if (isTruthy(left))
                return left;
        } else {
            // Short circuit if it is an AND and the left is truthy
            if (!isTruthy(left))
                return left;
        }

        // Continue to evaluate the right hand of the logical expression
        return evaluate(expr.right);
    }

    @Override
    public Object visitSetExpr(Expr.Set expr) {
        Object object = evaluate(expr.object);

        if (!(object instanceof LoxInstance)) {
            throw new RuntimeError(expr.name,
                    "Only instances have fields.");
        }

        Object value = evaluate(expr.value);
        ((LoxInstance) object).set(expr.name, value);

        return value;
    }

    @Override
    public Object visitSuperExpr(Expr.Super expr) {
        // Look up the "super" variable which is automatically defined in an
        // enclosing scope during the instance method call if a class has a
        // superclass. See LoxClass.findMethod for more info
        int distance = locals.get(expr);

        // Retrieve "super" as the class which is the direct superclass of the
        // class which contains this super keyword (not necessarily the same as
        // the class referred to by the "this" instance)
        LoxClass superclass = (LoxClass) environment.getAt(distance, "super");

        // Retrieve "this" as the class instance which is invoking this method,
        // which is always in the environment immediately below the one
        // containing super
        LoxInstance object = (LoxInstance) environment.getAt(
                distance - 1, "this");

        // Find the method on the retrieved superclass and return a function
        // which is bound to the retrieved "this" class instance
        LoxFunction method = superclass.findMethod(expr.method.lexeme);

        // Superclass doesn't have this method
        if (method == null) {
            throw new RuntimeError(expr.method,
                    "Undefined property '" + expr.method.lexeme + "'.");
        }

        return method.bind(object);
    }

    @Override
    public Object visitThisExpr(Expr.This expr) {
        // Look up the "this" variable which is automatically defined in an
        // enclosing scope during the instance method call, see
        // LoxFunction.bind for more info
        return lookUpVariable(expr.keyword, expr);
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
        return lookUpVariable(expr.name, expr);
    }

    private Object lookUpVariable(Token name, Expr expr) {
        // Get the resolved distance up the scope stack for this variable
        Integer distance = locals.get(expr);

        if (distance != null) {
            // Retrieve the value for the variable in the correct environment
            return environment.getAt(distance, name.lexeme);
        } else {
            // No distance, this is a global variable
            return globals.get(name);
        }
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

    void resolve(Expr expr, int depth) {
        locals.put(expr, depth);
    }

    void executeBlock(List<Stmt> statements, Environment environment) {
        Environment previous = this.environment;

        try {
            this.environment = environment;

            for (Stmt statement : statements) {
                execute(statement);
            }
        } finally {
            this.environment = previous;
        }
    }

    @Override
    public Void visitBlockStmt(Stmt.Block stmt) {
        executeBlock(stmt.statements, new Environment(environment));
        return null;
    }

    @Override
    public Void visitClassStmt(Stmt.Class stmt) {
        // Ensure the superclass is actually a class, if supplied
        Object superclass = null;
        if (stmt.superclass != null) {
            superclass = evaluate(stmt.superclass);
            if (!(superclass instanceof LoxClass)) {
                throw new RuntimeError(stmt.superclass.name,
                        "Superclass must be a class.");
            }
        }

        environment.define(stmt.name.lexeme, null);

        // Create the environment holding "super" and set it to the superclass
        if (stmt.superclass != null) {
            environment = new Environment(environment);
            environment.define("super", superclass);
        }

        // Evaluate all methods on this class
        Map<String, LoxFunction> methods = new HashMap<>();
        for (Stmt.Function method : stmt.methods) {
            // Conditionally set initializer flag since this is a method
            // definition
            LoxFunction function = new LoxFunction(
                    method, environment, method.name.lexeme.equals("init"));
            methods.put(method.name.lexeme, function);
        }

        LoxClass klass = new LoxClass(stmt.name.lexeme,
                (LoxClass) superclass, methods);

        if (superclass != null) {
            // Restore the enclosing environment if there is superclass
            environment = environment.enclosing;
        }

        environment.assign(stmt.name, klass);

        return null;
    }

    @Override
    public Void visitExpressionStmt(Stmt.Expression stmt) {
        evaluate(stmt.expression);

        // Required when using the "boxed void" (Void) type
        return null;
    }

    @Override
    public Void visitFunctionStmt(Stmt.Function stmt) {
        // Store the Environment from function declaration time
        // Set isInitializer to false since this is a function and not a
        // class method declaration
        LoxFunction function = new LoxFunction(stmt, environment, false);

        environment.define(stmt.name.lexeme, function);

        return null;
    }

    @Override
    public Void visitIfStmt(Stmt.If stmt) {
        if (isTruthy(evaluate(stmt.condition))) {
            execute(stmt.thenBranch);
        } else if (stmt.elseBranch != null) {
            execute(stmt.elseBranch);
        }

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
    public Void visitReturnStmt(Stmt.Return stmt) {
        Object value = null;
        if (stmt.value != null)
            // Evaluate return statement's value if it has one
            value = evaluate(stmt.value);

        // Throw an exception with the return value to unwind the stack back to
        // the call site of the function which is returning
        throw new Return(value);
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

    @Override
    public Void visitWhileStmt(Stmt.While stmt) {
        while (isTruthy(evaluate(stmt.condition))) {
            execute(stmt.body);
        }

        return null;
    }

    @Override
    public Object visitAssignExpr(Expr.Assign expr) {
        Object value = evaluate(expr.value);

        Integer distance = locals.get(expr);
        if (distance != null) {
            environment.assignAt(distance, expr.name, value);
        } else {
            globals.assign(expr.name, value);
        }

        return value;
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

    @Override
    public Object visitCallExpr(Expr.Call expr) {
        // Evaluate the expression which yields the callee
        Object callee = evaluate(expr.callee);

        // Evaluate all argument expressions and and add the results
        List<Object> arguments = new ArrayList<>();
        for (Expr argument : expr.arguments) {
            arguments.add(evaluate(argument));
        }

        // Ensure the callee can be called (implements calling interface)
        // "foobar"(); <- RuntimeError, since String does not implement LoxCallable
        if (!(callee instanceof LoxCallable)) {
            throw new RuntimeError(expr.paren, "Can only call functions and classes.");
        }

        // Cast to a LoxCallable and call it, returning the result
        LoxCallable function = (LoxCallable) callee;

        // Check the arity of the function and fail if number of args do not match
        // fun add(a, b, c) { print a + b + c; }
        // arity = 3
        // add(1, 2, 3, 4); <- RuntimeError
        // add(1, 2) <- RuntimeError
        if (arguments.size() != function.arity()) {
            throw new RuntimeError(expr.paren,
                    "Expected " + function.arity() + " arguments but got " + arguments.size() + ".");
        }

        return function.call(this, arguments);
    }

    @Override
    public Object visitGetExpr(Expr.Get expr) {
        Object object = evaluate(expr.object);

        if (object instanceof LoxInstance) {
            return ((LoxInstance) object).get(expr.name);
        }

        throw new RuntimeError(expr.name, "Only instances have properties.");
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
