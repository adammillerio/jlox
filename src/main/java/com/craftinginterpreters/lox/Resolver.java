package com.craftinginterpreters.lox;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

class Resolver implements Expr.Visitor<Void>, Stmt.Visitor<Void> {
    private final Interpreter interpreter;
    private final Stack<Map<String, Boolean>> scopes = new Stack<>();
    // Type of current function being resolved, if any, used for error checking
    private FunctionType currentFunction = FunctionType.NONE;

    Resolver(Interpreter interpreter) {
        this.interpreter = interpreter;
    }

    private enum FunctionType {
        // Top-level code
        NONE,
        // Function definition
        FUNCTION,
        // Initializer (constructor method on a class)
        INITIALIZER,
        // Method, which is a function defined on a class and bound to it's
        // instances
        METHOD,
    }

    private enum ClassType {
        // Top-level code
        NONE,
        // Class definition
        CLASS,
        // Subclass definition
        SUBCLASS,
    }

    private ClassType currentClass = ClassType.NONE;

    void resolve(List<Stmt> statements) {
        for (Stmt statement : statements) {
            resolve(statement);
        }
    }

    private void resolve(Stmt stmt) {
        stmt.accept(this);
    }

    private void resolve(Expr expr) {
        expr.accept(this);
    }

    private void resolveFunction(Stmt.Function function, FunctionType type) {
        // Store the previous value, since functions can be defined in functions
        FunctionType enclosingFunction = currentFunction;
        // Set the new current function type
        currentFunction = type;

        // Create a new local scope for the function call
        beginScope();

        for (Token param : function.params) {
            // Declare and define all function parameters, so that they are
            // available during function body resolution
            declare(param);
            define(param);
        }

        resolve(function.body);
        endScope();

        // Restore previous function type
        currentFunction = enclosingFunction;
    }

    private void beginScope() {
        scopes.push(new HashMap<String, Boolean>());
    }

    private void endScope() {
        scopes.pop();
    }

    private void declare(Token name) {
        if (scopes.isEmpty())
            return;

        Map<String, Boolean> scope = scopes.peek();

        // Error on variable redefinition outside the global scope
        // var a = "first";
        // var a = "second"; <- Error
        if (scope.containsKey(name.lexeme)) {
            Lox.error(name, "Already a variable with this name in this scope.");
        }

        // Place the var in the scope on tbe top of the stack, with a false to
        // indicate that it is not initialized yet. This prevents it from being
        // referenced prior to initialization.
        // var a = "outer";
        // { var a = a; } <- Error, a is in scope but still false
        scope.put(name.lexeme, false);
    }

    private void define(Token name) {
        if (scopes.isEmpty())
            return;

        // Mark the var as initialized, so that it can be used
        scopes.peek().put(name.lexeme, true);
    }

    private void resolveLocal(Expr expr, Token name) {
        // Traverse backwards through the scope stack
        for (int i = scopes.size() - 1; i >= 0; i--) {
            // This scope has a variable with this name
            if (scopes.get(i).containsKey(name.lexeme)) {
                // Resolve the variable corresponding to this expression, with
                // the number of scopes backward that it is located
                interpreter.resolve(expr, scopes.size() - 1 - i);
                return;
            }
        }
    }

    @Override
    public Void visitBlockStmt(Stmt.Block stmt) {
        beginScope();
        resolve(stmt.statements);
        endScope();

        return null;
    }

    @Override
    public Void visitClassStmt(Stmt.Class stmt) {
        // Store the previous value, since classes can be defined in classes
        ClassType enclosingClass = currentClass;
        // Set the new current class type
        currentClass = ClassType.CLASS;

        declare(stmt.name);
        define(stmt.name);

        // class Foo < Foo {} <- Error
        if (stmt.superclass != null &&
                stmt.name.lexeme.equals(stmt.superclass.name.lexeme)) {
            Lox.error(stmt.superclass.name, "A class can't inherit from itself.");
        }

        // Resolve the superclass variable if there is one
        if (stmt.superclass != null) {
            // Now resolving a subclass
            currentClass = ClassType.SUBCLASS;
            resolve(stmt.superclass);
        }

        // Create a new scope and place the "super" variable in it, so that
        // it will be available in the subclass
        if (stmt.superclass != null) {
            beginScope();
            scopes.peek().put("super", true);
        }

        // Create a new enclosing scope for this method definition
        beginScope();
        // Register "this" as a variable, to be used in class methods
        scopes.peek().put("this", true);

        for (Stmt.Function method : stmt.methods) {
            FunctionType declaration = FunctionType.METHOD;
            resolveFunction(method, declaration);
        }

        // Discard the scope with "this"
        endScope();

        // Discard the scope with "super" if there is a superclass
        if (stmt.superclass != null)
            endScope();

        // Restore previous value
        currentClass = enclosingClass;
        return null;
    }

    @Override
    public Void visitExpressionStmt(Stmt.Expression stmt) {
        // Resolve a single expression statement
        resolve(stmt.expression);
        return null;
    }

    @Override
    public Void visitFunctionStmt(Stmt.Function stmt) {
        // Declare and define the function itself in the enclosing scope, this
        // is done eagerly before resolving the body, in order to allow the
        // function body to refer to itself during recursion
        declare(stmt.name);
        define(stmt.name);

        resolveFunction(stmt, FunctionType.FUNCTION);
        return null;
    }

    @Override
    public Void visitIfStmt(Stmt.If stmt) {
        // Resolve if condition and then branch, there is no control flow here,
        // so all paths are traversed during resolution
        resolve(stmt.condition);
        resolve(stmt.thenBranch);

        if (stmt.elseBranch != null)
            // Resolve else branch if it exists
            resolve(stmt.elseBranch);

        return null;
    }

    @Override
    public Void visitPrintStmt(Stmt.Print stmt) {
        // Resolve the expression which is being printed
        resolve(stmt.expression);
        return null;
    }

    @Override
    public Void visitReturnStmt(Stmt.Return stmt) {
        // Not in any function, can't return
        if (currentFunction == FunctionType.NONE) {
            Lox.error(stmt.keyword, "Can't return from top-level code.");
        }

        // Resolve expression being returned, if present
        if (stmt.value != null) {
            // Constructor/initializer methods always return this implictly,
            // so any return with a value in one is an error
            if (currentFunction == FunctionType.INITIALIZER) {
                Lox.error(stmt.keyword,
                        "Can't return a value from an initializer.");
            }

            resolve(stmt.value);
        }

        return null;
    }

    @Override
    public Void visitVarStmt(Stmt.Var stmt) {
        declare(stmt.name);

        if (stmt.initializer != null) {
            resolve(stmt.initializer);
        }

        define(stmt.name);
        return null;
    }

    @Override
    public Void visitWhileStmt(Stmt.While stmt) {
        // Resolve the condition and body, no control flow, so the loop is always
        // evaluated once
        resolve(stmt.condition);
        resolve(stmt.body);
        return null;
    }

    @Override
    public Void visitAssignExpr(Expr.Assign expr) {
        // Resolve the expression to be assigned for references to other vars
        // ie var a = i + 1
        resolve(expr.value);
        // Resolve the variable that is being assigned to
        resolveLocal(expr, expr.name);
        return null;
    }

    @Override
    public Void visitBinaryExpr(Expr.Binary expr) {
        // Resolve both operands of the binary expression
        resolve(expr.left);
        resolve(expr.right);
        return null;
    }

    @Override
    public Void visitCallExpr(Expr.Call expr) {
        // Resolve the callee itself
        resolve(expr.callee);

        for (Expr argument : expr.arguments) {
            // Resolve all arguments to the call
            resolve(argument);
        }

        return null;
    }

    @Override
    public Void visitGetExpr(Expr.Get expr) {
        // Resolve the left side of a property access/get expression
        // The right side (property dispatch) happens at runtime in the
        // interpreter
        resolve(expr.object);
        return null;
    }

    @Override
    public Void visitGroupingExpr(Expr.Grouping expr) {
        // Resolve expression inside the parenthesis
        resolve(expr.expression);
        return null;
    }

    @Override
    public Void visitLiteralExpr(Expr.Literal expr) {
        // This is a literal, no resolution necessary
        return null;
    }

    @Override
    public Void visitLogicalExpr(Expr.Logical expr) {
        // Resolve both sides of the logical expression, with no short circuit
        resolve(expr.left);
        resolve(expr.right);
        return null;
    }

    @Override
    public Void visitSetExpr(Expr.Set expr) {
        resolve(expr.value);
        resolve(expr.object);
        return null;
    }

    @Override
    public Void visitSuperExpr(Expr.Super expr) {
        if (currentClass == ClassType.NONE) {
            // super.NotInClass() <- Error
            Lox.error(expr.keyword,
                    "Can't use 'super' outside of a class.");
        } else if (currentClass != ClassType.SUBCLASS) {
            // class Foo { method() { print super.method() } } <- Error
            Lox.error(expr.keyword,
                    "Can't use 'super' in a class with no superclass.");
        }

        resolveLocal(expr, expr.keyword);
        return null;
    }

    @Override
    public Void visitThisExpr(Expr.This expr) {
        if (currentClass == ClassType.NONE) {
            // this keyword used outside of class scope, error
            Lox.error(expr.keyword,
                    "Can't use 'this' outside of a class.");
            return null;
        }

        resolveLocal(expr, expr.keyword);
        return null;
    }

    @Override
    public Void visitUnaryExpr(Expr.Unary expr) {
        // Resolve the unary operand
        resolve(expr.right);
        return null;
    }

    @Override
    public Void visitVariableExpr(Expr.Variable expr) {
        // Variable is declared but not initialized yet
        if (!scopes.isEmpty() && scopes.peek().get(expr.name.lexeme) == Boolean.FALSE) {
            Lox.error(expr.name, "Can't read local variable in its own initializer.");
        }

        resolveLocal(expr, expr.name);
        return null;
    }
}
