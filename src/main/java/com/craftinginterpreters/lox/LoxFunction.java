package com.craftinginterpreters.lox;

import java.util.List;

class LoxFunction implements LoxCallable {
    private final Stmt.Function declaration;
    // "Stored" environment, to be used for closures, ie functions defined in
    // scope of another function, to enclose/capture any variables referenced
    // in the function which may be in the outer scope
    // fun makeCounter() { var i = 0; fun count() { i = i + 1; print i; } return
    // count; }
    // count is a closure which encloses i by storing it in this Environment
    private final Environment closure;

    // Flag indicating that this callable is a constructor method on a class
    // This is a flag rather than just a check on the name of the method in
    // order to avoid the edge case of defining a function named init()
    private final boolean isInitializer;

    LoxFunction(Stmt.Function declaration, Environment closure, boolean isInitializer) {
        this.isInitializer = isInitializer;
        this.closure = closure;
        this.declaration = declaration;
    }

    // Create a bound method on a class, creating an enclosing scope and
    // assigning a this keyword for instance access
    LoxFunction bind(LoxInstance instance) {
        // Create the enclosing scope for this "bound" instance method on a
        // given class
        Environment environment = new Environment(closure);

        // Register "this" to correspond to the class instance which is
        // invoking the method
        environment.define("this", instance);

        // Return the LoxFunction method "bound" to the current instance
        return new LoxFunction(declaration, environment, isInitializer);
    }

    @Override
    public String toString() {
        return "<fn " + declaration.name.lexeme + ">";
    }

    @Override
    public int arity() {
        return declaration.params.size();
    }

    @Override
    public Object call(Interpreter interpreter, List<Object> arguments) {
        // Create a new environment in the scope chain for this function
        // This is done at call time and not declaration time, since the same
        // function can be called many times during recursion and each one needs
        // it's own scope. This Environment has the declaring scope's Environment
        // as a parent in order to access state from declaration scope at runtime
        Environment environment = new Environment(closure);

        // Bind all arguments to their named parameter values in the environment
        // fun add(a, b, c) { print a + b + c; }
        // add(1, 2, 3)
        // environment = { "a": 1, "b": 2, "c": 3 }
        for (int i = 0; i < declaration.params.size(); i++) {
            environment.define(declaration.params.get(i).lexeme, arguments.get(i));
        }

        // Execute function body using the constructed function scope environment
        // with mapped parameter values
        try {
            interpreter.executeBlock(declaration.body, environment);
        } catch (Return returnValue) {
            // Return statement (exception) encountered in function, return to
            // the value to the calling scope, if any and end early
            // This is done with an exception in order to unwind any portions
            // of the stack inside the calling function (ifs/whiles/etc)

            // Constructor/initializer with an empty return is valid, but
            // should implicitly return "this"
            if (isInitializer)
                return closure.getAt(0, "this");

            // Return the supplied value
            return returnValue.value;
        }

        if (isInitializer)
            // This is a constructor call, always return this
            // This is allowed even in subsequent explicit calls to init in
            // order to make the clox implementation simpler
            // var foo = Foo()
            // foo.init() <- Weird but valid, returns this
            return closure.getAt(0, "this");

        // No return statement in this function, so return nil implicitly
        return null;
    }
}
