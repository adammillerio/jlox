package com.craftinginterpreters.lox;

import java.util.HashMap;
import java.util.Map;

class Environment {
    final Environment enclosing;
    private final Map<String, Object> values = new HashMap<>();

    Environment() {
        enclosing = null;
    }

    Environment(Environment enclosing) {
        // Store a reference to the enclosing (parent) scope, forming a
        // parent-pointer tree or a "cactus stack", where there is a tree
        // with only one path at any given time from local to global
        this.enclosing = enclosing;
    }

    Object get(Token name) {
        // Check the local scope for this variable
        if (values.containsKey(name.lexeme)) {
            return values.get(name.lexeme);
        }

        // Walk up the scope chain to see if this variable is defined in
        // a higher level scope
        if (enclosing != null)
            return enclosing.get(name);

        // Variable is not defined in local or global scope
        throw new RuntimeError(name, "Undefined variable '" + name.lexeme + "'.");
    }

    void assign(Token name, Object value) {
        // If this variable is defined locally, assign it
        // If the same name exists locally and in enclosing scopes, the
        // local one will "shadow" any others, since this lookup always
        // happens first
        if (values.containsKey(name.lexeme)) {
            values.put(name.lexeme, value);
            return;
        }

        // Walk up the scope chain to see if this variable is available
        // for assignment in a higher level scope
        if (enclosing != null) {
            enclosing.assign(name, value);
            return;
        }

        // Variable is not defined in local or global scope
        throw new RuntimeError(name, "Undefined variable '" + name.lexeme + "'.");
    }

    void define(String name, Object value) {
        values.put(name, value);
    }
}
