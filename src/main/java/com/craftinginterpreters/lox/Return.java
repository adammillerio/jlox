package com.craftinginterpreters.lox;

class Return extends RuntimeException {
    final Object value;

    Return(Object value) {
        // Disable suppression and writable stack traces, since this is just
        // being used for control flow and not error handling
        // String message = null
        // Throwable cause = nul
        // enableSuppression = false
        // writableStackTrace = false
        super(null, null, false, false);
        // Set return value
        this.value = value;
    }
}
