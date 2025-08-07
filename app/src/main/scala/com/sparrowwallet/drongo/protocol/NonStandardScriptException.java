package com.sparrowwallet.drongo.protocol;

public class NonStandardScriptException extends Exception {
    public NonStandardScriptException() {
        super();
    }

    public NonStandardScriptException(String message) {
        super(message);
    }

    public NonStandardScriptException(Throwable cause) {
        super(cause);
    }

    public NonStandardScriptException(String message, Throwable cause) {
        super(message, cause);
    }
}
