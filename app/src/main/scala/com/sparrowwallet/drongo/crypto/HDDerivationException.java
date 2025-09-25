package com.sparrowwallet.drongo.crypto;

public class HDDerivationException extends RuntimeException {
    public HDDerivationException() {
        super();
    }

    public HDDerivationException(String message) {
        super(message);
    }

    public HDDerivationException(Throwable cause) {
        super(cause);
    }

    public HDDerivationException(String message, Throwable cause) {
        super(message, cause);
    }
}
