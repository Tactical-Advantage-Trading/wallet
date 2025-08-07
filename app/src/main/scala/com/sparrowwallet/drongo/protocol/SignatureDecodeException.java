package com.sparrowwallet.drongo.protocol;

public class SignatureDecodeException extends RuntimeException {
    public SignatureDecodeException() {
        super();
    }

    public SignatureDecodeException(String message) {
        super(message);
    }

    public SignatureDecodeException(Throwable cause) {
        super(cause);
    }

    public SignatureDecodeException(String message, Throwable cause) {
        super(message, cause);
    }
}
