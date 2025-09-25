package com.sparrowwallet.drongo.address;

public class InvalidAddressException extends Exception {
    public InvalidAddressException() {
        super();
    }

    public InvalidAddressException(String msg) {
        super(msg);
    }

    public InvalidAddressException(Throwable cause) {
        super(cause);
    }

    public InvalidAddressException(String message, Throwable cause) {
        super(message, cause);
    }
}
