package com.sparrowwallet.drongo.psbt;

public class PSBTParseException extends Exception {
    public PSBTParseException() {
        super();
    }

    public PSBTParseException(String message) {
        super(message);
    }

    public PSBTParseException(Throwable cause) {
        super(cause);
    }

    public PSBTParseException(String message, Throwable cause) {
        super(message, cause);
    }
}
