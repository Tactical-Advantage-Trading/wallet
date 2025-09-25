package com.sparrowwallet.drongo.psbt;

public class PSBTSignatureException extends PSBTParseException {
    public PSBTSignatureException() {
        super();
    }

    public PSBTSignatureException(String message) {
        super(message);
    }

    public PSBTSignatureException(Throwable cause) {
        super(cause);
    }

    public PSBTSignatureException(String message, Throwable cause) {
        super(message, cause);
    }
}
