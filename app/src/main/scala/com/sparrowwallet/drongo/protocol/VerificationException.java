package com.sparrowwallet.drongo.protocol;

public class VerificationException extends RuntimeException {
    public VerificationException(String msg) {
        super(msg);
    }

    public VerificationException(Exception e) {
        super(e);
    }

    public VerificationException(String msg, Throwable t) {
        super(msg, t);
    }

    public static class EmptyInputsOrOutputs extends VerificationException {
        public EmptyInputsOrOutputs() {
            super("Transaction had no inputs or no outputs.");
        }
    }

    public static class LargerThanMaxBlockSize extends VerificationException {
        public LargerThanMaxBlockSize() {
            super("Transaction larger than MAX_BLOCK_SIZE");
        }
    }

    public static class DuplicatedOutPoint extends VerificationException {
        public DuplicatedOutPoint() {
            super("Duplicated outpoint");
        }
    }

    public static class NegativeValueOutput extends VerificationException {
        public NegativeValueOutput() {
            super("Transaction output negative");
        }
    }

    public static class ExcessiveValue extends VerificationException {
        public ExcessiveValue() {
            super("Total transaction output value greater than possible");
        }
    }

    public static class CoinbaseScriptSizeOutOfRange extends VerificationException {
        public CoinbaseScriptSizeOutOfRange() {
            super("Coinbase script size out of range");
        }
    }

    public static class UnexpectedCoinbaseInput extends VerificationException {
        public UnexpectedCoinbaseInput() {
            super("Coinbase input as input in non-coinbase transaction");
        }
    }

    public static class NoncanonicalSignature extends VerificationException {
        public NoncanonicalSignature() {
            super("Signature encoding is not canonical");
        }
    }
}
