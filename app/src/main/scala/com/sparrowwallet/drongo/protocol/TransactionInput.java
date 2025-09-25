package com.sparrowwallet.drongo.protocol;

import com.sparrowwallet.drongo.Utils;

import java.io.IOException;
import java.io.OutputStream;

public class TransactionInput extends ChildMessage {
    public static final long SEQUENCE_LOCKTIME_DISABLED = 4294967295L;
    public static final long SEQUENCE_RBF_ENABLED = 4294967293L;
    public static final long MAX_RELATIVE_TIMELOCK = 2147483647L;
    public static final long RELATIVE_TIMELOCK_VALUE_MASK = 0xFFFF;
    public static final long RELATIVE_TIMELOCK_TYPE_FLAG = 0x400000;
    public static final int RELATIVE_TIMELOCK_SECONDS_INCREMENT = 512;

    // Allows for altering transactions after they were broadcast. Values below NO_SEQUENCE-1 mean it can be altered.
    private long sequence;

    // Data needed to connect to the output of the transaction we're gathering coins from.
    private TransactionOutPoint outpoint;

    private byte[] scriptBytes;

    private Script scriptSig;

    private TransactionWitness witness;

    public TransactionInput(Transaction transaction, TransactionOutPoint outpoint, byte[] scriptBytes) {
        this(transaction, outpoint, scriptBytes, null);
    }

    public TransactionInput(Transaction transaction, TransactionOutPoint outpoint, byte[] scriptBytes, TransactionWitness witness) {
        setParent(transaction);
        this.sequence = SEQUENCE_LOCKTIME_DISABLED;
        this.outpoint = outpoint;
        this.outpoint.setParent(this);
        this.scriptBytes = scriptBytes;
        this.witness = witness;
        length = 40 + (scriptBytes == null ? 1 : VarInt.sizeOf(scriptBytes.length) + scriptBytes.length);
        if(witness != null) {
            transaction.adjustLength(witness.getLength());
        }
    }

    public TransactionInput(Transaction transaction, byte[] rawtx, int offset) {
        super(rawtx, offset);
        setParent(transaction);
    }

    protected void parse() throws ProtocolException {
        outpoint = new TransactionOutPoint(payload, cursor, this);
        cursor += outpoint.getMessageSize();
        int scriptLen = (int) readVarInt();
        length = cursor - offset + scriptLen + 4;
        scriptBytes = readBytes(scriptLen);
        sequence = readUint32();
    }

    public byte[] getScriptBytes() {
        return scriptBytes;
    }

    public Script getScriptSig() {
        if(scriptSig == null) {
            if(isCoinBase()) {
                //ScriptSig may be invalid, attempt to parse
                scriptSig = new Script(scriptBytes, false);
                try {
                    scriptSig.parse();
                } catch (ProtocolException e) {
                    scriptSig = new Script(scriptSig.getChunks());
                }
            } else {
                scriptSig = new Script(scriptBytes);
            }
        }

        return scriptSig;
    }

    public void setScriptBytes(byte[] scriptBytes) {
        super.payload = null;
        this.scriptSig = null;
        int oldLength = length;
        this.scriptBytes = scriptBytes;
        // 40 = previous_outpoint (36) + sequence (4)
        int newLength = 40 + (scriptBytes == null ? 1 : VarInt.sizeOf(scriptBytes.length) + scriptBytes.length);
        adjustLength(newLength - oldLength);
    }

    public void clearScriptBytes() {
        setScriptBytes(new byte[0]);
    }

    public TransactionWitness getWitness() {
        return witness;
    }

    void witness(TransactionWitness witness) {
        this.witness = witness;
    }

    public void setWitness(TransactionWitness witness) {
        int newLength = witness != null ? witness.getLength() : 0;
        int existingLength = getWitness() != null ? getWitness().getLength() : 0;
        if(getParent() != null) {
            getParent().adjustLength(newLength - existingLength);
        }

        this.witness = witness;
    }

    public boolean hasWitness() {
        return witness != null;
    }

    public TransactionOutPoint getOutpoint() {
        return outpoint;
    }

    public long getSequenceNumber() {
        return sequence;
    }

    public void setSequenceNumber(long sequence) {
        this.sequence = sequence;
    }

    public int getIndex() {
        Transaction transaction = (Transaction)parent;
        return transaction.getInputs().indexOf(this);
    }

    /**
     * Coinbase transactions have special inputs with hashes of zero. If this is such an input, returns true.
     */
    public boolean isCoinBase() {
        return outpoint.getHash().equals(Sha256Hash.ZERO_HASH) &&
                (outpoint.getIndex() & 0xFFFFFFFFL) == 0xFFFFFFFFL;  // -1 but all is serialized to the wire as unsigned int.
    }

    public boolean isReplaceByFeeEnabled() {
        return sequence <= SEQUENCE_RBF_ENABLED;
    }

    public boolean isAbsoluteTimeLockDisabled() {
        return sequence >= SEQUENCE_LOCKTIME_DISABLED;
    }

    public boolean isAbsoluteTimeLocked() {
        return !isAbsoluteTimeLockDisabled() && !isRelativeTimeLocked();
    }

    public boolean isRelativeTimeLocked() {
        return getTransaction().isRelativeLocktimeAllowed() && sequence <= MAX_RELATIVE_TIMELOCK;
    }

    public boolean isRelativeTimeLockedInBlocks() {
        return isRelativeTimeLocked() && ((sequence & RELATIVE_TIMELOCK_TYPE_FLAG) == 0);
    }

    public long getRelativeLocktime() {
        return sequence & RELATIVE_TIMELOCK_VALUE_MASK;
    }

    public Transaction getTransaction() {
        return (Transaction)getParent();
    }

    protected void bitcoinSerializeToStream(OutputStream stream) throws IOException {
        outpoint.bitcoinSerializeToStream(stream);
        stream.write(new VarInt(scriptBytes.length).encode());
        stream.write(scriptBytes);
        Utils.uint32ToByteStreamLE(sequence, stream);
    }
}
