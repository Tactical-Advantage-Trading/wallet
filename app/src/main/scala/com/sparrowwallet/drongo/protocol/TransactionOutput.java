package com.sparrowwallet.drongo.protocol;

import com.sparrowwallet.drongo.Utils;
import com.sparrowwallet.drongo.address.Address;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

public class TransactionOutput extends ChildMessage {
    private long value;
    private byte[] scriptBytes;
    private Script script;

    private Address[] addresses = new Address[0];

    public TransactionOutput(Transaction transaction, long value, Script script) {
        this(transaction, value, script.getProgram());
    }

    public TransactionOutput(Transaction transaction, long value, byte[] scriptBytes) {
        this.value = value;
        this.scriptBytes = scriptBytes;
        setParent(transaction);
        length = 8 + VarInt.sizeOf(scriptBytes.length) + scriptBytes.length;
    }

    public TransactionOutput(Transaction parent, byte[] rawtx, int offset) {
        super(rawtx, offset);
        setParent(parent);
    }

    protected void parse() throws ProtocolException {
        value = readInt64();
        int scriptLen = (int) readVarInt();
        length = cursor - offset + scriptLen;
        scriptBytes = readBytes(scriptLen);
    }

    public byte[] bitcoinSerialize() {
        try {
            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            bitcoinSerializeToStream(outputStream);
            return outputStream.toByteArray();
        } catch (IOException e) {
            //can't happen
        }

        return null;
    }

    protected void bitcoinSerializeToStream(OutputStream stream) throws IOException {
        Utils.int64ToByteStreamLE(value, stream);
        // TODO: Move script serialization into the Script class, where it belongs.
        stream.write(new VarInt(scriptBytes.length).encode());
        stream.write(scriptBytes);
    }

    public byte[] getScriptBytes() {
        return scriptBytes;
    }

    public Script getScript() {
        if(script == null) {
            script = new Script(scriptBytes);
        }

        return script;
    }

    public long getValue() {
        return value;
    }

    public Address[] getAddresses() {
        return addresses;
    }

    public void setAddresses(Address[] addresses) {
        this.addresses = addresses;
    }

    public Sha256Hash getHash() {
        Transaction transaction = (Transaction)parent;
        return transaction.getTxId();
    }

    public int getIndex() {
        Transaction transaction = (Transaction)parent;
        return transaction.getOutputs().indexOf(this);
    }
}
