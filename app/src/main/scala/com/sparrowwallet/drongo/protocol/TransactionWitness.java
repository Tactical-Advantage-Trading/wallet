package com.sparrowwallet.drongo.protocol;

import com.sparrowwallet.drongo.crypto.ECKey;
import org.bouncycastle.util.encoders.Hex;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class TransactionWitness extends ChildMessage {
    private List<byte[]> pushes;

    public TransactionWitness(Transaction transaction, TransactionSignature signature) {
        setParent(transaction);
        this.pushes = new ArrayList<>();
        pushes.add(signature.encodeToBitcoin());
    }

    public TransactionWitness(Transaction transaction, ECKey pubKey, TransactionSignature signature) {
        setParent(transaction);
        this.pushes = new ArrayList<>();
        pushes.add(signature.encodeToBitcoin());
        pushes.add(pubKey.getPubKey());
    }

    public TransactionWitness(Transaction transaction, List<TransactionSignature> signatures, Script witnessScript) {
        setParent(transaction);
        this.pushes = new ArrayList<>();
        //If a multisig witness script, add a zero byte witness element to handle the multisig off by one bug
        if(ScriptType.MULTISIG.isScriptType(witnessScript)) {
            pushes.add(new byte[0]);
        }
        for(TransactionSignature signature : signatures) {
            pushes.add(signature.encodeToBitcoin());
        }
        pushes.add(witnessScript.getProgram());
    }

    public TransactionWitness(Transaction transaction) {
        this(transaction, new ArrayList<>());
    }

    public TransactionWitness(Transaction transaction, List<byte[]> witnesses) {
        setParent(transaction);
        this.pushes = witnesses;
    }

    public TransactionWitness(Transaction parent, byte[] rawtx, int offset) {
        super(rawtx, offset);
        setParent(parent);
        if(pushes == null) {
            pushes = new ArrayList<>();
        }
    }

    protected void parse() throws ProtocolException {
        long pushCount = readVarInt();
        for (int y = 0; y < pushCount; y++) {
            long pushSize = readVarInt();
            byte[] push = readBytes((int)pushSize);
            setPush(y, push);
        }
    }

    public List<byte[]> getPushes() {
        return Collections.unmodifiableList(pushes);
    }

    protected void setPush(int i, byte[] value) {
        if(pushes == null) {
            pushes = new ArrayList<>();
        }

        while (i >= pushes.size()) {
            pushes.add(new byte[]{});
        }
        pushes.set(i, value);
    }

    public int getPushCount() {
        return pushes.size();
    }

    public int getLength() {
        int length = new VarInt(pushes.size()).getSizeInBytes();
        for (int i = 0; i < pushes.size(); i++) {
            byte[] push = pushes.get(i);
            if(push.length == 1 && push[0] == 0) {
                length++;
            } else {
                length += new VarInt(push.length).getSizeInBytes();
                length += push.length;
            }
        }

        return length;
    }

    protected void bitcoinSerializeToStream(OutputStream stream) throws IOException {
        stream.write(new VarInt(pushes.size()).encode());
        for(int i = 0; i < pushes.size(); i++) {
            byte[] push = pushes.get(i);
            if(push.length == 1 && push[0] == 0) {
                stream.write(push);
            } else {
                stream.write(new VarInt(push.length).encode());
                stream.write(push);
            }
        }
    }

    public byte[] toByteArray() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            bitcoinSerializeToStream(baos);
        } catch(IOException e) { }

        return baos.toByteArray();
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        for (byte[] push : pushes) {
            if (push == null) {
                builder.append("NULL");
            } else if (push.length == 0) {
                builder.append("EMPTY");
            } else {
                builder.append(Hex.toHexString(push));
            }
            builder.append(" ");
        }

        return builder.toString().trim();
    }

    public List<ScriptChunk> asScriptChunks() {
        List<ScriptChunk> scriptChunks = new ArrayList<>(pushes.size());
        for(byte[] push : pushes) {
            scriptChunks.add(new ScriptChunk(ScriptChunk.getOpcodeForLength(push.length), push));
        }

        return scriptChunks;
    }

    public List<TransactionSignature> getSignatures() {
        List<TransactionSignature> signatures = new ArrayList<>();
        List<ScriptChunk> scriptChunks = this.asScriptChunks();
        for(ScriptChunk chunk : scriptChunks) {
            if(chunk.isSignature()) {
                signatures.add(chunk.getSignature());
            }
        }

        return signatures;
    }

    public Script getWitnessScript() {
        List<ScriptChunk> scriptChunks = this.asScriptChunks();
        if(!scriptChunks.isEmpty() && scriptChunks.get(scriptChunks.size() - 1).isScript()) {
            return scriptChunks.get(scriptChunks.size() - 1).getScript();
        }

        return null;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TransactionWitness other = (TransactionWitness) o;
        if (pushes.size() != other.pushes.size()) return false;
        for (int i = 0; i < pushes.size(); i++) {
            if (!Arrays.equals(pushes.get(i), other.pushes.get(i))) return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int hashCode = 1;
        for (byte[] push : pushes) {
            hashCode = 31 * hashCode + (push == null ? 0 : Arrays.hashCode(push));
        }
        return hashCode;
    }
}
