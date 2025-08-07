package com.sparrowwallet.drongo.protocol;

import com.sparrowwallet.drongo.Utils;
import com.sparrowwallet.drongo.address.*;
import com.sparrowwallet.drongo.crypto.ECKey;
import org.bouncycastle.util.encoders.Hex;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static com.sparrowwallet.drongo.protocol.ScriptType.*;

import static com.sparrowwallet.drongo.protocol.ScriptOpCodes.*;

public class Script {
    public static final long MAX_SCRIPT_ELEMENT_SIZE = 520;

    // The program is a set of chunks where each element is either [opcode] or [data, data, data ...]
    protected List<ScriptChunk> chunks;

    protected byte[] program;

    public Script(byte[] programBytes) {
        this(programBytes, true);
    }

    Script(byte[] programBytes, boolean parse) {
        program = programBytes;
        if(parse) {
            try {
                parse();
            } catch(ProtocolException e) {

            }
        }
    }

    public Script(List<ScriptChunk> chunks) {
        this.chunks = Collections.unmodifiableList(new ArrayList<>(chunks));
    }

    private static final ScriptChunk[] STANDARD_TRANSACTION_SCRIPT_CHUNKS = {
            new ScriptChunk(ScriptOpCodes.OP_DUP, null),
            new ScriptChunk(ScriptOpCodes.OP_HASH160, null),
            new ScriptChunk(ScriptOpCodes.OP_EQUALVERIFY, null),
            new ScriptChunk(ScriptOpCodes.OP_CHECKSIG, null),
    };

    void parse() {
        chunks = new ArrayList<>(5);   // Common size.
        ByteArrayInputStream bis = new ByteArrayInputStream(program);
        int initialSize = bis.available();
        while (bis.available() > 0) {
            int opcode = bis.read();

            long dataToRead = -1;
            if (opcode >= 0 && opcode < OP_PUSHDATA1) {
                // Read some bytes of data, where how many is the opcode value itself.
                dataToRead = opcode;
            } else if (opcode == OP_PUSHDATA1) {
                if (bis.available() < 1) throw new ProtocolException("Unexpected end of script - OP_PUSHDATA1 was followed by " + bis.available() + " bytes");
                dataToRead = bis.read();
            } else if (opcode == OP_PUSHDATA2) {
                // Read a short, then read that many bytes of data.
                if (bis.available() < 2) throw new ProtocolException("Unexpected end of script - OP_PUSHDATA2 was followed by only " + bis.available() + " bytes");
                dataToRead = Utils.readUint16FromStream(bis);
            } else if (opcode == OP_PUSHDATA4) {
                // Read a uint32, then read that many bytes of data.
                // Though this is allowed, because its value cannot be > 520, it should never actually be used
                if (bis.available() < 4) throw new ProtocolException("Unexpected end of script - OP_PUSHDATA4 was followed by only " + bis.available() + " bytes");
                dataToRead = Utils.readUint32FromStream(bis);
            }

            ScriptChunk chunk;
            if (dataToRead == -1) {
                chunk = new ScriptChunk(opcode, null);
            } else {
                if (dataToRead > bis.available())
                    throw new ProtocolException("Push of data element that is larger than remaining data");
                byte[] data = new byte[(int)dataToRead];
                if(dataToRead != 0 && bis.read(data, 0, (int)dataToRead) != dataToRead) {
                    throw new ProtocolException();
                }

                chunk = new ScriptChunk(opcode, data);
            }
            // Save some memory by eliminating redundant copies of the same chunk objects.
            for (ScriptChunk c : STANDARD_TRANSACTION_SCRIPT_CHUNKS) {
                if (c.equals(chunk)) chunk = c;
            }
            chunks.add(chunk);
        }
    }

    /** Returns the serialized program as a newly created byte array. */
    public byte[] getProgram() {
        try {
            // Don't round-trip as Bitcoin Core doesn't and it would introduce a mismatch.
            if (program != null)
                return Arrays.copyOf(program, program.length);
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            for (ScriptChunk chunk : chunks) {
                chunk.write(bos);
            }
            program = bos.toByteArray();
            return program;
        } catch (IOException e) {
            throw new RuntimeException(e);  // Cannot happen.
        }
    }

    public String getProgramAsHex() {
        return Hex.toHexString(getProgram());
    }

    public boolean isEmpty() {
        return chunks.isEmpty();
    }

    public List<ScriptChunk> getChunks() {
        return Collections.unmodifiableList(chunks);
    }

    /**
     * Returns true if this script has the required form to contain a destination address
     */
    public boolean containsToAddress() {
        for(ScriptType scriptType : ScriptType.values()) {
            if(scriptType.isScriptType(this)) {
                return true;
            }
        }

        return false;
    }

    /**
     * <p>If the program somehow pays to a pubkey, returns the pubkey.</p>
     *
     * <p>Otherwise this method throws a ScriptException.</p>
     */
    public ECKey getPubKey() throws ProtocolException {
        for(ScriptType scriptType : SINGLE_KEY_TYPES) {
            if(scriptType.isScriptType(this)) {
                return scriptType.getPublicKeyFromScript(this);
            }
        }

        throw new ProtocolException("Script not a standard form that contains a single key");
    }

    /**
     * <p>If the program somehow pays to a hash, returns the hash.</p>
     *
     * <p>Otherwise this method throws a ScriptException.</p>
     */
    public byte[] getPubKeyHash() throws ProtocolException {
        for(ScriptType scriptType : SINGLE_HASH_TYPES) {
            if(scriptType.isScriptType(this)) {
                return scriptType.getHashFromScript(this);
            }
        }

        throw new ProtocolException("Script not a standard form that contains a single hash");
    }

    public Address getToAddress() {
        try {
            return getToAddresses()[0];
        } catch(Exception e) {
            return null;
        }
    }

    /**
     * Gets the destination address from this script, if it's in the required form.
     */
    public Address[] getToAddresses() throws NonStandardScriptException {
        for(ScriptType scriptType : SINGLE_HASH_TYPES) {
            if(scriptType.isScriptType(this)) {
                return new Address[] { scriptType.getAddress(scriptType.getHashFromScript(this)) };
            }
        }

        //Special handling for taproot tweaked keys - we don't want to tweak them again
        if(P2TR.isScriptType(this)) {
            return new Address[] { new P2TRAddress(P2TR.getPublicKeyFromScript(this).getPubKeyXCoord()) };
        }

        for(ScriptType scriptType : SINGLE_KEY_TYPES) {
            if(scriptType.isScriptType(this)) {
                return new Address[] { scriptType.getAddress(scriptType.getPublicKeyFromScript(this)) };
            }
        }

        if(MULTISIG.isScriptType(this)) {
            List<Address> addresses = new ArrayList<>();
            ECKey[] pubKeys = MULTISIG.getPublicKeysFromScript(this);
            for(ECKey pubKey : pubKeys) {
                addresses.add(new P2PKAddress(pubKey.getPubKey()));
            }

            return addresses.toArray(new Address[addresses.size()]);
        }

        throw new NonStandardScriptException("Cannot find addresses in non standard script: " + toString());
    }

    public int getNumRequiredSignatures() throws NonStandardScriptException {
        //TODO: Handle P2TR script path spends
        if(P2PK.isScriptType(this) || P2PKH.isScriptType(this) || P2WPKH.isScriptType(this) || P2TR.isScriptType(this)) {
            return 1;
        }

        if(MULTISIG.isScriptType(this)) {
            return MULTISIG.getThreshold(this);
        }

        throw new NonStandardScriptException("Cannot find number of required signatures for script: " + toString());
    }

    public Script getFirstNestedScript() {
        for(ScriptChunk chunk : chunks) {
            if(chunk.isScript()) {
                return new Script(chunk.getData());
            }
        }

        return null;
    }

    public List<TransactionSignature> getSignatures() {
        List<TransactionSignature> signatures = new ArrayList<>();
        for(ScriptChunk chunk : chunks) {
            if(chunk.isSignature()) {
                signatures.add(chunk.getSignature());
            }
        }

        return signatures;
    }

    public static int decodeFromOpN(int opcode) {
        if((opcode != OP_0 && opcode != OP_1NEGATE) && (opcode < OP_1 || opcode > OP_16)) {
            throw new ProtocolException("decodeFromOpN called on non OP_N opcode: " + opcode);
        }

        if (opcode == OP_0)
            return 0;
        else if (opcode == OP_1NEGATE)
            return -1;
        else
            return opcode + 1 - OP_1;
    }

    public static int encodeToOpN(int value) {
        if(value < -1 || value > 16) {
            throw new ProtocolException("encodeToOpN called for " + value + " which we cannot encode in an opcode.");
        }
        if (value == 0)
            return OP_0;
        else if (value == -1)
            return OP_1NEGATE;
        else
            return value - 1 + OP_1;
    }

    public static byte[] removeAllInstancesOfOp(byte[] inputScript, int opCode) {
        return removeAllInstancesOf(inputScript, new byte[] {(byte)opCode});
    }

    public static byte[] removeAllInstancesOf(byte[] inputScript, byte[] chunkToRemove) {
        throw new ProtocolException("Not implemented");
    }

    private static boolean equalsRange(byte[] a, int start, byte[] b) {
        if (start + b.length > a.length)
            return false;
        for (int i = 0; i < b.length; i++)
            if (a[i + start] != b[i])
                return false;
        return true;
    }

    public String toString() {
        StringBuilder builder = new StringBuilder();
        for(ScriptChunk chunk : chunks) {
            builder.append(chunk.toString());
            builder.append(" ");
        }

        return builder.toString().trim();
    }

    public String toDisplayString() {
        return toDisplayString(chunks);
    }

    static String toDisplayString(List<ScriptChunk> scriptChunks) {
        StringBuilder builder = new StringBuilder();
        int signatureCount = 1;
        int pubKeyCount = 1;
        for(ScriptChunk chunk : scriptChunks) {
            if(chunk.isSignature()) {
                builder.append("<signature").append(signatureCount++).append(">");
            } else if(chunk.isScript()) {
                Script nestedScript = chunk.getScript();
                if(P2WPKH.isScriptType(nestedScript)) {
                    builder.append("(OP_0 <wpkh>)");
                } else if(P2WSH.isScriptType(nestedScript)) {
                    builder.append("(OP_0 <wsh>)");
                } else {
                    builder.append("(").append(nestedScript.toDisplayString()).append(")");
                }
            } else if(chunk.isPubKey()) {
                builder.append("<pubkey").append(pubKeyCount++).append(">");
            } else {
                builder.append(chunk.toString());
            }

            builder.append(" ");
        }

        return builder.toString().trim();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return Arrays.equals(getQuickProgram(), ((Script)o).getQuickProgram());
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(getQuickProgram());
    }

    // Utility that doesn't copy for internal use
    private byte[] getQuickProgram() {
        if (program != null)
            return program;
        return getProgram();
    }
}
