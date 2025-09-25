package com.sparrowwallet.drongo.protocol;

import com.sparrowwallet.drongo.Utils;
import com.sparrowwallet.drongo.address.Address;
import com.sparrowwallet.drongo.crypto.ECKey;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import static com.sparrowwallet.drongo.Utils.uint32ToByteStreamLE;
import static com.sparrowwallet.drongo.Utils.uint64ToByteStreamLE;

public class Transaction extends ChildMessage {
    public static final int MAX_BLOCK_SIZE = 1000 * 1000;
    public static final long MAX_BITCOIN = 21 * 1000 * 1000L;
    public static final long SATOSHIS_PER_BITCOIN = 100 * 1000 * 1000L;
    public static final long MAX_BLOCK_LOCKTIME = 500000000L;
    public static final int WITNESS_SCALE_FACTOR = 4;
    public static final int DEFAULT_SEGWIT_FLAG = 1;
    public static final int COINBASE_MATURITY_THRESHOLD = 100;

    //Min feerate for defining dust, defined in sats/vByte
    //From: https://github.com/bitcoin/bitcoin/blob/0.19/src/policy/policy.h#L50
    public static final double DUST_RELAY_TX_FEE = 3d;

    //Default min feerate, defined in sats/vByte
    public static final double DEFAULT_MIN_RELAY_FEE = 1d;

    public static final byte LEAF_VERSION_TAPSCRIPT = (byte)0xc0;

    private long version;
    private long locktime;
    private boolean segwit;
    private int segwitFlag;

    private Sha256Hash cachedTxId;
    private Sha256Hash cachedWTxId;

    private ArrayList<TransactionInput> inputs;
    private ArrayList<TransactionOutput> outputs;

    public Transaction() {
        version = 1;
        inputs = new ArrayList<>();
        outputs = new ArrayList<>();
        length = 8;
    }

    public Transaction(byte[] rawtx) {
        super(rawtx, 0);
    }

    public long getVersion() {
        return version;
    }

    public void setVersion(long version) {
        this.version = version;
    }

    public long getLocktime() {
        return locktime;
    }

    public void setLocktime(long locktime) {
        this.locktime = locktime;
    }

    public boolean isLocktimeEnabled() {
        if(locktime == 0) return false;
        return isLocktimeSequenceEnabled();
    }

    public boolean isLocktimeSequenceEnabled() {
        for(TransactionInput input : inputs) {
            if(!input.isAbsoluteTimeLockDisabled()) {
                return true;
            }
        }

        return false;
    }

    public boolean isRelativeLocktimeAllowed() {
        return version >= 2L;
    }

    public boolean isReplaceByFee() {
        for(TransactionInput input : inputs) {
            if(input.isReplaceByFeeEnabled()) {
                return true;
            }
        }

        return false;
    }

    public Sha256Hash getTxId() {
        if (cachedTxId == null) {
            if (!hasWitnesses() && cachedWTxId != null) {
                cachedTxId = cachedWTxId;
            } else {
                cachedTxId = calculateTxId(false);
            }
        }
        return cachedTxId;
    }

    public Sha256Hash getWTxId() {
        if (cachedWTxId == null) {
            if (!hasWitnesses() && cachedTxId != null) {
                cachedWTxId = cachedTxId;
            } else {
                cachedWTxId = calculateTxId(true);
            }
        }
        return cachedWTxId;
    }

    public Sha256Hash calculateTxId(boolean useWitnesses) {
        ByteArrayOutputStream stream = new UnsafeByteArrayOutputStream(length < 32 ? 32 : length + 32);
        try {
            bitcoinSerializeToStream(stream, useWitnesses);
        } catch (IOException e) {
            throw new RuntimeException(e); // cannot happen
        }
        return Sha256Hash.wrapReversed(Sha256Hash.hashTwice(stream.toByteArray()));
    }

    public boolean isSegwit() {
        return segwit;
    }

    public int getSegwitFlag() {
        return segwitFlag;
    }

    public void setSegwitFlag(int segwitFlag) {
        if(!segwit) {
            adjustLength(2);
            this.segwit = true;
        }

        this.segwitFlag = segwitFlag;
    }

    public void clearSegwit() {
        if(segwit) {
            adjustLength(-2);
            segwit = false;
        }
    }

    public boolean hasScriptSigs() {
        for(TransactionInput in : inputs) {
            if(in.getScriptBytes().length > 0) {
                return true;
            }
        }

        return false;
    }

    public boolean hasWitnesses() {
        for(TransactionInput in : inputs) {
            if(in.hasWitness()) {
                return true;
            }
        }

        return false;
    }

    public byte[] bitcoinSerialize() {
        boolean useWitnessFormat = isSegwit();
        return bitcoinSerialize(useWitnessFormat);
    }

    public byte[] bitcoinSerialize(boolean useWitnessFormat) {
        try {
            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            bitcoinSerializeToStream(outputStream, useWitnessFormat);
            return outputStream.toByteArray();
        } catch (IOException e) {
            //can't happen
        }

        return null;
    }

    public void bitcoinSerializeToStream(OutputStream stream) throws IOException {
        boolean useWitnessFormat = isSegwit();
        bitcoinSerializeToStream(stream, useWitnessFormat);
    }

    /**
     * Serialize according to <a href="https://github.com/bitcoin/bips/blob/master/bip-0144.mediawiki">BIP144</a> or the
     * <a href="https://en.bitcoin.it/wiki/Protocol_documentation#tx">classic format</a>, depending on if segwit is
     * desired.
     */
    protected void bitcoinSerializeToStream(OutputStream stream, boolean useWitnessFormat) throws IOException {
        // version
        uint32ToByteStreamLE(version, stream);

        // marker, flag
        if(useWitnessFormat) {
            stream.write(0);
            stream.write(segwitFlag);
        }

        // txin_count, txins
        stream.write(new VarInt(inputs.size()).encode());
        for(TransactionInput in : inputs) {
            in.bitcoinSerializeToStream(stream);
        }

        // txout_count, txouts
        stream.write(new VarInt(outputs.size()).encode());
        for(TransactionOutput out : outputs) {
            out.bitcoinSerializeToStream(stream);
        }

        // script_witnesses
        if(useWitnessFormat) {
            for(TransactionInput in : inputs) {
                //Per BIP141 all txins must have a witness
                if(!in.hasWitness()) {
                    in.setWitness(new TransactionWitness(this));
                }

                in.getWitness().bitcoinSerializeToStream(stream);
            }
        }

        // lock_time
        uint32ToByteStreamLE(locktime, stream);
    }

    /**
     * Deserialize according to <a href="https://github.com/bitcoin/bips/blob/master/bip-0144.mediawiki">BIP144</a> or
     * the <a href="https://en.bitcoin.it/wiki/Protocol_documentation#tx">classic format</a>, depending on if the
     * transaction is segwit or not.
     */
    public void parse() {
        // version
        version = readUint32();
        // peek at marker
        byte marker = payload[cursor];
        segwit = (marker == 0);
        // marker, flag
        if (segwit) {
            byte[] segwitHeader = readBytes(2);
            segwitFlag = segwitHeader[1];
        }
        // txin_count, txins
        parseInputs();
        // txout_count, txouts
        parseOutputs();
        // script_witnesses
        if (segwit)
            parseWitnesses();
        // lock_time
        locktime = readUint32();

        length = cursor - offset;
    }

    private void parseInputs() {
        long numInputs = readVarInt();
        inputs = new ArrayList<>(Math.min((int) numInputs, Utils.MAX_INITIAL_ARRAY_LENGTH));
        for (long i = 0; i < numInputs; i++) {
            TransactionInput input = new TransactionInput(this, payload, cursor);
            inputs.add(input);
            long scriptLen = readVarInt(TransactionOutPoint.MESSAGE_LENGTH);
            cursor += scriptLen + 4;
        }
    }

    private void parseOutputs() {
        long numOutputs = readVarInt();
        outputs = new ArrayList<>(Math.min((int) numOutputs, Utils.MAX_INITIAL_ARRAY_LENGTH));
        for (long i = 0; i < numOutputs; i++) {
            TransactionOutput output = new TransactionOutput(this, payload, cursor);
            outputs.add(output);
            long scriptLen = readVarInt(8);
            cursor += scriptLen;
        }
    }

    private void parseWitnesses() {
        int numWitnesses = inputs.size();
        for (int i = 0; i < numWitnesses; i++) {
            TransactionWitness witness = new TransactionWitness(this, payload, cursor);
            inputs.get(i).witness(witness);
            cursor += witness.getLength();
        }
    }

    public int getSize() {
        return length;
    }

    public double getVirtualSize() {
        return (double)getWeightUnits() / (double)WITNESS_SCALE_FACTOR;
    }

    public int getWeightUnits() {
        int wu = 0;

        // version
        wu += 4 * WITNESS_SCALE_FACTOR;
        // marker, flag
        if(isSegwit()) {
            wu += 2;
        }
        // txin_count, txins
        wu += new VarInt(inputs.size()).getSizeInBytes() * WITNESS_SCALE_FACTOR;
        for (TransactionInput in : inputs)
            wu += in.length * WITNESS_SCALE_FACTOR;
        // txout_count, txouts
        wu += new VarInt(outputs.size()).getSizeInBytes() * WITNESS_SCALE_FACTOR;
        for (TransactionOutput out : outputs)
            wu += out.length * WITNESS_SCALE_FACTOR;
        // script_witnesses
        if(isSegwit()) {
            for (TransactionInput in : inputs) {
                if (in.hasWitness()) {
                    wu += in.getWitness().getLength();
                }
            }
        }
        // lock_time
        wu += 4 * WITNESS_SCALE_FACTOR;

        return wu;
    }

    public List<TransactionInput> getInputs() {
        return Collections.unmodifiableList(inputs);
    }

    public TransactionInput addInput(Sha256Hash spendTxHash, long outputIndex, Script script) {
        if(isSegwit()) {
            return addInput(spendTxHash, outputIndex, script, new TransactionWitness(this));
        } else {
            return addInput(new TransactionInput(this, new TransactionOutPoint(spendTxHash, outputIndex), script.getProgram()));
        }
    }

    public TransactionInput addInput(Sha256Hash spendTxHash, long outputIndex, Script script, TransactionWitness witness) {
        if(!isSegwit()) {
            setSegwitFlag(DEFAULT_SEGWIT_FLAG);
        }

        return addInput(new TransactionInput(this, new TransactionOutPoint(spendTxHash, outputIndex), script.getProgram(), witness));
    }

    public TransactionInput addInput(TransactionInput input) {
        input.setParent(this);
        inputs.add(input);
        adjustLength(inputs.size(), input.length);
        return input;
    }

    public List<TransactionOutput> getOutputs() {
        return Collections.unmodifiableList(outputs);
    }

    public void shuffleOutputs() {
        Collections.shuffle(outputs);
    }

    public TransactionOutput addOutput(long value, Script script) {
        return addOutput(new TransactionOutput(this, value, script));
    }

    public TransactionOutput addOutput(long value, Address address) {
        return addOutput(new TransactionOutput(this, value, address.getOutputScript()));
    }

    public TransactionOutput addOutput(long value, ECKey pubkey) {
        return addOutput(new TransactionOutput(this, value, ScriptType.P2PK.getOutputScript(pubkey)));
    }

    public TransactionOutput addOutput(TransactionOutput output) {
        output.setParent(this);
        outputs.add(output);
        adjustLength(outputs.size(), output.length);
        return output;
    }

    public void verify() throws VerificationException {
        if (inputs.size() == 0 || outputs.size() == 0)
            throw new VerificationException.EmptyInputsOrOutputs();
        if (this.getMessageSize() > MAX_BLOCK_SIZE)
            throw new VerificationException.LargerThanMaxBlockSize();

        HashSet<TransactionOutPoint> outpoints = new HashSet<>();
        for (TransactionInput input : inputs) {
            if (outpoints.contains(input.getOutpoint()))
                throw new VerificationException.DuplicatedOutPoint();
            outpoints.add(input.getOutpoint());
        }

        long valueOut = 0L;
        for (TransactionOutput output : outputs) {
            long value = output.getValue();
            if (value < 0)
                throw new VerificationException.NegativeValueOutput();
            try {
                valueOut += value;
            } catch (ArithmeticException e) {
                throw new VerificationException.ExcessiveValue();
            }
            double bitcoin = (double)value/SATOSHIS_PER_BITCOIN;
            if (bitcoin > MAX_BITCOIN) {
                throw new VerificationException.ExcessiveValue();
            }
        }

        if (isCoinBase()) {
            if (inputs.get(0).getScriptBytes().length < 2 || inputs.get(0).getScriptBytes().length > 100)
                throw new VerificationException.CoinbaseScriptSizeOutOfRange();
        } else {
            for (TransactionInput input : inputs)
                if (input.isCoinBase())
                    throw new VerificationException.UnexpectedCoinbaseInput();
        }
    }

    public boolean isCoinBase() {
        return inputs.size() == 1 && inputs.get(0).isCoinBase();
    }

    public static boolean isTransaction(byte[] bytes) {
        //Incomplete quick test
        if(bytes.length == 0) {
            return false;
        }
        long version = Utils.readUint32(bytes, 0);
        return version > 0 && version < 5;
    }

    public Sha256Hash hashForLegacySignature(int inputIndex, Script redeemScript, SigHash sigHash) {
        return hashForLegacySignature(inputIndex, redeemScript.getProgram(), sigHash.value);
    }

    public Sha256Hash hashForLegacySignature(int inputIndex, byte[] connectedScript, byte sigHashType) {
        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            this.bitcoinSerializeToStream(baos);
            Transaction tx = new Transaction(baos.toByteArray());

            // Clear input scripts in preparation for signing. If we're signing a fresh
            // transaction that step isn't very helpful, but it doesn't add much cost relative to the actual
            // EC math so we'll do it anyway.
            for (int i = 0; i < tx.inputs.size(); i++) {
                TransactionInput input = tx.inputs.get(i);
                input.clearScriptBytes();
            }

            // This step has no purpose beyond being synchronized with Bitcoin Core's bugs. OP_CODESEPARATOR
            // is a legacy holdover from a previous, broken design of executing scripts that shipped in Bitcoin 0.1.
            // It was seriously flawed and would have let anyone take anyone elses money. Later versions switched to
            // the design we use today where scripts are executed independently but share a stack. This left the
            // OP_CODESEPARATOR instruction having no purpose as it was only meant to be used internally, not actually
            // ever put into scripts. Deleting OP_CODESEPARATOR is a step that should never be required but if we don't
            // do it, we could split off the best chain.
            connectedScript = Script.removeAllInstancesOfOp(connectedScript, ScriptOpCodes.OP_CODESEPARATOR);

            TransactionInput input = tx.inputs.get(inputIndex);
            input.setScriptBytes(connectedScript);

            if((sigHashType & 0x1f) == SigHash.NONE.value) {
                // SIGHASH_NONE means no outputs are signed at all - the signature is effectively for a "blank cheque".
                tx.outputs = new ArrayList<>(0);
                // The signature isn't broken by new versions of the transaction issued by other parties.
                for(int i = 0; i < tx.inputs.size(); i++) {
                    if(i != inputIndex) {
                        tx.inputs.get(i).setSequenceNumber(0);
                    }
                }
            } else if((sigHashType & 0x1f) == SigHash.SINGLE.value) {
                // SIGHASH_SINGLE means only sign the output at the same index as the input (ie, my output).
                if(inputIndex >= tx.outputs.size()) {
                    // The input index is beyond the number of outputs, it's a buggy signature made by a broken
                    // Bitcoin implementation. Bitcoin Core also contains a bug in handling this case:
                    // any transaction output that is signed in this case will result in both the signed output
                    // and any future outputs to this public key being steal-able by anyone who has
                    // the resulting signature and the public key (both of which are part of the signed tx input).

                    // Bitcoin Core's bug is that SignatureHash was supposed to return a hash and on this codepath it
                    // actually returns the constant "1" to indicate an error, which is never checked for. Oops.
                    return Sha256Hash.wrap("0100000000000000000000000000000000000000000000000000000000000000");
                }
                // In SIGHASH_SINGLE the outputs after the matching input index are deleted, and the outputs before
                // that position are "nulled out". Unintuitively, the value in a "null" transaction is set to -1.
                tx.outputs = new ArrayList<>(tx.outputs.subList(0, inputIndex + 1));
                for(int i = 0; i < inputIndex; i++) {
                    tx.outputs.set(i, new TransactionOutput(tx, -1L, new byte[]{}));
                }
                // The signature isn't broken by new versions of the transaction issued by other parties.
                for(int i = 0; i < tx.inputs.size(); i++) {
                    if(i != inputIndex) {
                        tx.inputs.get(i).setSequenceNumber(0);
                    }
                }
            }

            if((sigHashType & SigHash.ANYONECANPAY.value) == SigHash.ANYONECANPAY.value) {
                // SIGHASH_ANYONECANPAY means the signature in the input is not broken by changes/additions/removals
                // of other inputs. For example, this is useful for building assurance contracts.
                tx.inputs = new ArrayList<>();
                tx.inputs.add(input);
            }

            ByteArrayOutputStream bos = new ByteArrayOutputStream(tx.length);
            tx.bitcoinSerializeToStream(bos, false);
            // We also have to write a hash type (sigHashType is actually an unsigned char)
            uint32ToByteStreamLE(0x000000ff & sigHashType, bos);
            // Note that this is NOT reversed to ensure it will be signed correctly. If it were to be printed out
            // however then we would expect that it is IS reversed.
            Sha256Hash hash = Sha256Hash.twiceOf(bos.toByteArray());
            bos.close();

            return hash;
        } catch (IOException e) {
            throw new RuntimeException(e);  // Cannot happen.
        }
    }

    /**
     * <p>Calculates a signature hash, that is, a hash of a simplified form of the transaction. How exactly the transaction
     * is simplified is specified by the type and anyoneCanPay parameters.</p>
     *
     * (See BIP143: https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki)</p>
     *
     * @param inputIndex   input the signature is being calculated for. Tx signatures are always relative to an input.
     * @param scriptCode   the script that should be in the given input during signing.
     * @param prevValue    the value of the coin being spent
     * @param sigHash      Should usually be SigHash.ALL
     */
    public synchronized Sha256Hash hashForWitnessSignature(int inputIndex, Script scriptCode, long prevValue, SigHash sigHash) {
        return hashForWitnessSignature(inputIndex, scriptCode.getProgram(), prevValue, sigHash.value);
    }

    public synchronized Sha256Hash hashForWitnessSignature(int inputIndex, byte[] scriptCode, long prevValue, byte sigHashType) {
        ByteArrayOutputStream bos = new UnsafeByteArrayOutputStream(length == UNKNOWN_LENGTH ? 256 : length + 4);
        try {
            byte[] hashPrevouts = new byte[32];
            byte[] hashSequence = new byte[32];
            byte[] hashOutputs = new byte[32];
            int basicSigHashType = sigHashType & 0x1f;
            boolean anyoneCanPay = (sigHashType & SigHash.ANYONECANPAY.value) == SigHash.ANYONECANPAY.value;
            boolean signAll = (basicSigHashType != SigHash.SINGLE.value) && (basicSigHashType != SigHash.NONE.value);

            if(!anyoneCanPay) {
                ByteArrayOutputStream bosHashPrevouts = new UnsafeByteArrayOutputStream(256);
                for(int i = 0; i < this.inputs.size(); ++i) {
                    bosHashPrevouts.write(this.inputs.get(i).getOutpoint().getHash().getReversedBytes());
                    uint32ToByteStreamLE(this.inputs.get(i).getOutpoint().getIndex(), bosHashPrevouts);
                }
                hashPrevouts = Sha256Hash.hashTwice(bosHashPrevouts.toByteArray());
            }

            if(!anyoneCanPay && signAll) {
                ByteArrayOutputStream bosSequence = new UnsafeByteArrayOutputStream(256);
                for(int i = 0; i < this.inputs.size(); ++i) {
                    uint32ToByteStreamLE(this.inputs.get(i).getSequenceNumber(), bosSequence);
                }
                hashSequence = Sha256Hash.hashTwice(bosSequence.toByteArray());
            }

            if(signAll) {
                ByteArrayOutputStream bosHashOutputs = new UnsafeByteArrayOutputStream(256);
                for(int i = 0; i < this.outputs.size(); ++i) {
                    uint64ToByteStreamLE(BigInteger.valueOf(this.outputs.get(i).getValue()), bosHashOutputs);
                    bosHashOutputs.write(new VarInt(this.outputs.get(i).getScriptBytes().length).encode());
                    bosHashOutputs.write(this.outputs.get(i).getScriptBytes());
                }
                hashOutputs = Sha256Hash.hashTwice(bosHashOutputs.toByteArray());
            } else if(basicSigHashType == SigHash.SINGLE.value && inputIndex < outputs.size()) {
                ByteArrayOutputStream bosHashOutputs = new UnsafeByteArrayOutputStream(256);
                uint64ToByteStreamLE(BigInteger.valueOf(this.outputs.get(inputIndex).getValue()), bosHashOutputs);
                bosHashOutputs.write(new VarInt(this.outputs.get(inputIndex).getScriptBytes().length).encode());
                bosHashOutputs.write(this.outputs.get(inputIndex).getScriptBytes());
                hashOutputs = Sha256Hash.hashTwice(bosHashOutputs.toByteArray());
            }

            uint32ToByteStreamLE(version, bos);
            bos.write(hashPrevouts);
            bos.write(hashSequence);
            bos.write(inputs.get(inputIndex).getOutpoint().getHash().getReversedBytes());
            uint32ToByteStreamLE(inputs.get(inputIndex).getOutpoint().getIndex(), bos);
            VarInt scriptLength = new VarInt(scriptCode.length);
            bos.write(scriptLength.encode());
            bos.write(scriptCode);
            uint64ToByteStreamLE(BigInteger.valueOf(prevValue), bos);
            uint32ToByteStreamLE(inputs.get(inputIndex).getSequenceNumber(), bos);
            bos.write(hashOutputs);
            uint32ToByteStreamLE(this.locktime, bos);
            uint32ToByteStreamLE(0x000000ff & sigHashType, bos);
        } catch (IOException e) {
            throw new RuntimeException(e);  // Cannot happen.
        }

        return Sha256Hash.twiceOf(bos.toByteArray());
    }

    /**
     * <p>Calculates a signature hash, that is, a hash of a simplified form of the transaction. How exactly the transaction
     * is simplified is specified by the type and anyoneCanPay parameters.</p>
     *
     * (See BIP341: https://github.com/bitcoin/bips/blob/master/bip-0341.mediawiki)</p>
     *
     * @param spentUtxos   the ordered list of spent UTXOs corresponding to the inputs of this transaction
     * @param inputIndex   input the signature is being calculated for. Tx signatures are always relative to an input.
     * @param scriptPath   whether we are signing for the keypath or the scriptpath
     * @param script       if signing for the scriptpath, the script to sign
     * @param sigHash      should usually be SigHash.ALL
     * @param annex        annex data
     */
    public synchronized Sha256Hash hashForTaprootSignature(List<TransactionOutput> spentUtxos, int inputIndex, boolean scriptPath, Script script, SigHash sigHash, byte[] annex) {
        return hashForTaprootSignature(spentUtxos, inputIndex, scriptPath, script, sigHash.value, annex);
    }

    public synchronized Sha256Hash hashForTaprootSignature(List<TransactionOutput> spentUtxos, int inputIndex, boolean scriptPath, Script script, byte sigHashType, byte[] annex) {
        if(spentUtxos.size() != getInputs().size()) {
            throw new IllegalArgumentException("Provided spent UTXOs length does not equal the number of transaction inputs");
        }
        if(inputIndex >= getInputs().size()) {
            throw new IllegalArgumentException("Input index is greater than the number of transaction inputs");
        }

        ByteArrayOutputStream bos = new UnsafeByteArrayOutputStream(length == UNKNOWN_LENGTH ? 256 : length + 4);
        try {
            byte outType = sigHashType == 0x00 ? SigHash.ALL.value : (byte)(sigHashType & 0x03);
            boolean anyoneCanPay = (sigHashType & SigHash.ANYONECANPAY.value) == SigHash.ANYONECANPAY.value;

            bos.write(0x00);
            bos.write(sigHashType);
            uint32ToByteStreamLE(this.version, bos);
            uint32ToByteStreamLE(this.locktime, bos);

            if(!anyoneCanPay) {
                ByteArrayOutputStream outpoints = new ByteArrayOutputStream();
                ByteArrayOutputStream outputValues = new ByteArrayOutputStream();
                ByteArrayOutputStream outputScriptPubKeys = new ByteArrayOutputStream();
                ByteArrayOutputStream inputSequences = new ByteArrayOutputStream();
                for(int i = 0; i < getInputs().size(); i++) {
                    TransactionInput input = getInputs().get(i);
                    input.getOutpoint().bitcoinSerializeToStream(outpoints);
                    Utils.uint64ToByteStreamLE(BigInteger.valueOf(spentUtxos.get(i).getValue()), outputValues);
                    byteArraySerialize(spentUtxos.get(i).getScriptBytes(), outputScriptPubKeys);
                    Utils.uint32ToByteStreamLE(input.getSequenceNumber(), inputSequences);
                }
                bos.write(Sha256Hash.hash(outpoints.toByteArray()));
                bos.write(Sha256Hash.hash(outputValues.toByteArray()));
                bos.write(Sha256Hash.hash(outputScriptPubKeys.toByteArray()));
                bos.write(Sha256Hash.hash(inputSequences.toByteArray()));
            }

            if(outType == SigHash.ALL.value) {
                ByteArrayOutputStream outputs = new ByteArrayOutputStream();
                for(TransactionOutput output : getOutputs()) {
                    output.bitcoinSerializeToStream(outputs);
                }
                bos.write(Sha256Hash.hash(outputs.toByteArray()));
            }

            byte spendType = 0x00;
            if(annex != null) {
                spendType |= 0x01;
            }
            if(scriptPath) {
                spendType |= 0x02;
            }
            bos.write(spendType);

            if(anyoneCanPay) {
                getInputs().get(inputIndex).getOutpoint().bitcoinSerializeToStream(bos);
                Utils.int64ToByteStreamLE(spentUtxos.get(inputIndex).getValue(), bos);
                byteArraySerialize(spentUtxos.get(inputIndex).getScriptBytes(), bos);
                Utils.uint32ToByteStreamLE(getInputs().get(inputIndex).getSequenceNumber(), bos);
            } else {
                Utils.uint32ToByteStreamLE(inputIndex, bos);
            }

            if((spendType & 0x01) != 0) {
                ByteArrayOutputStream annexStream = new ByteArrayOutputStream();
                byteArraySerialize(annex, annexStream);
                bos.write(Sha256Hash.hash(annexStream.toByteArray()));
            }

            if(outType == SigHash.SINGLE.value) {
                if(inputIndex < getOutputs().size()) {
                    bos.write(Sha256Hash.hash(getOutputs().get(inputIndex).bitcoinSerialize()));
                } else {
                    bos.write(Sha256Hash.ZERO_HASH.getBytes());
                }
            }

            if(scriptPath) {
                ByteArrayOutputStream leafStream = new ByteArrayOutputStream();
                leafStream.write(LEAF_VERSION_TAPSCRIPT);
                byteArraySerialize(script.getProgram(), leafStream);
                bos.write(Utils.taggedHash("TapLeaf", leafStream.toByteArray()));
                bos.write(0x00);
                Utils.uint32ToByteStreamLE(-1, bos);
            }

            byte[] msgBytes = bos.toByteArray();
            long requiredLength = 175 - (anyoneCanPay ? 49 : 0) - (outType != SigHash.ALL.value && outType != SigHash.SINGLE.value ? 32 : 0) + (annex != null ? 32 : 0) + (scriptPath ? 37 : 0);
            if(msgBytes.length != requiredLength) {
                throw new IllegalStateException("Invalid message length, was " + msgBytes.length + " not " + requiredLength);
            }

            return Sha256Hash.wrap(Utils.taggedHash("TapSighash", msgBytes));
        } catch (IOException e) {
            throw new RuntimeException(e);  // Cannot happen.
        }
    }

    private void byteArraySerialize(byte[] bytes, OutputStream outputStream) throws IOException {
        outputStream.write(new VarInt(bytes.length).encode());
        outputStream.write(bytes);
    }
}
