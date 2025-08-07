package com.sparrowwallet.drongo.psbt;

import com.sparrowwallet.drongo.KeyDerivation;
import com.sparrowwallet.drongo.Utils;
import com.sparrowwallet.drongo.crypto.ECKey;
import com.sparrowwallet.drongo.protocol.NonStandardScriptException;
import com.sparrowwallet.drongo.protocol.Script;
import com.sparrowwallet.drongo.protocol.ScriptType;
import com.sparrowwallet.drongo.protocol.Sha256Hash;
import com.sparrowwallet.drongo.protocol.SigHash;
import com.sparrowwallet.drongo.protocol.Transaction;
import com.sparrowwallet.drongo.protocol.TransactionInput;
import com.sparrowwallet.drongo.protocol.TransactionOutput;
import com.sparrowwallet.drongo.protocol.TransactionSignature;
import com.sparrowwallet.drongo.protocol.TransactionWitness;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.sparrowwallet.drongo.protocol.ScriptType.P2SH;
import static com.sparrowwallet.drongo.protocol.ScriptType.P2SH_P2WPKH;
import static com.sparrowwallet.drongo.protocol.ScriptType.P2SH_P2WSH;
import static com.sparrowwallet.drongo.protocol.ScriptType.P2TR;
import static com.sparrowwallet.drongo.protocol.ScriptType.P2WPKH;
import static com.sparrowwallet.drongo.protocol.ScriptType.P2WSH;
import static com.sparrowwallet.drongo.protocol.ScriptType.WITNESS_TYPES;
import static com.sparrowwallet.drongo.protocol.TransactionSignature.Type.ECDSA;
import static com.sparrowwallet.drongo.protocol.TransactionSignature.Type.SCHNORR;
import static com.sparrowwallet.drongo.psbt.PSBTEntry.parseKeyDerivation;
import static com.sparrowwallet.drongo.psbt.PSBTEntry.parseTaprootKeyDerivation;

public class PSBTInput {
    public static final byte PSBT_IN_NON_WITNESS_UTXO = 0x00;
    public static final byte PSBT_IN_WITNESS_UTXO = 0x01;
    public static final byte PSBT_IN_PARTIAL_SIG = 0x02;
    public static final byte PSBT_IN_SIGHASH_TYPE = 0x03;
    public static final byte PSBT_IN_REDEEM_SCRIPT = 0x04;
    public static final byte PSBT_IN_WITNESS_SCRIPT = 0x05;
    public static final byte PSBT_IN_BIP32_DERIVATION = 0x06;
    public static final byte PSBT_IN_FINAL_SCRIPTSIG = 0x07;
    public static final byte PSBT_IN_FINAL_SCRIPTWITNESS = 0x08;
    public static final byte PSBT_IN_POR_COMMITMENT = 0x09;
    public static final byte PSBT_IN_PROPRIETARY = (byte)0xfc;
    public static final byte PSBT_IN_TAP_KEY_SIG = 0x13;
    public static final byte PSBT_IN_TAP_BIP32_DERIVATION = 0x16;
    public static final byte PSBT_IN_TAP_INTERNAL_KEY = 0x17;

    private final PSBT psbt;
    private Transaction nonWitnessUtxo;
    private TransactionOutput witnessUtxo;
    private final Map<ECKey, TransactionSignature> partialSignatures = new LinkedHashMap<>();
    private SigHash sigHash;
    private Script redeemScript;
    private Script witnessScript;
    private final Map<ECKey, KeyDerivation> derivedPublicKeys = new LinkedHashMap<>();
    private Script finalScriptSig;
    private TransactionWitness finalScriptWitness;
    private String porCommitment;
    private final Map<String, String> proprietary = new LinkedHashMap<>();
    private TransactionSignature tapKeyPathSignature;
    private Map<ECKey, Map<KeyDerivation, List<Sha256Hash>>> tapDerivedPublicKeys = new LinkedHashMap<>();
    private ECKey tapInternalKey;

    private final Transaction transaction;
    private final int index;

    PSBTInput(PSBT psbt, Transaction transaction, int index) {
        this.psbt = psbt;
        this.transaction = transaction;
        this.index = index;
    }

    PSBTInput(PSBT psbt, ScriptType scriptType, Transaction transaction, int index, Transaction utxo, int utxoIndex, Script redeemScript, Script witnessScript, Map<ECKey, KeyDerivation> derivedPublicKeys, Map<String, String> proprietary, ECKey tapInternalKey, boolean alwaysAddNonWitnessTx) {
        this(psbt, transaction, index);

        if(Arrays.asList(ScriptType.WITNESS_TYPES).contains(scriptType)) {
            this.witnessUtxo = utxo.getOutputs().get(utxoIndex);
        } else {
            this.nonWitnessUtxo = utxo;
        }

        if(alwaysAddNonWitnessTx) {
            //Add non-witness UTXO to segwit types to handle Trezor, Bitbox and Ledger requirements
            this.nonWitnessUtxo = utxo;
        }

        this.redeemScript = redeemScript;
        this.witnessScript = witnessScript;

        if(scriptType != P2TR) {
            this.derivedPublicKeys.putAll(derivedPublicKeys);
        }

        this.proprietary.putAll(proprietary);

        this.tapInternalKey = tapInternalKey == null ? null : ECKey.fromPublicOnly(tapInternalKey.getPubKeyXCoord());

        if(tapInternalKey != null && !derivedPublicKeys.values().isEmpty()) {
            KeyDerivation tapKeyDerivation = derivedPublicKeys.values().iterator().next();
            Map<KeyDerivation, List<Sha256Hash>> innerMap = new HashMap<>();
            innerMap.put(tapKeyDerivation, Collections.emptyList());
            tapDerivedPublicKeys.put(this.tapInternalKey, innerMap);
        }

        this.sigHash = getDefaultSigHash();
    }

    PSBTInput(PSBT psbt, List<PSBTEntry> inputEntries, Transaction transaction, int index) throws PSBTParseException {
        this.psbt = psbt;
        for(PSBTEntry entry : inputEntries) {
            switch(entry.getKeyType()) {
                case PSBT_IN_NON_WITNESS_UTXO:
                    entry.checkOneByteKey();
                    Transaction nonWitnessTx = new Transaction(entry.getData());
                    nonWitnessTx.verify();
                    Sha256Hash inputHash = nonWitnessTx.calculateTxId(false);
                    Sha256Hash outpointHash = transaction.getInputs().get(index).getOutpoint().getHash();
                    if(!outpointHash.equals(inputHash)) {
                        throw new PSBTParseException("Hash of provided non witness utxo transaction " + inputHash + " does not match transaction input outpoint hash " + outpointHash + " at index " + index);
                    }

                    this.nonWitnessUtxo = nonWitnessTx;
                    break;
                case PSBT_IN_WITNESS_UTXO:
                    entry.checkOneByteKey();
                    TransactionOutput witnessTxOutput = new TransactionOutput(null, entry.getData(), 0);
                    if(!P2SH.isScriptType(witnessTxOutput.getScript()) && !P2WPKH.isScriptType(witnessTxOutput.getScript()) && !P2WSH.isScriptType(witnessTxOutput.getScript()) && !P2TR.isScriptType(witnessTxOutput.getScript())) {
                        throw new PSBTParseException("Witness UTXO provided for non-witness or unknown input");
                    }
                    this.witnessUtxo = witnessTxOutput;
                    break;
                case PSBT_IN_PARTIAL_SIG:
                    entry.checkOneBytePlusPubKey();
                    ECKey sigPublicKey = ECKey.fromPublicOnly(entry.getKeyData());
                    //TODO: Verify signature
                    TransactionSignature signature = TransactionSignature.decodeFromBitcoin(ECDSA, entry.getData(), true);
                    this.partialSignatures.put(sigPublicKey, signature);
                    break;
                case PSBT_IN_SIGHASH_TYPE:
                    entry.checkOneByteKey();
                    long sighashType = Utils.readUint32(entry.getData(), 0);
                    this.sigHash = SigHash.fromByte((byte)sighashType);
                    break;
                case PSBT_IN_REDEEM_SCRIPT:
                    entry.checkOneByteKey();
                    Script redeemScript = new Script(entry.getData());
                    Script scriptPubKey = null;
                    if(this.nonWitnessUtxo != null) {
                        scriptPubKey = this.nonWitnessUtxo.getOutputs().get((int)transaction.getInputs().get(index).getOutpoint().getIndex()).getScript();
                    } else if(this.witnessUtxo != null) {
                        scriptPubKey = this.witnessUtxo.getScript();
                        if(!P2WPKH.isScriptType(redeemScript) && !P2WSH.isScriptType(redeemScript)) { //Witness UTXO should only be provided for P2SH-P2WPKH or P2SH-P2WSH
                            throw new PSBTParseException("Witness UTXO provided but redeem script is not P2WPKH or P2WSH");
                        }
                    }
                    this.redeemScript = redeemScript;
                    break;
                case PSBT_IN_WITNESS_SCRIPT:
                    entry.checkOneByteKey();
                    Script witnessScript = new Script(entry.getData());
                    byte[] pubKeyHash = null;
                    if(this.redeemScript != null && P2WSH.isScriptType(this.redeemScript)) { //P2SH-P2WSH
                        pubKeyHash = this.redeemScript.getPubKeyHash();
                    } else if(this.witnessUtxo != null && P2WSH.isScriptType(this.witnessUtxo.getScript())) { //P2WSH
                        pubKeyHash = this.witnessUtxo.getScript().getPubKeyHash();
                    }
                    if(pubKeyHash == null) {

                    } else if(!Arrays.equals(Sha256Hash.hash(witnessScript.getProgram()), pubKeyHash)) {
                        throw new PSBTParseException("Witness script hash does not match provided pay to script hash " + Utils.bytesToHex(pubKeyHash));
                    }
                    this.witnessScript = witnessScript;
                    break;
                case PSBT_IN_BIP32_DERIVATION:
                    entry.checkOneBytePlusPubKey();
                    ECKey derivedPublicKey = ECKey.fromPublicOnly(entry.getKeyData());
                    KeyDerivation keyDerivation = parseKeyDerivation(entry.getData());
                    this.derivedPublicKeys.put(derivedPublicKey, keyDerivation);
                    break;
                case PSBT_IN_FINAL_SCRIPTSIG:
                    entry.checkOneByteKey();
                    Script finalScriptSig = new Script(entry.getData());
                    this.finalScriptSig = finalScriptSig;
                    break;
                case PSBT_IN_FINAL_SCRIPTWITNESS:
                    entry.checkOneByteKey();
                    TransactionWitness finalScriptWitness = new TransactionWitness(null, entry.getData(), 0);
                    this.finalScriptWitness = finalScriptWitness;
                    break;
                case PSBT_IN_POR_COMMITMENT:
                    entry.checkOneByteKey();
                    String porMessage = new String(entry.getData(), StandardCharsets.UTF_8);
                    this.porCommitment = porMessage;
                    break;
                case PSBT_IN_PROPRIETARY:
                    this.proprietary.put(Utils.bytesToHex(entry.getKeyData()), Utils.bytesToHex(entry.getData()));
                    break;
                case PSBT_IN_TAP_KEY_SIG:
                    entry.checkOneByteKey();
                    this.tapKeyPathSignature = TransactionSignature.decodeFromBitcoin(SCHNORR, entry.getData(), true);
                    break;
                case PSBT_IN_TAP_BIP32_DERIVATION:
                    entry.checkOneBytePlusXOnlyPubKey();
                    ECKey tapPublicKey = ECKey.fromPublicOnly(entry.getKeyData());
                    Map<KeyDerivation, List<Sha256Hash>> tapKeyDerivations = parseTaprootKeyDerivation(entry.getData());
                    if(tapKeyDerivations.isEmpty()) {
                    } else {
                        this.tapDerivedPublicKeys.put(tapPublicKey, tapKeyDerivations);
                    }
                    break;
                case PSBT_IN_TAP_INTERNAL_KEY:
                    entry.checkOneByteKey();
                    this.tapInternalKey = ECKey.fromPublicOnly(entry.getData());
                    break;
                default:
                    break;
            }
        }

        this.transaction = transaction;
        this.index = index;
    }

    void combine(PSBTInput psbtInput) {
        if(psbtInput.nonWitnessUtxo != null) {
            nonWitnessUtxo = psbtInput.nonWitnessUtxo;
        }

        if(psbtInput.witnessUtxo != null) {
            witnessUtxo = psbtInput.witnessUtxo;
        }

        partialSignatures.putAll(psbtInput.partialSignatures);

        if(psbtInput.sigHash != null) {
            sigHash = psbtInput.sigHash;
        }

        if(psbtInput.redeemScript != null) {
            redeemScript = psbtInput.redeemScript;
        }

        if(psbtInput.witnessScript != null) {
            witnessScript = psbtInput.witnessScript;
        }

        derivedPublicKeys.putAll(psbtInput.derivedPublicKeys);

        if(psbtInput.porCommitment != null) {
            porCommitment = psbtInput.porCommitment;
        }

        proprietary.putAll(psbtInput.proprietary);

        if(psbtInput.tapKeyPathSignature != null) {
            tapKeyPathSignature = psbtInput.tapKeyPathSignature;
        }

        tapDerivedPublicKeys.putAll(psbtInput.tapDerivedPublicKeys);

        if(psbtInput.tapInternalKey != null) {
            tapInternalKey = psbtInput.tapInternalKey;
        }
    }

    public Transaction getNonWitnessUtxo() {
        return nonWitnessUtxo;
    }

    public void setNonWitnessUtxo(Transaction nonWitnessUtxo) {
        this.nonWitnessUtxo = nonWitnessUtxo;
    }

    public TransactionOutput getWitnessUtxo() {
        return witnessUtxo;
    }

    public void setWitnessUtxo(TransactionOutput witnessUtxo) {
        this.witnessUtxo = witnessUtxo;
    }

    public TransactionSignature getPartialSignature(ECKey publicKey) {
        return partialSignatures.get(publicKey);
    }

    public SigHash getSigHash() {
        return sigHash;
    }

    public void setSigHash(SigHash sigHash) {
        this.sigHash = sigHash;
    }

    public Script getRedeemScript() {
        return redeemScript;
    }

    public void setRedeemScript(Script redeemScript) {
        this.redeemScript = redeemScript;
    }

    public Script getWitnessScript() {
        return witnessScript;
    }

    public void setWitnessScript(Script witnessScript) {
        this.witnessScript = witnessScript;
    }

    public KeyDerivation getKeyDerivation(ECKey publicKey) {
        return derivedPublicKeys.get(publicKey);
    }

    public Script getFinalScriptSig() {
        return finalScriptSig;
    }

    public void setFinalScriptSig(Script finalScriptSig) {
        this.finalScriptSig = finalScriptSig;
    }

    public TransactionWitness getFinalScriptWitness() {
        return finalScriptWitness;
    }

    public void setFinalScriptWitness(TransactionWitness finalScriptWitness) {
        this.finalScriptWitness = finalScriptWitness;
    }

    public String getPorCommitment() {
        return porCommitment;
    }

    public void setPorCommitment(String porCommitment) {
        this.porCommitment = porCommitment;
    }

    public Map<ECKey, TransactionSignature> getPartialSignatures() {
        return partialSignatures;
    }

    public ECKey getKeyForSignature(TransactionSignature signature) {
        for(Map.Entry<ECKey, TransactionSignature> entry : partialSignatures.entrySet()) {
            if(entry.getValue().equals(signature)) {
                return entry.getKey();
            }
        }

        return null;
    }

    public Map<ECKey, KeyDerivation> getDerivedPublicKeys() {
        return derivedPublicKeys;
    }

    public Map<String, String> getProprietary() {
        return proprietary;
    }

    public TransactionSignature getTapKeyPathSignature() {
        return tapKeyPathSignature;
    }

    public void setTapKeyPathSignature(TransactionSignature tapKeyPathSignature) {
        this.tapKeyPathSignature = tapKeyPathSignature;
    }

    public Map<ECKey, Map<KeyDerivation, List<Sha256Hash>>> getTapDerivedPublicKeys() {
        return tapDerivedPublicKeys;
    }

    public void setTapDerivedPublicKeys(Map<ECKey, Map<KeyDerivation, List<Sha256Hash>>> tapDerivedPublicKeys) {
        this.tapDerivedPublicKeys = tapDerivedPublicKeys;
    }

    public ECKey getTapInternalKey() {
        return tapInternalKey;
    }

    public void setTapInternalKey(ECKey tapInternalKey) {
        this.tapInternalKey = tapInternalKey;
    }

    public boolean isTaproot() {
        return getUtxo() != null && getScriptType() == P2TR;
    }

    public boolean isSigned() {
        if(getTapKeyPathSignature() != null) {
            return true;
        } else if(!getPartialSignatures().isEmpty()) {
            try {
                //All partial sigs are already verified
                int reqSigs = getSigningScript().getNumRequiredSignatures();
                int sigs = getPartialSignatures().size();
                return sigs >= reqSigs;
            } catch(NonStandardScriptException e) {
                return false;
            }
        } else {
            return isFinalized();
        }
    }

    public Collection<TransactionSignature> getSignatures() {
        if(getFinalScriptWitness() != null) {
            return getFinalScriptWitness().getSignatures();
        } else if(getFinalScriptSig() != null) {
            return getFinalScriptSig().getSignatures();
        } else if(getTapKeyPathSignature() != null) {
            return Collections.singletonList(getTapKeyPathSignature());
        } else {
            return getPartialSignatures().values();
        }
    }

    private SigHash getDefaultSigHash() {
        if(isTaproot()) {
            return SigHash.DEFAULT;
        }

        return SigHash.ALL;
    }

    public boolean sign(ECKey privKey) {
        return sign(new PSBTInputSigner() {
            @Override
            public TransactionSignature sign(Sha256Hash hash, SigHash sigHash, TransactionSignature.Type signatureType) {
                return privKey.sign(hash, sigHash, signatureType);
            }

            @Override
            public ECKey getPubKey() {
                return ECKey.fromPublicOnly(privKey);
            }
        });
    }

    public boolean sign(PSBTInputSigner psbtInputSigner) {
        SigHash localSigHash = getSigHash();
        if(localSigHash == null) {
            localSigHash = getDefaultSigHash();
        }

        if(getNonWitnessUtxo() != null || getWitnessUtxo() != null) {
            Script signingScript = getSigningScript();
            if(signingScript != null) {
                Sha256Hash hash = getHashForSignature(signingScript, localSigHash);
                TransactionSignature.Type type = isTaproot() ? SCHNORR : ECDSA;
                TransactionSignature transactionSignature = psbtInputSigner.sign(hash, localSigHash, type);

                if(type == SCHNORR) {
                    tapKeyPathSignature = transactionSignature;
                } else {
                    ECKey pubKey = psbtInputSigner.getPubKey();
                    getPartialSignatures().put(pubKey, transactionSignature);
                }

                return true;
            }
        }

        return false;
    }

    boolean verifySignatures() throws PSBTSignatureException {
        SigHash localSigHash = getSigHash();
        if(localSigHash == null) {
            localSigHash = getDefaultSigHash();
        }

        if(getNonWitnessUtxo() != null || getWitnessUtxo() != null) {
            Script signingScript = getSigningScript();
            if(signingScript != null) {
                Sha256Hash hash = getHashForSignature(signingScript, localSigHash);

                if(isTaproot() && tapKeyPathSignature != null) {
                    ECKey outputKey = P2TR.getPublicKeyFromScript(getUtxo().getScript());
                    if(!outputKey.verify(hash, tapKeyPathSignature)) {
                        throw new PSBTSignatureException("Tweaked internal key does not verify against provided taproot keypath signature");
                    }
                } else {
                    for(ECKey sigPublicKey : getPartialSignatures().keySet()) {
                        TransactionSignature signature = getPartialSignature(sigPublicKey);
                        if(!sigPublicKey.verify(hash, signature)) {
                            throw new PSBTSignatureException("Partial signature does not verify against provided public key");
                        }
                    }
                }

                //TODO: Implement Bitcoin Script engine to verify finalScriptSig and finalScriptWitness

                return true;
            }
        }

        return false;
    }

    public Map<ECKey, TransactionSignature> getSigningKeys(Set<ECKey> availableKeys) {
        Collection<TransactionSignature> signatures = getSignatures();
        Script signingScript = getSigningScript();

        Map<ECKey, TransactionSignature> signingKeys = new LinkedHashMap<>();
        if(signingScript != null) {
            Sha256Hash hash = getHashForSignature(signingScript, getSigHash() == null ? getDefaultSigHash() : getSigHash());

            for(ECKey sigPublicKey : availableKeys) {
                for(TransactionSignature signature : signatures) {
                    if(sigPublicKey.verify(hash, signature)) {
                        signingKeys.put(sigPublicKey, signature);
                    }
                }
            }
        }

        return signingKeys;
    }

    public ScriptType getScriptType() {
        Script signingScript = getUtxo().getScript();

        boolean p2sh = false;
        if(P2SH.isScriptType(signingScript)) {
            p2sh = true;

            if(getRedeemScript() != null) {
                signingScript = getRedeemScript();
            } else if(getFinalScriptSig() != null) {
                signingScript = getFinalScriptSig().getFirstNestedScript();
            } else {
                return null;
            }
        }

        if(P2WPKH.isScriptType(signingScript)) {
            return p2sh ? P2SH_P2WPKH : P2WPKH;
        } else if(P2WSH.isScriptType(signingScript)) {
            return p2sh ? P2SH_P2WSH : P2WSH;
        }

        return ScriptType.getType(signingScript);
    }

    public Script getSigningScript() {
        Script signingScript = getUtxo().getScript();

        if(P2SH.isScriptType(signingScript)) {
            if(getRedeemScript() != null) {
                signingScript = getRedeemScript();
            } else if(getFinalScriptSig() != null) {
                signingScript = getFinalScriptSig().getFirstNestedScript();
            } else {
                return null;
            }
        }

        if(P2WPKH.isScriptType(signingScript)) {
            signingScript = ScriptType.P2PKH.getOutputScript(signingScript.getPubKeyHash());
        } else if(P2WSH.isScriptType(signingScript)) {
            if(getWitnessScript() != null) {
                signingScript = getWitnessScript();
            } else if(getFinalScriptWitness() != null && getFinalScriptWitness().getWitnessScript() != null) {
                return getFinalScriptWitness().getWitnessScript();
            } else {
                return null;
            }
        }

        if(P2TR.isScriptType(signingScript)) {
            //For now, only support keypath spends and just return the ScriptPubKey
            //In future return the script from PSBT_IN_TAP_LEAF_SCRIPT
        }

        return signingScript;
    }

    public boolean isFinalized() {
        return getFinalScriptSig() != null || getFinalScriptWitness() != null;
    }

    public TransactionInput getInput() {
        return transaction.getInputs().get(index);
    }

    public TransactionOutput getUtxo() {
        int vout = (int)transaction.getInputs().get(index).getOutpoint().getIndex();
        return getWitnessUtxo() != null ? getWitnessUtxo() : (getNonWitnessUtxo() != null ?  getNonWitnessUtxo().getOutputs().get(vout) : null);
    }

    public void clearNonFinalFields() {
        partialSignatures.clear();
        sigHash = null;
        redeemScript = null;
        witnessScript = null;
        porCommitment = null;
        proprietary.clear();
        tapDerivedPublicKeys.clear();
        tapKeyPathSignature = null;
    }

    private Sha256Hash getHashForSignature(Script connectedScript, SigHash localSigHash) {
        Sha256Hash hash;

        ScriptType scriptType = getScriptType();
        if(scriptType == ScriptType.P2TR) {
            List<PSBTInput> psbtInputs = psbt.getPsbtInputs();
            List<TransactionOutput> spentUtxos = new ArrayList<>();

            for (PSBTInput input : psbtInputs) {
                spentUtxos.add(input.getUtxo());
            }
            hash = transaction.hashForTaprootSignature(spentUtxos, index, !P2TR.isScriptType(connectedScript), connectedScript, localSigHash, null);
        } else if(Arrays.asList(WITNESS_TYPES).contains(scriptType)) {
            long prevValue = getUtxo().getValue();
            hash = transaction.hashForWitnessSignature(index, connectedScript, prevValue, localSigHash);
        } else {
            hash = transaction.hashForLegacySignature(index, connectedScript, localSigHash);
        }

        return hash;
    }
}
