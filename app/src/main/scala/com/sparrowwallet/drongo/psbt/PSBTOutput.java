package com.sparrowwallet.drongo.psbt;

import com.sparrowwallet.drongo.KeyDerivation;
import com.sparrowwallet.drongo.Utils;
import com.sparrowwallet.drongo.crypto.ECKey;
import com.sparrowwallet.drongo.protocol.Script;
import com.sparrowwallet.drongo.protocol.ScriptType;
import com.sparrowwallet.drongo.protocol.Sha256Hash;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static com.sparrowwallet.drongo.protocol.ScriptType.P2TR;
import static com.sparrowwallet.drongo.psbt.PSBTEntry.parseKeyDerivation;
import static com.sparrowwallet.drongo.psbt.PSBTEntry.parseTaprootKeyDerivation;

public class PSBTOutput {
    public static final byte PSBT_OUT_REDEEM_SCRIPT = 0x00;
    public static final byte PSBT_OUT_WITNESS_SCRIPT = 0x01;
    public static final byte PSBT_OUT_BIP32_DERIVATION = 0x02;
    public static final byte PSBT_OUT_TAP_INTERNAL_KEY = 0x05;
    public static final byte PSBT_OUT_TAP_BIP32_DERIVATION = 0x07;
    public static final byte PSBT_OUT_PROPRIETARY = (byte)0xfc;

    private Script redeemScript;
    private Script witnessScript;
    private final Map<ECKey, KeyDerivation> derivedPublicKeys = new LinkedHashMap<>();
    private final Map<String, String> proprietary = new LinkedHashMap<>();
    private Map<ECKey, Map<KeyDerivation, List<Sha256Hash>>> tapDerivedPublicKeys = new LinkedHashMap<>();
    private ECKey tapInternalKey;

    PSBTOutput() {
        //empty constructor
    }

    PSBTOutput(ScriptType scriptType, Script redeemScript, Script witnessScript, Map<ECKey, KeyDerivation> derivedPublicKeys, Map<String, String> proprietary, ECKey tapInternalKey) {
        this.redeemScript = redeemScript;
        this.witnessScript = witnessScript;

        if(scriptType != P2TR) {
            this.derivedPublicKeys.putAll(derivedPublicKeys);
        }

        this.proprietary.putAll(proprietary);

        this.tapInternalKey = tapInternalKey == null ? null : ECKey.fromPublicOnly(tapInternalKey.getPubKeyXCoord());

        if(tapInternalKey != null && !derivedPublicKeys.values().isEmpty()) {
            KeyDerivation tapKeyDerivation = derivedPublicKeys.values().iterator().next();
            tapDerivedPublicKeys.put(this.tapInternalKey, Collections.singletonMap(tapKeyDerivation, Collections.emptyList()));
        }
    }

    PSBTOutput(List<PSBTEntry> outputEntries) throws PSBTParseException {
        for(PSBTEntry entry : outputEntries) {
            switch (entry.getKeyType()) {
                case PSBT_OUT_REDEEM_SCRIPT:
                    entry.checkOneByteKey();
                    Script redeemScript = new Script(entry.getData());
                    this.redeemScript = redeemScript;
                    break;
                case PSBT_OUT_WITNESS_SCRIPT:
                    entry.checkOneByteKey();
                    Script witnessScript = new Script(entry.getData());
                    this.witnessScript = witnessScript;
                    break;
                case PSBT_OUT_BIP32_DERIVATION:
                    entry.checkOneBytePlusPubKey();
                    ECKey derivedPublicKey = ECKey.fromPublicOnly(entry.getKeyData());
                    KeyDerivation keyDerivation = parseKeyDerivation(entry.getData());
                    this.derivedPublicKeys.put(derivedPublicKey, keyDerivation);
                    break;
                case PSBT_OUT_PROPRIETARY:
                    proprietary.put(Utils.bytesToHex(entry.getKeyData()), Utils.bytesToHex(entry.getData()));
                    break;
                case PSBT_OUT_TAP_INTERNAL_KEY:
                    entry.checkOneByteKey();
                    this.tapInternalKey = ECKey.fromPublicOnly(entry.getData());
                    break;
                case PSBT_OUT_TAP_BIP32_DERIVATION:
                    entry.checkOneBytePlusXOnlyPubKey();
                    ECKey tapPublicKey = ECKey.fromPublicOnly(entry.getKeyData());
                    Map<KeyDerivation, List<Sha256Hash>> tapKeyDerivations = parseTaprootKeyDerivation(entry.getData());
                    if (tapKeyDerivations.isEmpty()) {
                        break;
                    } else {
                        this.tapDerivedPublicKeys.put(tapPublicKey, tapKeyDerivations);
                    }
                    break;
                default:
                    break;
            }
        }
    }

    void combine(PSBTOutput psbtOutput) {
        if(psbtOutput.redeemScript != null) {
            redeemScript = psbtOutput.redeemScript;
        }

        if(psbtOutput.witnessScript != null) {
            witnessScript = psbtOutput.witnessScript;
        }

        derivedPublicKeys.putAll(psbtOutput.derivedPublicKeys);
        proprietary.putAll(psbtOutput.proprietary);

        tapDerivedPublicKeys.putAll(psbtOutput.tapDerivedPublicKeys);

        if(psbtOutput.tapInternalKey != null) {
            tapInternalKey = psbtOutput.tapInternalKey;
        }
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

    public Map<ECKey, KeyDerivation> getDerivedPublicKeys() {
        return derivedPublicKeys;
    }

    public Map<String, String> getProprietary() {
        return proprietary;
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

    public void clearNonFinalFields() {
        tapDerivedPublicKeys.clear();
    }
}
