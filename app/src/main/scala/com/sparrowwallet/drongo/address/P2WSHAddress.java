package com.sparrowwallet.drongo.address;

import com.sparrowwallet.drongo.Network;
import com.sparrowwallet.drongo.protocol.Bech32;
import com.sparrowwallet.drongo.protocol.ScriptType;
import com.sparrowwallet.drongo.protocol.Sha256Hash;

public class P2WSHAddress extends Address {
    public P2WSHAddress(byte[] scriptHash) {
        super(scriptHash);
    }

    @Override
    public int getVersion(Network network) {
        return 0;
    }

    @Override
    public String getAddress(Network network) {
        return Bech32.encode(network.getBech32AddressHRP(), getVersion(), data);
    }

    @Override
    public ScriptType getScriptType() {
        return ScriptType.P2WSH;
    }

    @Override
    public String getOutputScriptDataType() {
        return "Witness Script Hash";
    }

    public static P2WSHAddress fromProgram(byte[] program) {
        return new P2WSHAddress(Sha256Hash.hash(program));
    }
}
