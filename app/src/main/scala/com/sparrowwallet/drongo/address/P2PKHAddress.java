package com.sparrowwallet.drongo.address;

import com.sparrowwallet.drongo.Network;
import com.sparrowwallet.drongo.protocol.ScriptType;

public class P2PKHAddress extends Address {
    public P2PKHAddress(byte[] pubKeyHash) {
        super(pubKeyHash);
    }

    @Override
    public int getVersion(Network network) {
        return network.getP2PKHAddressHeader();
    }

    @Override
    public ScriptType getScriptType() {
        return ScriptType.P2PKH;
    }

    @Override
    public String getOutputScriptDataType() {
        return "Public Key Hash";
    }
}
