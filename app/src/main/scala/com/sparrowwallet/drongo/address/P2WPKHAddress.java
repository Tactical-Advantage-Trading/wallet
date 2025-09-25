package com.sparrowwallet.drongo.address;

import com.sparrowwallet.drongo.Network;
import com.sparrowwallet.drongo.protocol.Bech32;
import com.sparrowwallet.drongo.protocol.ScriptType;

public class P2WPKHAddress extends Address {
    public P2WPKHAddress(byte[] pubKeyHash) {
        super(pubKeyHash);
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
        return ScriptType.P2WPKH;
    }

    @Override
    public String getOutputScriptDataType() {
        return "Witness Public Key Hash";
    }
}
