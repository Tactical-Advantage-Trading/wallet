package com.sparrowwallet.drongo.address;

import com.sparrowwallet.drongo.Network;
import com.sparrowwallet.drongo.protocol.Bech32;
import com.sparrowwallet.drongo.protocol.ScriptType;

public class P2TRAddress extends Address {
    public P2TRAddress(byte[] pubKey) {
        super(pubKey);
    }

    @Override
    public int getVersion(Network network) {
        return 1;
    }

    @Override
    public String getAddress(Network network) {
        return Bech32.encode(network.getBech32AddressHRP(), getVersion(), data);
    }

    @Override
    public ScriptType getScriptType() {
        return ScriptType.P2TR;
    }

    @Override
    public String getOutputScriptDataType() {
        return "Taproot";
    }
}
