package com.sparrowwallet.drongo.address;

import com.sparrowwallet.drongo.Network;
import com.sparrowwallet.drongo.Utils;
import com.sparrowwallet.drongo.protocol.ScriptType;

public class P2PKAddress extends Address {
    public P2PKAddress(byte[] pubKey) {
        super(pubKey);
    }

    @Override
    public int getVersion(Network network) {
        return network.getP2PKHAddressHeader();
    }

    @Override
    public String getAddress(Network network) {
        return Utils.bytesToHex(data);
    }

    public ScriptType getScriptType() {
        return ScriptType.P2PK;
    }

    @Override
    public String getOutputScriptDataType() {
        return "Public Key";
    }
}
