package com.sparrowwallet.drongo.address;

import com.sparrowwallet.drongo.Network;
import com.sparrowwallet.drongo.Utils;
import com.sparrowwallet.drongo.protocol.ScriptType;

public class P2SHAddress extends Address {
    public P2SHAddress(byte[] scriptHash) {
        super(scriptHash);
    }

    @Override
    public int getVersion(Network network) {
        return network.getP2SHAddressHeader();
    }

    @Override
    public ScriptType getScriptType() {
        return ScriptType.P2SH;
    }

    @Override
    public String getOutputScriptDataType() {
        return "Script Hash";
    }

    public static P2SHAddress fromProgram(byte[] program) {
        return new P2SHAddress(Utils.sha256hash160(program));
    }
}
