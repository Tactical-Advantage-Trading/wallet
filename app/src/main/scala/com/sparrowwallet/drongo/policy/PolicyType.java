package com.sparrowwallet.drongo.policy;

import com.sparrowwallet.drongo.protocol.ScriptType;

import static com.sparrowwallet.drongo.protocol.ScriptType.P2WPKH;
import static com.sparrowwallet.drongo.protocol.ScriptType.P2WSH;

public enum PolicyType {
    SINGLE("Single Signature", P2WPKH), MULTI("Multi Signature", P2WSH), CUSTOM("Custom", P2WSH);

    private String name;
    private ScriptType defaultScriptType;

    PolicyType(String name, ScriptType defaultScriptType) {
        this.name = name;
        this.defaultScriptType = defaultScriptType;
    }

    public String getName() {
        return name;
    }

    public ScriptType getDefaultScriptType() {
        return defaultScriptType;
    }

    @Override
    public String toString() {
        return name;
    }
}
