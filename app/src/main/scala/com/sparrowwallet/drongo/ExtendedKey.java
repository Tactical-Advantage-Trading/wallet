package com.sparrowwallet.drongo;

import com.sparrowwallet.drongo.crypto.ChildNumber;
import com.sparrowwallet.drongo.crypto.DeterministicHierarchy;
import com.sparrowwallet.drongo.crypto.DeterministicKey;
import com.sparrowwallet.drongo.crypto.ECKey;
import com.sparrowwallet.drongo.crypto.HDKeyDerivation;
import com.sparrowwallet.drongo.crypto.LazyECPoint;
import com.sparrowwallet.drongo.protocol.Base58;
import com.sparrowwallet.drongo.protocol.ScriptType;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class ExtendedKey {
    private final byte[] parentFingerprint;
    private final DeterministicKey key;
    private final ChildNumber keyChildNumber;
    private final DeterministicHierarchy hierarchy;

    public ExtendedKey(DeterministicKey key, byte[] parentFingerprint, ChildNumber keyChildNumber) {
        this.parentFingerprint = parentFingerprint;
        this.key = key;
        this.keyChildNumber = keyChildNumber;
        this.hierarchy = new DeterministicHierarchy(key);
    }

    public byte[] getParentFingerprint() {
        return parentFingerprint;
    }

    public DeterministicKey getKey() {
        return key;
    }

    public DeterministicKey getKey(List<ChildNumber> path) {
        return hierarchy.get(path);
    }

    public String toString() {
        return getExtendedKey();
    }

    public String toString(Header extendedKeyHeader) {
        return getExtendedKey(extendedKeyHeader);
    }

    public String getExtendedKey() {
        return Base58.encodeChecked(getExtendedKeyBytes());
    }

    public String getExtendedKey(Header extendedKeyHeader) {
        return Base58.encodeChecked(getExtendedKeyBytes(extendedKeyHeader));
    }

    public ChildNumber getKeyChildNumber() {
        return keyChildNumber;
    }

    public byte[] getExtendedKeyBytes() {
        return getExtendedKeyBytes(key.isPubKeyOnly() ? Network.get().getXpubHeader() : Network.get().getXprvHeader());
    }

    public byte[] getExtendedKeyBytes(Header extendedKeyHeader) {
        ByteBuffer buffer = ByteBuffer.allocate(78);
        buffer.putInt(extendedKeyHeader.header);
        buffer.put((byte) key.getDepth());
        buffer.put(parentFingerprint);
        buffer.putInt(keyChildNumber.i());
        buffer.put(key.getChainCode());
        if(key.isPubKeyOnly()) {
            buffer.put(key.getPubKey());
        } else {
            buffer.put((byte)0);
            buffer.put(key.getPrivKeyBytes());
        }

        return buffer.array();
    }

    public static ExtendedKey fromDescriptor(String descriptor) {
        byte[] serializedKey = Base58.decodeChecked(descriptor);
        ByteBuffer buffer = ByteBuffer.wrap(serializedKey);
        int headerInt = buffer.getInt();
        Header header = Header.getHeader(headerInt);
        if(header == null) {
            throw new IllegalArgumentException("Unknown header bytes for extended key on " + Network.get().getName() + ": " + DeterministicKey.toBase58(serializedKey).substring(0, 4));
        }

        int depth = buffer.get() & 0xFF; // convert signed byte to positive int since depth cannot be negative
        byte[] parentFingerprint = new byte[4];
        buffer.get(parentFingerprint);
        final int i = buffer.getInt();
        ChildNumber childNumber;
        List<ChildNumber> path;

        if(depth == 0 && !header.isPrivateKey()) {
            //Poorly formatted public extended key, add first child path element
            childNumber = new ChildNumber(0, false);
        } else if ((i & ChildNumber.HARDENED_BIT) != 0) {
            childNumber = new ChildNumber(i ^ ChildNumber.HARDENED_BIT, true); //already hardened
        } else {
            childNumber = new ChildNumber(i, false);
        }
        path = Collections.singletonList(childNumber);

        byte[] chainCode = new byte[32];
        buffer.get(chainCode);
        byte[] data = new byte[33];
        buffer.get(data);
        if(buffer.hasRemaining()) {
            throw new IllegalArgumentException("Found unexpected data in key");
        }

        if(header.isPrivateKey()) {
            DeterministicKey prvKey = HDKeyDerivation.createMasterPrivKeyFromBytes(Arrays.copyOfRange(data, 1, 33), chainCode, path);
            return new ExtendedKey(prvKey, parentFingerprint, childNumber);
        } else {
            DeterministicKey pubKey = new DeterministicKey(path, chainCode, new LazyECPoint(ECKey.CURVE.getCurve(), data), depth, parentFingerprint);
            return new ExtendedKey(pubKey, parentFingerprint, childNumber);
        }
    }

    public static boolean isValid(String extPubKey) {
        try {
            ExtendedKey.fromDescriptor(extPubKey);
        } catch (Exception e) {
            return false;
        }

        return true;
    }

    public ExtendedKey copy() {
        //DeterministicKey is effectively final
        return new ExtendedKey(key, Arrays.copyOf(parentFingerprint, parentFingerprint.length), keyChildNumber);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ExtendedKey that = (ExtendedKey) o;
        return that.toString().equals(this.toString());
    }

    @Override
    public int hashCode() {
        return toString().hashCode();
    }

    public enum Header {
        xprv("xprv", 0x0488ADE4, ScriptType.P2PKH, true, true),
        xpub("xpub", 0x0488B21E, ScriptType.P2PKH, false, true),
        yprv("yprv", 0x049D7878, ScriptType.P2SH_P2WPKH, true, true),
        ypub("ypub", 0x049D7CB2, ScriptType.P2SH_P2WPKH, false, true),
        zprv("zprv", 0x04b2430c, ScriptType.P2WPKH, true, true),
        zpub("zpub", 0x04B24746, ScriptType.P2WPKH, false, true),
        Yprv("Yprv", 0x0295b005, ScriptType.P2SH_P2WSH, true, true),
        Ypub("Ypub", 0x0295b43f, ScriptType.P2SH_P2WSH, false, true),
        Zprv("Zprv", 0x02aa7a99, ScriptType.P2WSH, true, true),
        Zpub("Zpub", 0x02aa7ed3, ScriptType.P2WSH, false, true),
        tprv("tprv", 0x04358394, ScriptType.P2PKH, true, false),
        tpub("tpub", 0x043587cf, ScriptType.P2PKH, false, false),
        uprv("uprv", 0x044a4e28, ScriptType.P2SH_P2WPKH, true, false),
        upub("upub", 0x044a5262, ScriptType.P2SH_P2WPKH, false, false),
        vprv("vprv", 0x045f18bc, ScriptType.P2WPKH, true, false),
        vpub("vpub", 0x045f1cf6, ScriptType.P2WPKH, false, false),
        Uprv("Uprv", 0x024285b5, ScriptType.P2SH_P2WSH, true, false),
        Upub("Upub", 0x024289ef, ScriptType.P2SH_P2WSH, false, false),
        Vprv("Vprv", 0x02575048, ScriptType.P2WSH, true, false),
        Vpub("Vpub", 0x02575483, ScriptType.P2WSH, false, false);

        private final String name;
        private final int header;
        private final ScriptType defaultScriptType;
        private final boolean privateKey;
        private final boolean mainnet;

        Header(String name, int header, ScriptType defaultScriptType, boolean privateKey, boolean mainnet) {
            this.name = name;
            this.header = header;
            this.defaultScriptType = defaultScriptType;
            this.privateKey = privateKey;
            this.mainnet = mainnet;
        }

        public String getName() {
            return name;
        }

        public String getDisplayName() {
            return name;
        }

        public int getHeader() {
            return header;
        }

        public ScriptType getDefaultScriptType() {
            return defaultScriptType;
        }

        public boolean isPrivateKey() {
            return privateKey;
        }

        public Network getNetwork() {
            return mainnet ? Network.MAINNET : Network.TESTNET;
        }

        public static List<Header> getHeaders(Network network) {
            List<Header> filteredHeaders = new ArrayList<>();
            for (Header header : Header.values()) {
                if (header.getNetwork() == network
                        || (header.getNetwork() == Network.TESTNET && network == Network.REGTEST)
                        || (header.getNetwork() == Network.TESTNET && network == Network.SIGNET)) {
                    filteredHeaders.add(header);
                }
            }
            return filteredHeaders;
        }

        public static Header fromExtendedKey(String xkey) {
            for(Header extendedKeyHeader : getHeaders(Network.get())) {
                if(xkey.startsWith(extendedKeyHeader.name)) {
                    return extendedKeyHeader;
                }
            }

            for(Network network : getOtherNetworks(Network.get())) {
                for(Header otherNetworkKeyHeader : getHeaders(network)) {
                    if(xkey.startsWith(otherNetworkKeyHeader.name)) {
                        throw new IllegalArgumentException("Provided " + otherNetworkKeyHeader.name + " extended key invalid on configured " + Network.get().getName() + " network. Use a " + network.getName() + " configuration to use this extended key.");
                    }
                }
            }

            throw new IllegalArgumentException("Unrecognised extended key header for " + Network.get().getName() + ": " + xkey);
        }

        public static Header fromScriptType(ScriptType scriptType, boolean privateKey) {
            for(Header header : getHeaders(Network.get())) {
                if(header.defaultScriptType != null && header.defaultScriptType.equals(scriptType) && header.isPrivateKey() == privateKey) {
                    return header;
                }
            }

            return Network.get().getXpubHeader();
        }

        private static Header getHeader(int header) {
            for(Header extendedKeyHeader : getHeaders(Network.get())) {
                if(header == extendedKeyHeader.header) {
                    return extendedKeyHeader;
                }
            }

            for(Network otherNetwork : getOtherNetworks(Network.get())) {
                for(Header otherNetworkKeyHeader : getHeaders(otherNetwork)) {
                    if(header == otherNetworkKeyHeader.header) {
                        throw new IllegalArgumentException("Provided " + otherNetworkKeyHeader.name + " extended key invalid on configured " + Network.get().getName() + " network. Use a " + otherNetwork.getName() + " configuration to use this extended key.");
                    }
                }
            }

            return null;
        }

        private static List<Network> getOtherNetworks(Network providedNetwork) {
            List<Network> otherNetworks = new ArrayList<>();
            for (Network network : Network.values()) {
                if (network != providedNetwork) {
                    otherNetworks.add(network);
                }
            }
            return otherNetworks;
        }
    }
}