package com.sparrowwallet.drongo.crypto;


import com.sparrowwallet.drongo.Network;

import java.util.Arrays;
import java.util.Objects;

/**
 * Parses and generates private keys in the form used by the Bitcoin "dumpprivkey" command. This is the private key
 * bytes with a header byte and 4 checksum bytes at the end. If there are 33 private key bytes instead of 32, then
 * the last byte is a discriminator value for the compressed pubkey.
 */
public class DumpedPrivateKey extends VersionedChecksummedBytes {
    private final boolean compressed;

    /**
     * Construct a private key from its Base58 representation.
     * @param base58
     *            The textual form of the private key.
     */
    public static DumpedPrivateKey fromBase58(String base58) {
        return new DumpedPrivateKey(base58);
    }

    // Used by ECKey.getPrivateKeyEncoded()
    DumpedPrivateKey(byte[] keyBytes, boolean compressed) {
        super(Network.get().getDumpedPrivateKeyHeader(), encode(keyBytes, compressed));
        this.compressed = compressed;
    }

    private static byte[] encode(byte[] keyBytes, boolean compressed) {
        if(keyBytes.length != 32) {
            throw new IllegalArgumentException("Private keys must be 32 bytes");
        }

        if (!compressed) {
            return keyBytes;
        } else {
            // Keys that have compressed public components have an extra 1 byte on the end in dumped form.
            byte[] bytes = new byte[33];
            System.arraycopy(keyBytes, 0, bytes, 0, 32);
            bytes[32] = 1;
            return bytes;
        }
    }

    private DumpedPrivateKey(String encoded) {
        super(encoded);
        if(version != Network.get().getDumpedPrivateKeyHeader())
            throw new IllegalArgumentException("Invalid version " + version + " for network " + Network.get());
        if(bytes.length == 33 && bytes[32] == 1) {
            compressed = true;
            bytes = Arrays.copyOf(bytes, 32);  // Chop off the additional marker byte.
        } else if(bytes.length == 32) {
            compressed = false;
        } else {
            throw new IllegalArgumentException("Wrong number of bytes for a private key, not 32 or 33");
        }
    }

    /**
     * Returns an ECKey created from this encoded private key.
     */
    public ECKey getKey() {
        return ECKey.fromPrivate(bytes, compressed);
    }

    @Override
    public boolean equals(Object o) {
        if(this == o) {
            return true;
        }
        if(o == null || getClass() != o.getClass()) {
            return false;
        }
        if(!super.equals(o)) {
            return false;
        }
        DumpedPrivateKey that = (DumpedPrivateKey) o;
        return compressed == that.compressed;
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), compressed);
    }
}