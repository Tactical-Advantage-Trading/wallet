package com.sparrowwallet.drongo.crypto;

import com.sparrowwallet.drongo.Utils;
import com.sparrowwallet.drongo.protocol.TransactionSignature;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.Objects;

/**
 * Groups the two components that make up a Schnorr signature
 */
public class SchnorrSignature {
    /**
     * The two components of the signature.
     */
    public final BigInteger r, s;

    /**
     * Constructs a signature with the given components. Does NOT automatically canonicalise the signature.
     */
    public SchnorrSignature(BigInteger r, BigInteger s) {
        this.r = r;
        this.s = s;
    }

    public byte[] encode() {
        ByteBuffer buffer = ByteBuffer.allocate(64);
        buffer.put(Utils.bigIntegerToBytes(r, 32));
        buffer.put(Utils.bigIntegerToBytes(s, 32));
        return buffer.array();
    }

    public static SchnorrSignature decode(byte[] bytes) {
        if(bytes.length != 64) {
            throw new IllegalArgumentException("Invalid Schnorr signature length of " + bytes.length + " bytes");
        }

        BigInteger r = new BigInteger(1, Arrays.copyOfRange(bytes, 0, 32));
        BigInteger s = new BigInteger(1, Arrays.copyOfRange(bytes, 32, 64));

        return new SchnorrSignature(r, s);
    }

    public static TransactionSignature decodeFromBitcoin(byte[] bytes) {
        if(bytes.length < 64 || bytes.length > 65) {
            throw new IllegalArgumentException("Invalid Schnorr signature length of " + bytes.length + " bytes");
        }

        BigInteger r = new BigInteger(1, Arrays.copyOfRange(bytes, 0, 32));
        BigInteger s = new BigInteger(1, Arrays.copyOfRange(bytes, 32, 64));

        if(bytes.length == 65) {
            return new TransactionSignature(r, s, TransactionSignature.Type.SCHNORR, bytes[64]);
        }

        return new TransactionSignature(r, s, TransactionSignature.Type.SCHNORR, (byte)0);
    }

    public boolean verify(byte[] data, byte[] pub) {
        throw new IllegalStateException("libsecp256k1 is not enabled");
    }

    @Override
    public boolean equals(Object o) {
        if(this == o) {
            return true;
        }
        if(o == null || getClass() != o.getClass()) {
            return false;
        }
        SchnorrSignature that = (SchnorrSignature) o;
        return r.equals(that.r) && s.equals(that.s);
    }

    @Override
    public int hashCode() {
        return Objects.hash(r, s);
    }
}
