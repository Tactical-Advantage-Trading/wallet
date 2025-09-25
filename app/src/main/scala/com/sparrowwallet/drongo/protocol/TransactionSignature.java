package com.sparrowwallet.drongo.protocol;

import com.sparrowwallet.drongo.crypto.ECDSASignature;
import com.sparrowwallet.drongo.crypto.ECKey;
import com.sparrowwallet.drongo.crypto.SchnorrSignature;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.util.Objects;

public class TransactionSignature {
    private final ECDSASignature ecdsaSignature;
    private final SchnorrSignature schnorrSignature;

    /**
     * A byte that controls which parts of a transaction are signed. This is exposed because signatures
     * parsed off the wire may have sighash flags that aren't "normal" serializations of the enum values.
     * Because Bitcoin Core works via bit testing, we must not lose the exact value when round-tripping
     * otherwise we'll fail to verify signature hashes.
     */
    public final byte sighashFlags;

    /** Constructs a signature with the given components of the given type and SIGHASH_ALL. */
    public TransactionSignature(BigInteger r, BigInteger s, Type type) {
        this(r, s, type, type == Type.ECDSA ? SigHash.ALL.value : SigHash.DEFAULT.value);
    }

    /** Constructs a transaction signature based on the ECDSA signature. */
    public TransactionSignature(ECDSASignature signature, SigHash sigHash) {
        this(signature.r, signature.s, Type.ECDSA, sigHash.value);
    }

    /** Constructs a transaction signature based on the Schnorr signature. */
    public TransactionSignature(SchnorrSignature signature, SigHash sigHash) {
        this(signature.r, signature.s, Type.SCHNORR, sigHash.value);
    }

    /** Constructs a signature with the given components, type and raw sighash flag bytes (needed for rule compatibility). */
    public TransactionSignature(BigInteger r, BigInteger s, Type type, byte sighashFlags) {
        ecdsaSignature = type == Type.ECDSA ? new ECDSASignature(r, s) : null;
        schnorrSignature = type == Type.SCHNORR ? new SchnorrSignature(r, s) : null;
        this.sighashFlags = sighashFlags;
    }

    /**
     * Returns a dummy invalid signature whose R/S values are set such that they will take up the same number of
     * encoded bytes as a real signature. This can be useful when you want to fill out a transaction to be of the
     * right size (e.g. for fee calculations) but don't have the requisite signing key yet and will fill out the
     * real signature later.
     */
    public static TransactionSignature dummy(Type type) {
        BigInteger val = ECKey.HALF_CURVE_ORDER;
        return new TransactionSignature(val, val, type);
    }

    public boolean anyoneCanPay() {
        return (sighashFlags & SigHash.ANYONECANPAY.value) != 0;
    }

    private SigHash getSigHash() {
        if(sighashFlags == SigHash.DEFAULT.byteValue()) {
            return SigHash.DEFAULT;
        }

        boolean anyoneCanPay = anyoneCanPay();
        final int mode = sighashFlags & 0x1f;
        if (mode == SigHash.NONE.value) {
            return anyoneCanPay ? SigHash.ANYONECANPAY_NONE : SigHash.NONE;
        } else if (mode == SigHash.SINGLE.value) {
            return anyoneCanPay ? SigHash.ANYONECANPAY_SINGLE : SigHash.SINGLE;
        } else {
            return anyoneCanPay ? SigHash.ANYONECANPAY_ALL : SigHash.ALL;
        }
    }

    /**
     * What we get back from the signer are the two components of a signature, r and s. To get a flat byte stream
     * of the type used by Bitcoin we have to encode them using DER encoding, which is just a way to pack the two
     * components into a structure, and then we append a byte to the end for the sighash flags.
     */
    public byte[] encodeToBitcoin() {
        if(ecdsaSignature != null) {
            try {
                ByteArrayOutputStream bos = ecdsaSignature.derByteStream();
                bos.write(sighashFlags);
                return bos.toByteArray();
            } catch (IOException e) {
                throw new RuntimeException(e);  // Cannot happen.
            }
        } else if(schnorrSignature != null) {
            SigHash sigHash = getSigHash();
            ByteBuffer buffer = ByteBuffer.allocate(sigHash == SigHash.DEFAULT ? 64 : 65);
            buffer.put(schnorrSignature.encode());
            if(sigHash != SigHash.DEFAULT) {
                buffer.put(sighashFlags);
            }
            return buffer.array();
        }

        throw new IllegalStateException("TransactionSignature has no values");
    }

    public static TransactionSignature decodeFromBitcoin(byte[] bytes, boolean requireCanonicalEncoding) throws SignatureDecodeException {
        if(bytes.length == 64 || bytes.length == 65) {
            return decodeFromBitcoin(Type.SCHNORR, bytes, requireCanonicalEncoding);
        }

        return decodeFromBitcoin(Type.ECDSA, bytes, requireCanonicalEncoding);
    }

    public static TransactionSignature decodeFromBitcoin(Type type, byte[] bytes, boolean requireCanonicalEncoding) throws SignatureDecodeException {
        if(type == Type.ECDSA) {
            return ECDSASignature.decodeFromBitcoin(bytes, requireCanonicalEncoding, false);
        } else if(type == Type.SCHNORR) {
            return SchnorrSignature.decodeFromBitcoin(bytes);
        }

        throw new IllegalStateException("Unknown TransactionSignature type " + type);
    }

    public boolean verify(byte[] data, ECKey pubKey) {
        if(ecdsaSignature != null) {
            return ecdsaSignature.verify(data, pubKey.getPubKey());
        } else {
            return schnorrSignature.verify(data, pubKey.getPubKeyXCoord());
        }
    }

    @Override
    public boolean equals(Object o) {
        if(this == o) {
            return true;
        }
        if(o == null || getClass() != o.getClass()) {
            return false;
        }
        TransactionSignature that = (TransactionSignature) o;
        return sighashFlags == that.sighashFlags && Objects.equals(ecdsaSignature, that.ecdsaSignature) && Objects.equals(schnorrSignature, that.schnorrSignature);
    }

    @Override
    public int hashCode() {
        return Objects.hash(ecdsaSignature, schnorrSignature, sighashFlags);
    }

    public enum Type {
        ECDSA, SCHNORR
    }
}