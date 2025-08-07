package com.sparrowwallet.drongo.crypto;

import org.bouncycastle.math.ec.ECCurve;
import org.bouncycastle.math.ec.ECPoint;
import org.bouncycastle.util.encoders.Hex;

import java.util.Arrays;

public class LazyECPoint {
    // If curve is set, bits is also set. If curve is unset, point is set and bits is unset. Point can be set along
    // with curve and bits when the cached form has been accessed and thus must have been converted.

    private final ECCurve curve;
    private final byte[] bits;
    private final boolean compressed;

    // This field is effectively final - once set it won't change again. However it can be set after
    // construction.
    private ECPoint point;

    public LazyECPoint(ECCurve curve, byte[] bits) {
        this.curve = curve;
        this.bits = (bits != null && bits.length == 32 ? addYCoord(bits) : bits);
        this.compressed = ECKey.isPubKeyCompressed(bits);
    }

    public LazyECPoint(ECPoint point, boolean compressed) {
        this.point = point;
        this.compressed = compressed;
        this.curve = null;
        this.bits = null;
    }

    public ECPoint get() {
        if (point == null)
            point = curve.decodePoint(bits);
        return point;
    }

    // Delegated methods.

    public ECPoint getDetachedPoint() {
        return get().getDetachedPoint();
    }

    public boolean isCompressed() {
        return compressed;
    }

    public byte[] getEncoded() {
        if (bits != null)
            return Arrays.copyOf(bits, bits.length);
        else
            return get().getEncoded(compressed);
    }

    public byte[] getEncoded(boolean compressed) {
        if (compressed == isCompressed() && bits != null)
            return Arrays.copyOf(bits, bits.length);
        else
            return get().getEncoded(compressed);
    }

    public byte[] getEncodedXCoord() {
        byte[] compressed = getEncoded(true);
        byte[] xcoord = new byte[32];
        System.arraycopy(compressed, 1, xcoord, 0, 32);
        return xcoord;
    }

    public String toString() {
        return Hex.toHexString(getEncoded());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return Arrays.equals(getCanonicalEncoding(), ((LazyECPoint)o).getCanonicalEncoding());
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(getCanonicalEncoding());
    }

    private byte[] getCanonicalEncoding() {
        return getEncoded(true);
    }

    private static byte[] addYCoord(byte[] xcoord) {
        byte[] compressed = new byte[33];
        compressed[0] = 0x02;
        System.arraycopy(xcoord, 0, compressed, 1, 32);
        return compressed;
    }
}
