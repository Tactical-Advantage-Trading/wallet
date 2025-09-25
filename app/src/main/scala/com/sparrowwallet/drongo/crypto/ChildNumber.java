package com.sparrowwallet.drongo.crypto;

public class ChildNumber {
    /**
     * The bit that's set in the child number to indicate whether this key is "hardened". Given a hardened key, it is
     * not possible to derive a child public key if you know only the hardened public key. With a non-hardened key this
     * is possible, so you can derive trees of public keys given only a public parent, but the downside is that it's
     * possible to leak private keys if you disclose a parent public key and a child private key (elliptic curve maths
     * allows you to work upwards).
     */
    public static final int HARDENED_BIT = 0x80000000;

    public static final ChildNumber ZERO = new ChildNumber(0);
    public static final ChildNumber ZERO_HARDENED = new ChildNumber(0, true);
    public static final ChildNumber ONE = new ChildNumber(1);
    public static final ChildNumber ONE_HARDENED = new ChildNumber(1, true);

    /** Integer i as per BIP 32 spec, including the MSB denoting derivation type (0 = public, 1 = private) **/
    private final int i;

    public ChildNumber(int childNumber, boolean isHardened) {
        if (hasHardenedBit(childNumber))
            throw new IllegalArgumentException("Most significant bit is reserved and shouldn't be set: " + childNumber);
        i = isHardened ? (childNumber | HARDENED_BIT) : childNumber;
    }

    public ChildNumber(int i) {
        this.i = i;
    }

    private static boolean hasHardenedBit(int a) {
        return (a & HARDENED_BIT) != 0;
    }

    public boolean isHardened() {
        return hasHardenedBit(i);
    }

    public int num() {
        return i & (~HARDENED_BIT);
    }

    /** Returns the uint32 encoded form of the path element, including the most significant bit. */
    public int i() { return i; }

    public String toString() {
        return toString(true);
    }

    public String toString(boolean useApostrophes) {
        return num() + (isHardened() ? (useApostrophes ? "'" : "h") : "");
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return i == ((ChildNumber)o).i;
    }

    public int hashCode() {
        return i;
    }
}
