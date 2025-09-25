package com.sparrowwallet.drongo.crypto;


import com.sparrowwallet.drongo.Utils;
import com.sparrowwallet.drongo.protocol.Base58;
import com.sparrowwallet.drongo.protocol.Sha256Hash;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class DeterministicKey extends ECKey {
    private final DeterministicKey parent;
    private final List<ChildNumber> childNumberPath;
    private int depth;
    private byte[] parentFingerprint; // 0 if this key is root node of key hierarchy

    /** 32 bytes */
    private final byte[] chainCode;

    /**
     * Constructs a key from its components, including its public key data and possibly-redundant
     * information about its parent key.  Invoked when deserializing, but otherwise not something that
     * you normally should use.
     */
    public DeterministicKey(List<ChildNumber> childNumberPath,
                            byte[] chainCode,
                            LazyECPoint publicAsPoint,
                            int depth,
                            byte[] parentFingerprint) {
        super(null, compressPoint(publicAsPoint));
        if(chainCode.length != 32) {
            throw new IllegalArgumentException("Chaincode not 32 bytes in length");
        }
        this.parent = null;
        this.childNumberPath = childNumberPath;
        this.chainCode = Arrays.copyOf(chainCode, chainCode.length);
        this.depth = depth;
        this.parentFingerprint = parentFingerprint;
    }

    public DeterministicKey(List<ChildNumber> childNumberPath,
                            byte[] chainCode,
                            byte[] publicKey,
                            int depth,
                            byte[] parentFingerprint) {
        this(childNumberPath, chainCode, new LazyECPoint(ECKey.CURVE.getCurve(), publicKey), depth, parentFingerprint);
    }

    public DeterministicKey(List<ChildNumber> childNumberPath,
                            byte[] chainCode,
                            LazyECPoint publicAsPoint,
                            BigInteger priv,
                            DeterministicKey parent) {
        super(priv, compressPoint(publicAsPoint));
        if(chainCode.length != 32) {
            throw new IllegalArgumentException("Chaincode not 32 bytes in length");
        }
        this.parent = parent;
        this.childNumberPath = childNumberPath;
        this.chainCode = Arrays.copyOf(chainCode, chainCode.length);
        this.depth = parent == null ? 0 : parent.depth + 1;
        this.parentFingerprint = (parent != null) ? parent.getFingerprint() : new byte[4];
    }

    public DeterministicKey(List<ChildNumber> childNumberPath,
                            byte[] chainCode,
                            BigInteger priv,
                            DeterministicKey parent) {
        super(priv, ECKey.publicPointFromPrivate(priv), true);
        this.parent = parent;
        this.childNumberPath = childNumberPath;
        this.chainCode = Arrays.copyOf(chainCode, chainCode.length);
        this.depth = parent == null ? 0 : parent.depth + 1;
        this.parentFingerprint = (parent != null) ? parent.getFingerprint() : new byte[4];
    }

    /**
     * Return this key's depth in the hierarchy, where the root node is at depth zero.
     * This may be different than the number of segments in the path if this key was
     * deserialized without access to its parent.
     */
    public int getDepth() {
        return depth;
    }

    /** Returns the first 32 bits of the result of {@link #getIdentifier()}. */
    public byte[] getFingerprint() {
        return Arrays.copyOfRange(getIdentifier(), 0, 4);
    }

    /**
     * Returns RIPE-MD160(SHA256(pub key bytes)).
     */
    public byte[] getIdentifier() {
        return Utils.sha256hash160(getPubKey());
    }

    /**
     * Returns the path through some DeterministicHierarchy which reaches this keys position in the tree.
     * A path can be written as 0/1/0 which means the first child of the root, the second child of that node, then
     * the first child of that node.
     */
    public List<ChildNumber> getPath() {
        return Collections.unmodifiableList(childNumberPath);
    }

    public DeterministicKey getParent() {
        return parent;
    }

    /**
     * Return the fingerprint of the key from which this key was derived, if this is a
     * child key, or else an array of four zero-value bytes.
     */
    public byte[] getParentFingerprint() {
        return parentFingerprint;
    }

    /**
     * Returns private key bytes, padded with zeros to 33 bytes.
     * @throws java.lang.IllegalStateException if the private key bytes are missing.
     */
    public byte[] getPrivKeyBytes33() {
        byte[] bytes33 = new byte[33];
        byte[] priv = getPrivKeyBytes();
        System.arraycopy(priv, 0, bytes33, 33 - priv.length, priv.length);
        return bytes33;
    }
    /**
     * Returns the same key with the private bytes removed. May return the same instance. The purpose of this is to save
     * memory: the private key can always be very efficiently rederived from a parent that a private key, so storing
     * all the private keys in RAM is a poor tradeoff especially on constrained devices. This means that the returned
     * key may still be usable for signing and so on, so don't expect it to be a true pubkey-only object! If you want
     * that then you should follow this call with a call to {@link #dropParent()}.
     */
    public DeterministicKey dropPrivateBytes() {
        if (isPubKeyOnly()) {
            return this;
        } else {
            return new DeterministicKey(getPath(), getChainCode(), pub, null, parent);
        }
    }

    /**
     * <p>Returns the same key with the parent pointer removed (it still knows its own path and the parent fingerprint).</p>
     *
     * <p>If this key doesn't have private key bytes stored/cached itself, but could rederive them from the parent, then
     * the new key returned by this method won't be able to do that. Thus, using dropPrivateBytes().dropParent() on a
     * regular DeterministicKey will yield a new DeterministicKey that cannot sign or do other things involving the
     * private key at all.</p>
     */
    public DeterministicKey dropParent() {
        DeterministicKey key = new DeterministicKey(getPath(), getChainCode(), pub, priv, null);
        key.parentFingerprint = parentFingerprint;
        key.depth = depth;
        return key;
    }

    /** Returns the last element of the path returned by {@link DeterministicKey#getPath()} */
    public ChildNumber getChildNumber() {
        return childNumberPath.size() == 0 ? ChildNumber.ZERO : childNumberPath.get(childNumberPath.size() - 1);
    }

    public byte[] getChainCode() {
        return chainCode;
    }

    public static String toBase58(byte[] ser) {
        return Base58.encode(addChecksum(ser));
    }

    static byte[] addChecksum(byte[] input) {
        int inputLength = input.length;
        byte[] checksummed = new byte[inputLength + 4];
        System.arraycopy(input, 0, checksummed, 0, inputLength);
        byte[] checksum = Sha256Hash.hashTwice(input);
        System.arraycopy(checksum, 0, checksummed, inputLength, 4);
        return checksummed;
    }
}
