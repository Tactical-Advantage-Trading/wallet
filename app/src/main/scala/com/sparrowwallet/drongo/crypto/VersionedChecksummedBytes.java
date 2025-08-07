package com.sparrowwallet.drongo.crypto;


import com.sparrowwallet.drongo.Utils;
import com.sparrowwallet.drongo.protocol.Base58;
import com.sparrowwallet.drongo.protocol.Sha256Hash;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Objects;

/**
 * <p>In Bitcoin the following format is often used to represent some type of key:</p>
 * <p/>
 * <pre>[one version byte] [data bytes] [4 checksum bytes]</pre>
 * <p/>
 * <p>and the result is then Base58 encoded. This format is used for addresses, and private keys exported using the
 * dumpprivkey command.</p>
 */
public class VersionedChecksummedBytes implements Serializable, Cloneable, Comparable<VersionedChecksummedBytes> {
    protected final int version;
    protected byte[] bytes;

    protected VersionedChecksummedBytes(String encoded) {
        byte[] versionAndDataBytes = Base58.decodeChecked(encoded);
        byte versionByte = versionAndDataBytes[0];
        version = versionByte & 0xFF;
        bytes = new byte[versionAndDataBytes.length - 1];
        System.arraycopy(versionAndDataBytes, 1, bytes, 0, versionAndDataBytes.length - 1);
    }

    protected VersionedChecksummedBytes(int version, byte[] bytes) {
        this.version = version;
        this.bytes = bytes;
    }

    /**
     * Returns the base-58 encoded String representation of this
     * object, including version and checksum bytes.
     */
    public final String toBase58() {
        // A stringified buffer is:
        //   1 byte version + data bytes + 4 bytes check code (a truncated hash)
        byte[] addressBytes = new byte[1 + bytes.length + 4];
        addressBytes[0] = (byte) version;
        System.arraycopy(bytes, 0, addressBytes, 1, bytes.length);
        byte[] checksum = Sha256Hash.hashTwice(addressBytes, 0, bytes.length + 1);
        System.arraycopy(checksum, 0, addressBytes, bytes.length + 1, 4);
        return Base58.encode(addressBytes);
    }

    @Override
    public String toString() {
        return toBase58();
    }

    @Override
    public boolean equals(Object o) {
        if(this == o) {
            return true;
        }
        if(o == null || getClass() != o.getClass()) {
            return false;
        }
        VersionedChecksummedBytes that = (VersionedChecksummedBytes) o;
        return version == that.version && Arrays.equals(bytes, that.bytes);
    }

    @Override
    public int hashCode() {
        int result = Objects.hash(version);
        result = 31 * result + Arrays.hashCode(bytes);
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * This implementation narrows the return type to <code>VersionedChecksummedBytes</code>
     * and allows subclasses to throw <code>CloneNotSupportedException</code> even though it
     * is never thrown by this implementation.
     */
    @Override
    public VersionedChecksummedBytes clone() throws CloneNotSupportedException {
        return (VersionedChecksummedBytes) super.clone();
    }

    /**
     * {@inheritDoc}
     *
     * This implementation uses an optimized Google Guava method to compare <code>bytes</code>.
     */
    @Override
    public int compareTo(VersionedChecksummedBytes o) {
        int result =  Integer.compare(this.version, o.version);
        Utils.LexicographicByteArrayComparator lexicographicByteArrayComparator = new Utils.LexicographicByteArrayComparator();
        return result != 0 ? result : lexicographicByteArrayComparator.compare(this.bytes, o.bytes);
    }

    /**
     * Returns the "version" or "header" byte: the first byte of the data. This is used to disambiguate what the
     * contents apply to, for example, which network the key or address is valid on.
     *
     * @return A positive number between 0 and 255.
     */
    public int getVersion() {
        return version;
    }
}
