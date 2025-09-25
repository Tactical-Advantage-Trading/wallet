package com.sparrowwallet.drongo.protocol;

import com.sparrowwallet.drongo.Utils;
import com.sparrowwallet.drongo.address.Address;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Objects;

public class TransactionOutPoint extends ChildMessage {

    static final int MESSAGE_LENGTH = 36;

    /** Hash of the transaction to which we refer. */
    private Sha256Hash hash;
    /** Which output of that transaction we are talking about. */
    private long index;

    private Address[] addresses = new Address[0];

    public TransactionOutPoint(Sha256Hash hash, long index) {
        this.hash = hash;
        this.index = index;
        length = MESSAGE_LENGTH;
    }

    public TransactionOutPoint(byte[] rawtx, int offset, Message parent) {
        super(rawtx, offset);
        setParent(parent);
    }

    protected void parse() throws ProtocolException {
        length = MESSAGE_LENGTH;
        hash = readHash();
        index = readUint32();
    }

    public Sha256Hash getHash() {
        return hash;
    }

    public long getIndex() {
        return index;
    }

    public Address[] getAddresses() {
        return addresses;
    }

    public void setAddresses(Address[] addresses) {
        this.addresses = addresses;
    }

    public byte[] bitcoinSerialize() {
        try {
            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            bitcoinSerializeToStream(outputStream);
            return outputStream.toByteArray();
        } catch (IOException e) {
            //can't happen
        }

        return null;
    }

    @Override
    protected void bitcoinSerializeToStream(OutputStream stream) throws IOException {
        stream.write(hash.getReversedBytes());
        Utils.uint32ToByteStreamLE(index, stream);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TransactionOutPoint other = (TransactionOutPoint) o;
        return getIndex() == other.getIndex() && getHash().equals(other.getHash());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getIndex(), getHash());
    }
}
