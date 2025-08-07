package com.sparrowwallet.drongo.protocol;

import com.sparrowwallet.drongo.Utils;

import java.io.IOException;
import java.io.OutputStream;

public abstract class Message {
    public static final int MAX_SIZE = 0x02000000; // 32MB
    public static final int UNKNOWN_LENGTH = Integer.MIN_VALUE;

    protected byte[] payload;

    // The offset is how many bytes into the provided byte array this message payload starts at.
    protected int offset;
    // The cursor keeps track of where we are in the byte array as we parse it.
    // Note that it's relative to the start of the array NOT the start of the message payload.
    protected int cursor;

    protected int length = UNKNOWN_LENGTH;

    protected Message() {

    }

    protected Message(byte[] payload, int offset) {
        this.payload = payload;
        this.cursor = this.offset = offset;

        parse();
    }

    protected abstract void parse() throws ProtocolException;

    public int getOffset() {
        return offset;
    }

    public int getLength() {
        return length;
    }

    /**
     * This returns a correct value by parsing the message.
     */
    public final int getMessageSize() {
        if (length == UNKNOWN_LENGTH) {
            throw new ProtocolException();
        }

        return length;
    }

    protected long readUint32() throws ProtocolException {
        try {
            long u = Utils.readUint32(payload, cursor);
            cursor += 4;
            return u;
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new ProtocolException(e);
        }
    }

    protected long readInt64() throws ProtocolException {
        try {
            long u = Utils.readInt64(payload, cursor);
            cursor += 8;
            return u;
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new ProtocolException(e);
        }
    }

    protected byte[] readBytes(int length) throws ProtocolException {
        if ((length > MAX_SIZE) || (cursor + length > payload.length)) {
            throw new ProtocolException("Claimed value length too large: " + length);
        }
        try {
            byte[] b = new byte[length];
            System.arraycopy(payload, cursor, b, 0, length);
            cursor += length;
            return b;
        } catch (IndexOutOfBoundsException e) {
            throw new ProtocolException(e);
        }
    }

    protected long readVarInt() throws ProtocolException {
        return readVarInt(0);
    }

    protected long readVarInt(int offset) throws ProtocolException {
        try {
            VarInt varint = new VarInt(payload, cursor + offset);
            cursor += offset + varint.getOriginalSizeInBytes();
            return varint.value;
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new ProtocolException(e);
        }
    }

    protected Sha256Hash readHash() throws ProtocolException {
        // We have to flip it around, as it's been read off the wire in little endian.
        // Not the most efficient way to do this but the clearest.
        return Sha256Hash.wrapReversed(readBytes(32));
    }

    protected void bitcoinSerializeToStream(OutputStream stream) throws IOException {

    }

    protected void adjustLength(int adjustment) {
        adjustLength(0, adjustment);
    }

    protected void adjustLength(int newArraySize, int adjustment) {
        if (length == UNKNOWN_LENGTH)
            return;
        // Our own length is now unknown if we have an unknown length adjustment.
        if (adjustment == UNKNOWN_LENGTH) {
            length = UNKNOWN_LENGTH;
            return;
        }
        length += adjustment;
        // Check if we will need more bytes to encode the length prefix.
        if (newArraySize == 1)
            length++;  // The assumption here is we never call adjustLength with the same arraySize as before.
        else if (newArraySize != 0)
            length += VarInt.sizeOf(newArraySize) - VarInt.sizeOf(newArraySize - 1);
    }
}
