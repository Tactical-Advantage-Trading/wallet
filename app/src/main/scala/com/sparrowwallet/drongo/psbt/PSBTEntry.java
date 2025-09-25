package com.sparrowwallet.drongo.psbt;

import com.sparrowwallet.drongo.KeyDerivation;
import com.sparrowwallet.drongo.Utils;
import com.sparrowwallet.drongo.crypto.ChildNumber;
import com.sparrowwallet.drongo.protocol.Sha256Hash;
import com.sparrowwallet.drongo.protocol.VarInt;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class PSBTEntry {
    private final byte[] key;
    private final byte keyType;
    private final byte[] keyData;
    private final byte[] data;

    public PSBTEntry(byte[] key, byte keyType, byte[] keyData, byte[] data) {
        this.key = key;
        this.keyType = keyType;
        this.keyData = keyData;
        this.data = data;
    }

    PSBTEntry(ByteBuffer psbtByteBuffer) throws PSBTParseException {
        int keyLen = readCompactInt(psbtByteBuffer);

        if (keyLen == 0x00) {
            key = null;
            keyType = 0x00;
            keyData = null;
            data = null;
        } else {
            byte[] key = new byte[keyLen];
            psbtByteBuffer.get(key);

            byte keyType = key[0];

            byte[] keyData = null;
            if (key.length > 1) {
                keyData = new byte[key.length - 1];
                System.arraycopy(key, 1, keyData, 0, keyData.length);
            }

            int dataLen = readCompactInt(psbtByteBuffer);
            byte[] data = new byte[dataLen];
            psbtByteBuffer.get(data);

            this.key = key;
            this.keyType = keyType;
            this.keyData = keyData;
            this.data = data;
        }
    }

    public static Map<KeyDerivation, List<Sha256Hash>> parseTaprootKeyDerivation(byte[] data) throws PSBTParseException {
        if(data.length < 1) {
            throw new PSBTParseException("Invalid taproot key derivation: no bytes");
        }
        VarInt varInt = new VarInt(data, 0);
        int offset = varInt.getOriginalSizeInBytes();

        if(data.length < offset + (varInt.value * 32)) {
            throw new PSBTParseException("Invalid taproot key derivation: not enough bytes for leaf hashes");
        }
        List<Sha256Hash> leafHashes = new ArrayList<>();
        for(int i = 0; i < varInt.value; i++) {
            leafHashes.add(Sha256Hash.wrap(Arrays.copyOfRange(data, offset + (i * 32), offset + (i * 32) + 32)));
        }

        KeyDerivation keyDerivation = parseKeyDerivation(Arrays.copyOfRange(data, offset + (leafHashes.size() * 32), data.length));
        return Collections.singletonMap(keyDerivation, leafHashes);
    }

    public static KeyDerivation parseKeyDerivation(byte[] data) throws PSBTParseException {
        if(data.length < 4) {
            throw new PSBTParseException("Invalid master fingerprint specified: not enough bytes");
        }
        String masterFingerprint = getMasterFingerprint(Arrays.copyOfRange(data, 0, 4));
        if(masterFingerprint.length() != 8) {
            throw new PSBTParseException("Invalid master fingerprint specified: " + masterFingerprint);
        }
        if(data.length < 8) {
            return new KeyDerivation(masterFingerprint, "m");
        }
        List<ChildNumber> bip32pathList = readBIP32Derivation(Arrays.copyOfRange(data, 4, data.length));
        String bip32path = KeyDerivation.writePath(bip32pathList);
        return new KeyDerivation(masterFingerprint, bip32path);
    }

    public static String getMasterFingerprint(byte[] data) {
        return Utils.bytesToHex(data);
    }

    public static List<ChildNumber> readBIP32Derivation(byte[] data) {
        List<ChildNumber> path = new ArrayList<>();

        ByteBuffer bb = ByteBuffer.wrap(data);
        byte[] buf = new byte[4];

        do {
            bb.get(buf);
            Utils.reverse(buf);
            ByteBuffer pbuf = ByteBuffer.wrap(buf);
            path.add(new ChildNumber(pbuf.getInt()));
        } while(bb.hasRemaining());

        return path;
    }

    public static byte[] serializeTaprootKeyDerivation(List<Sha256Hash> leafHashes, KeyDerivation keyDerivation) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();

        VarInt hashesLen = new VarInt(leafHashes.size());
        baos.write(hashesLen.encode());
        for(Sha256Hash leafHash : leafHashes) {
            baos.write(leafHash.getBytes());
        }

        baos.write(serializeKeyDerivation(keyDerivation));
        return baos.toByteArray();
    }

    public static byte[] serializeKeyDerivation(KeyDerivation keyDerivation) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte[] fingerprintBytes = Utils.hexToBytes(keyDerivation.getMasterFingerprint());
        if(fingerprintBytes.length != 4) {
            throw new IllegalArgumentException("Invalid number of fingerprint bytes: " + fingerprintBytes.length);
        }
        baos.write(fingerprintBytes);

        for(ChildNumber childNumber : keyDerivation.getDerivation()) {
            byte[] indexBytes = new byte[4];
            Utils.uint32ToByteArrayLE(childNumber.i(), indexBytes, 0);
            baos.write(indexBytes);
        }

        return baos.toByteArray();
    }

    static PSBTEntry populateEntry(byte type, byte[] keydata, byte[] data) {
        return new PSBTEntry(new byte[] {type}, type, keydata, data);
    }

    void serializeToStream(ByteArrayOutputStream baos) throws IOException {
        int keyLen = 1;
        if(keyData != null) {
            keyLen += keyData.length;
        }

        baos.write(writeCompactInt(keyLen));
        baos.write(key);
        if(keyData != null) {
            baos.write(keyData);
        }

        baos.write(writeCompactInt(data.length));
        baos.write(data);
    }

    public byte[] getKey() {
        return key;
    }

    public byte getKeyType() {
        return keyType;
    }

    public byte[] getKeyData() {
        return keyData;
    }

    public byte[] getData() {
        return data;
    }

    public static int readCompactInt(ByteBuffer psbtByteBuffer) throws PSBTParseException {
        byte b = psbtByteBuffer.get();

        switch (b) {
            case (byte) 0xfd: {
                byte[] buf = new byte[2];
                psbtByteBuffer.get(buf);
                ByteBuffer byteBuffer = ByteBuffer.wrap(buf);
                byteBuffer.order(ByteOrder.LITTLE_ENDIAN);
                return byteBuffer.getShort() & 0xFFFF;
            }
            case (byte) 0xfe: {
                byte[] buf = new byte[4];
                psbtByteBuffer.get(buf);
                ByteBuffer byteBuffer = ByteBuffer.wrap(buf);
                byteBuffer.order(ByteOrder.LITTLE_ENDIAN);
                return byteBuffer.getInt();
            }
            case (byte) 0xff: {
                byte[] buf = new byte[8];
                psbtByteBuffer.get(buf);
                ByteBuffer byteBuffer = ByteBuffer.wrap(buf);
                byteBuffer.order(ByteOrder.LITTLE_ENDIAN);
                throw new PSBTParseException("Data too long:" + byteBuffer.getLong());
            }
            default:
                return (int) (b & 0xff);
        }
    }

    public static byte[] writeCompactInt(long val) {
        ByteBuffer bb = null;

        if (val < 0xfdL) {
            bb = ByteBuffer.allocate(1);
            bb.order(ByteOrder.LITTLE_ENDIAN);
            bb.put((byte) val);
        } else if (val < 0xffffL) {
            bb = ByteBuffer.allocate(3);
            bb.order(ByteOrder.LITTLE_ENDIAN);
            bb.put((byte) 0xfd);
            bb.put((byte) (val & 0xff));
            bb.put((byte) ((val >> 8) & 0xff));
        } else if (val < 0xffffffffL) {
            bb = ByteBuffer.allocate(5);
            bb.order(ByteOrder.LITTLE_ENDIAN);
            bb.put((byte) 0xfe);
            bb.putInt((int) val);
        } else {
            bb = ByteBuffer.allocate(9);
            bb.order(ByteOrder.LITTLE_ENDIAN);
            bb.put((byte) 0xff);
            bb.putLong(val);
        }

        return bb.array();
    }

    public void checkOneByteKey() throws PSBTParseException {
        if(this.getKey().length != 1) {
            throw new PSBTParseException("PSBT key type must be one byte");
        }
    }

    public void checkOneBytePlusXpubKey() throws PSBTParseException {
        if(this.getKey().length != 79) {
            throw new PSBTParseException("PSBT key type must be one byte plus xpub");
        }
    }

    public void checkOneBytePlusPubKey() throws PSBTParseException {
        if(this.getKey().length != 34) {
            throw new PSBTParseException("PSBT key type must be one byte plus pub key");
        }
    }

    public void checkOneBytePlusXOnlyPubKey() throws PSBTParseException {
        if(this.getKey().length != 33) {
            throw new PSBTParseException("PSBT key type must be one byte plus x only pub key");
        }
    }
}
