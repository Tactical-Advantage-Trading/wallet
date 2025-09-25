package com.sparrowwallet.drongo.crypto;

import com.sparrowwallet.drongo.Utils;

import org.bouncycastle.math.ec.ECPoint;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class HDKeyDerivation {
    public static final String BITCOIN_SEED_KEY = "Bitcoin seed";

    public static DeterministicKey createMasterPrivateKey(byte[] seed) throws HDDerivationException {
        byte[] hmacSha512 = Utils.getHmacSha512Hash(BITCOIN_SEED_KEY.getBytes(StandardCharsets.UTF_8), seed);
        byte[] privKeyBytes = Arrays.copyOfRange(hmacSha512, 0, 32);
        byte[] chainCode = Arrays.copyOfRange(hmacSha512, 32, 64);
        Arrays.fill(hmacSha512, (byte)0);
        DeterministicKey masterPrivKey = createMasterPrivKeyFromBytes(privKeyBytes, chainCode);
        Arrays.fill(privKeyBytes, (byte)0);
        Arrays.fill(chainCode, (byte)0);
        return masterPrivKey;
    }

    public static DeterministicKey createMasterPrivKeyFromBytes(byte[] privKeyBytes, byte[] chainCode) throws HDDerivationException {
        // childNumberPath is an empty list because we are creating the root key.
        return createMasterPrivKeyFromBytes(privKeyBytes, chainCode, Collections.emptyList());
    }

    public static DeterministicKey createMasterPrivKeyFromBytes(byte[] privKeyBytes, byte[] chainCode, List<ChildNumber> childNumberPath) throws HDDerivationException {
        BigInteger priv = new BigInteger(1, privKeyBytes);
        if(priv.equals(BigInteger.ZERO) || priv.compareTo(ECKey.CURVE.getN()) > 0) {
            throw new HDDerivationException("Private key bytes are not valid");
        }

        return new DeterministicKey(childNumberPath, chainCode, priv, null);
    }

    public static DeterministicKey createMasterPubKeyFromBytes(byte[] pubKeyBytes, byte[] chainCode) {
        return new DeterministicKey(Collections.emptyList(), chainCode, new LazyECPoint(ECKey.CURVE.getCurve(), pubKeyBytes), null, null);
    }

    public static DeterministicKey deriveChildKey(DeterministicKey parent, ChildNumber childNumber) throws HDDerivationException {
        if(parent.isPubKeyOnly()) {
            RawKeyBytes rawKey = deriveChildKeyBytesFromPublic(parent, childNumber);
            return new DeterministicKey(Utils.appendChild(parent.getPath(), childNumber), rawKey.chainCode, new LazyECPoint(ECKey.CURVE.getCurve(), rawKey.keyBytes), null, parent);
        } else {
            RawKeyBytes rawKey = deriveChildKeyBytesFromPrivate(parent, childNumber);
            return new DeterministicKey(Utils.appendChild(parent.getPath(), childNumber), rawKey.chainCode, new BigInteger(1, rawKey.keyBytes), parent);
        }
    }

    public static RawKeyBytes deriveChildKeyBytesFromPrivate(DeterministicKey parent, ChildNumber childNumber) throws HDDerivationException {
        if(parent.isPubKeyOnly()) {
            throw new HDDerivationException("Parent key must have private key bytes for this method");
        }

        byte[] parentPublicKey = parent.getPubKeyPoint().getEncoded(true);
        if(parentPublicKey.length != 33) {
            throw new HDDerivationException("Parent pubkey must be 33 bytes, but is " + parentPublicKey.length);
        }

        ByteBuffer data = ByteBuffer.allocate(37);
        if (childNumber.isHardened()) {
            data.put(parent.getPrivKeyBytes33());
        } else {
            data.put(parentPublicKey);
        }

        data.putInt(childNumber.i());
        byte[] i = Utils.getHmacSha512Hash(parent.getChainCode(), data.array());
        if(i.length != 64) {
            throw new HDDerivationException("HmacSHA512 output must be 64 bytes, is " + i.length);
        }

        byte[] il = Arrays.copyOfRange(i, 0, 32);
        byte[] chainCode = Arrays.copyOfRange(i, 32, 64);
        BigInteger ilInt = new BigInteger(1, il);
        if(ilInt.compareTo(ECKey.CURVE.getN()) > 0) {
            throw new HDDerivationException("Illegal derived key: I_L >= n");
        }

        final BigInteger priv = parent.getPrivKey();
        BigInteger ki = priv.add(ilInt).mod(ECKey.CURVE.getN());
        if(ki.equals(BigInteger.ZERO)) {
            throw new HDDerivationException("Illegal derived key: derived private key equals 0");
        }

        return new RawKeyBytes(ki.toByteArray(), chainCode);
    }

    public static RawKeyBytes deriveChildKeyBytesFromPublic(DeterministicKey parent, ChildNumber childNumber) throws HDDerivationException {
        if(childNumber.isHardened()) {
            throw new HDDerivationException("Can't use private derivation with public keys only");
        }

        byte[] parentPublicKey = parent.getPubKeyPoint().getEncoded(true);
        if(parentPublicKey.length != 33) {
            throw new HDDerivationException("Parent pubkey must be 33 bytes, but is " + parentPublicKey.length);
        }

        ByteBuffer data = ByteBuffer.allocate(37);
        data.put(parentPublicKey);
        data.putInt(childNumber.i());
        byte[] i = Utils.getHmacSha512Hash(parent.getChainCode(), data.array());
        if(i.length != 64) {
            throw new HDDerivationException("HmacSHA512 output must be 64 bytes, is " + i.length);
        }

        byte[] il = Arrays.copyOfRange(i, 0, 32);
        byte[] chainCode = Arrays.copyOfRange(i, 32, 64);
        BigInteger ilInt = new BigInteger(1, il);
        if(ilInt.compareTo(ECKey.CURVE.getN()) > 0) {
            throw new HDDerivationException("Illegal derived key: I_L >= n");
        }

        final BigInteger N = ECKey.CURVE.getN();
        ECPoint Ki = ECKey.publicPointFromPrivate(ilInt).add(parent.getPubKeyPoint());
        if(Ki.equals(ECKey.CURVE.getCurve().getInfinity())) {
            throw new HDDerivationException("Illegal derived key: derived public key equals infinity");
        }

        return new RawKeyBytes(Ki.getEncoded(true), chainCode);
    }

    public static class RawKeyBytes {
        public final byte[] keyBytes, chainCode;

        public RawKeyBytes(byte[] keyBytes, byte[] chainCode) {
            this.keyBytes = keyBytes;
            this.chainCode = chainCode;
        }
    }
}
