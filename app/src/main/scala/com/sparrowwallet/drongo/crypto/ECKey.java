package com.sparrowwallet.drongo.crypto;

import com.sparrowwallet.drongo.Utils;
import com.sparrowwallet.drongo.protocol.ScriptType;
import com.sparrowwallet.drongo.protocol.Sha256Hash;
import com.sparrowwallet.drongo.protocol.SigHash;
import com.sparrowwallet.drongo.protocol.TransactionSignature;
import com.sparrowwallet.drongo.protocol.VarInt;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Integer;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERBitString;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSequenceGenerator;
import org.bouncycastle.asn1.DERTaggedObject;
import org.bouncycastle.asn1.DLSequence;
import org.bouncycastle.asn1.x9.X9ECParameters;
import org.bouncycastle.asn1.x9.X9IntegerConverter;
import org.bouncycastle.crypto.AsymmetricCipherKeyPair;
import org.bouncycastle.crypto.digests.SHA256Digest;
import org.bouncycastle.crypto.ec.CustomNamedCurves;
import org.bouncycastle.crypto.generators.ECKeyPairGenerator;
import org.bouncycastle.crypto.params.ECDomainParameters;
import org.bouncycastle.crypto.params.ECKeyGenerationParameters;
import org.bouncycastle.crypto.params.ECPrivateKeyParameters;
import org.bouncycastle.crypto.params.ECPublicKeyParameters;
import org.bouncycastle.crypto.signers.ECDSASigner;
import org.bouncycastle.math.ec.ECAlgorithms;
import org.bouncycastle.math.ec.ECPoint;
import org.bouncycastle.math.ec.FixedPointCombMultiplier;
import org.bouncycastle.math.ec.FixedPointUtil;
import org.bouncycastle.math.ec.custom.sec.SecP256K1Curve;
import org.bouncycastle.util.encoders.Hex;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.security.SecureRandom;
import java.security.SignatureException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Objects;

/**
 * <p>Represents an elliptic curve public and (optionally) private key, usable for digital signatures but not encryption.
 * Creating a new ECKey with the empty constructor will generate a new random keypair. Other static methods can be used
 * when you already have the public or private parts. If you create a key with only the public part, you can check
 * signatures but not create them.</p>
 *
 * <p>ECKey also provides access to Bitcoin Core compatible text message signing, as accessible via the UI or JSON-RPC.
 * This is slightly different to signing raw bytes - if you want to sign your own data and it won't be exposed as
 * text to people, you don't want to use this. If in doubt, ask on the mailing list.</p>
 *
 * <p>The ECDSA algorithm supports <i>key recovery</i> in which a signature plus a couple of discriminator bits can
 * be reversed to find the public key used to calculate it. This can be convenient when you have a message and a
 * signature and want to find out who signed it, rather than requiring the user to provide the expected identity.</p>
 *
 * <p>This class supports a variety of serialization forms. The methods that accept/return byte arrays serialize
 * private keys as raw byte arrays and public keys using the SEC standard byte encoding for public keys. Signatures
 * are encoded using ASN.1/DER inside the Bitcoin protocol.</p>
 *
 * <p>A key can be <i>compressed</i> or <i>uncompressed</i>. This refers to whether the public key is represented
 * when encoded into bytes as an (x, y) coordinate on the elliptic curve, or whether it's represented as just an X
 * co-ordinate and an extra byte that carries a sign bit. With the latter form the Y coordinate can be calculated
 * dynamically, however, <b>because the binary serialization is different the address of a key changes if its
 * compression status is changed</b>. If you deviate from the defaults it's important to understand this: money sent
 * to a compressed version of the key will have a different address to the same key in uncompressed form. Whether
 * a public key is compressed or not is recorded in the SEC binary serialisation format, and preserved in a flag in
 * this class so round-tripping preserves state. Unless you're working with old software or doing unusual things, you
 * can usually ignore the compressed/uncompressed distinction.</p>
 */
public class ECKey {
    // The parameters of the secp256k1 curve that Bitcoin uses.
    private static final X9ECParameters CURVE_PARAMS = CustomNamedCurves.getByName("secp256k1");

    /** The parameters of the secp256k1 curve that Bitcoin uses. */
    public static final ECDomainParameters CURVE;

    /**
     * Equal to CURVE.getN().shiftRight(1), used for canonicalising the S value of a signature. If you aren't
     * sure what this is about, you can ignore it.
     */
    public static final BigInteger HALF_CURVE_ORDER;

    private static final SecureRandom secureRandom;

    static {
        // Tell Bouncy Castle to precompute data that's needed during secp256k1 calculations.
        FixedPointUtil.precompute(CURVE_PARAMS.getG());
        CURVE = new ECDomainParameters(CURVE_PARAMS.getCurve(), CURVE_PARAMS.getG(), CURVE_PARAMS.getN(),
                CURVE_PARAMS.getH());
        HALF_CURVE_ORDER = CURVE_PARAMS.getN().shiftRight(1);
        secureRandom = new SecureRandom();
    }

    // The two parts of the key. If "pub" is set but not "priv", we can only verify signatures, not make them.
    protected final BigInteger priv;
    protected final LazyECPoint pub;

    private byte[] pubKeyHash;

    /**
     * Generates an entirely new keypair. Point compression is used so the resulting public key will be 33 bytes
     * (32 for the co-ordinate and 1 byte to represent the y bit).
     */
    public ECKey() {
        this(secureRandom);
    }

    /**
     * Generates an entirely new keypair with the given {@link SecureRandom} object. Point compression is used so the
     * resulting public key will be 33 bytes (32 for the co-ordinate and 1 byte to represent the y bit).
     */
    public ECKey(SecureRandom secureRandom) {
        ECKeyPairGenerator generator = new ECKeyPairGenerator();
        ECKeyGenerationParameters keygenParams = new ECKeyGenerationParameters(CURVE, secureRandom);
        generator.init(keygenParams);
        AsymmetricCipherKeyPair keypair = generator.generateKeyPair();
        ECPrivateKeyParameters privParams = (ECPrivateKeyParameters) keypair.getPrivate();
        ECPublicKeyParameters pubParams = (ECPublicKeyParameters) keypair.getPublic();
        priv = privParams.getD();
        pub = getPointWithCompression(pubParams.getQ(), true);
    }

    protected ECKey(BigInteger priv, ECPoint pub, boolean compressed) {
        this(priv, getPointWithCompression(pub, compressed));
    }

    protected ECKey(BigInteger priv, LazyECPoint pub) {
        if(priv != null) {
            if(priv.equals(BigInteger.ZERO) || priv.equals(BigInteger.ONE)) {
                throw new IllegalArgumentException("Private key is illegal: " + priv);
            }
        }

        if(pub == null) {
            throw new IllegalArgumentException("Public key cannot be null");
        }

        this.priv = priv;
        this.pub = pub;
    }

    /**
     * Utility for compressing an elliptic curve point. Returns the same point if it's already compressed.
     * See the ECKey class docs for a discussion of point compression.
     */
    public static LazyECPoint compressPoint(LazyECPoint point) {
        return point.isCompressed() ? point : getPointWithCompression(point.get(), true);
    }

    /**
     * Utility for decompressing an elliptic curve point. Returns the same point if it's already uncompressed.
     * See the ECKey class docs for a discussion of point compression.
     */
    public static LazyECPoint decompressPoint(LazyECPoint point) {
        return !point.isCompressed() ? point : getPointWithCompression(point.get(), false);
    }

    private static LazyECPoint getPointWithCompression(ECPoint point, boolean compressed) {
        return new LazyECPoint(point, compressed);
    }

    /**
     * Creates an ECKey given the private key only. The public key is calculated from it (this is slow). The resulting
     * public key is compressed.
     */
    public static ECKey fromPrivate(BigInteger privKey) {
        return fromPrivate(privKey, true);
    }

    /**
     * Creates an ECKey given the private key only. The public key is calculated from it (this is slow).
     * @param compressed Determines whether the resulting ECKey will use a compressed encoding for the public key.
     */
    public static ECKey fromPrivate(BigInteger privKey, boolean compressed) {
        ECPoint point = publicPointFromPrivate(privKey);
        return new ECKey(privKey, getPointWithCompression(point, compressed));
    }

    /**
     * Creates an ECKey given the private key only. The public key is calculated from it (this is slow). The resulting
     * public key is compressed.
     */
    public static ECKey fromPrivate(byte[] privKeyBytes) {
        return fromPrivate(new BigInteger(1, privKeyBytes));
    }

    /**
     * Creates an ECKey given the private key only. The public key is calculated from it (this is slow).
     * @param compressed Determines whether the resulting ECKey will use a compressed encoding for the public key.
     */
    public static ECKey fromPrivate(byte[] privKeyBytes, boolean compressed) {
        return fromPrivate(new BigInteger(1, privKeyBytes), compressed);
    }

    /**
     * Creates an ECKey that cannot be used for signing, only verifying signatures, from the given point.
     * @param compressed Determines whether the resulting ECKey will use a compressed encoding for the public key.
     */
    public static ECKey fromPublicOnly(ECPoint pub, boolean compressed) {
        return new ECKey(null, pub, compressed);
    }

    /**
     * Creates an ECKey that cannot be used for signing, only verifying signatures, from the given encoded point.
     * The compression state of pub will be preserved.
     */
    public static ECKey fromPublicOnly(byte[] pub) {
        return new ECKey(null, new LazyECPoint(CURVE.getCurve(), pub));
    }

    public static ECKey fromPublicOnly(ECKey key) {
        return fromPublicOnly(key.getPubKeyPoint(), key.isCompressed());
    }

    /**
     * Returns true if this key doesn't have unencrypted access to private key bytes. This may be because it was never
     * given any private key bytes to begin with (a watching key), or because the key is encrypted.
     */
    public boolean isPubKeyOnly() {
        return priv == null;
    }

    /**
     * Returns true if this key has unencrypted access to private key bytes. Does the opposite of
     * {@link #isPubKeyOnly()}.
     */
    public boolean hasPrivKey() {
        return priv != null;
    }

    /**
     * Output this ECKey as an ASN.1 encoded private key, as understood by OpenSSL or used by Bitcoin Core
     * in its wallet storage format.
     * @throws MissingPrivateKeyException if the private key is missing or encrypted.
     */
    public byte[] toASN1() {
        try {
            byte[] privKeyBytes = getPrivKeyBytes();
            ByteArrayOutputStream baos = new ByteArrayOutputStream(400);

            // ASN1_SEQUENCE(EC_PRIVATEKEY) = {
            //   ASN1_SIMPLE(EC_PRIVATEKEY, version, LONG),
            //   ASN1_SIMPLE(EC_PRIVATEKEY, privateKey, ASN1_OCTET_STRING),
            //   ASN1_EXP_OPT(EC_PRIVATEKEY, parameters, ECPKPARAMETERS, 0),
            //   ASN1_EXP_OPT(EC_PRIVATEKEY, publicKey, ASN1_BIT_STRING, 1)
            // } ASN1_SEQUENCE_END(EC_PRIVATEKEY)
            DERSequenceGenerator seq = new DERSequenceGenerator(baos);
            seq.addObject(new ASN1Integer(1)); // version
            seq.addObject(new DEROctetString(privKeyBytes));
            seq.addObject(new DERTaggedObject(0, CURVE_PARAMS.toASN1Primitive()));
            seq.addObject(new DERTaggedObject(1, new DERBitString(getPubKey())));
            seq.close();
            return baos.toByteArray();
        } catch (IOException e) {
            throw new RuntimeException(e);  // Cannot happen, writing to memory stream.
        }
    }

    /**
     * Returns public key bytes from the given private key. To convert a byte array into a BigInteger,
     * use {@code new BigInteger(1, bytes);}
     */
    public static byte[] publicKeyFromPrivate(BigInteger privKey, boolean compressed) {
        ECPoint point = publicPointFromPrivate(privKey);
        return point.getEncoded(compressed);
    }

    /**
     * Returns public key point from the given private key. To convert a byte array into a BigInteger,
     * use {@code new BigInteger(1, bytes);}
     */
    public static ECPoint publicPointFromPrivate(BigInteger privKey) {
        if (privKey.bitLength() > CURVE.getN().bitLength()) {
            privKey = privKey.mod(CURVE.getN());
        }

        return new FixedPointCombMultiplier().multiply(CURVE.getG(), privKey);
    }

    /** Gets the hash160 form of the public key (as seen in addresses). */
    public byte[] getPubKeyHash() {
        if (pubKeyHash == null)
            pubKeyHash = Utils.sha256hash160(this.pub.getEncoded());
        return pubKeyHash;
    }

    /**
     * Gets the raw public key value. This appears in transaction scriptSigs. Note that this is <b>not</b> the same
     * as the pubKeyHash/address.
     */
    public byte[] getPubKey() {
        return pub.getEncoded();
    }

    /**
     * Gets the x coordinate of the raw public key value. This appears in transaction scriptPubKeys for Taproot outputs.
     */
    public byte[] getPubKeyXCoord() {
        return pub.getEncodedXCoord();
    }

    /** Gets the public key in the form of an elliptic curve point object from Bouncy Castle. */
    public ECPoint getPubKeyPoint() {
        return pub.get();
    }

    /**
     * Gets the private key in the form of an integer field element. The public key is derived by performing EC
     * point addition this number of times (i.e. point multiplying).
     *
     * @throws java.lang.IllegalStateException if the private key bytes are not available.
     */
    public BigInteger getPrivKey() {
        if (priv == null) {
            throw new MissingPrivateKeyException();
        }

        return priv;
    }

    /**
     * Returns whether this key is using the compressed form or not. Compressed pubkeys are only 33 bytes, not 64.
     */
    public boolean isCompressed() {
        return pub.isCompressed();
    }

    public TransactionSignature sign(Sha256Hash input, SigHash sigHash, TransactionSignature.Type type) {
        TransactionSignature transactionSignature;

        if(type == TransactionSignature.Type.SCHNORR) {
            SchnorrSignature schnorrSignature = signSchnorr(input);
            transactionSignature = new TransactionSignature(schnorrSignature, sigHash);
        } else {
            ECDSASignature ecdsaSignature = signEcdsa(input);
            transactionSignature = new TransactionSignature(ecdsaSignature, sigHash);
        }

        //Verify transaction signature immediately after signing as recommended in BIP340
        if(!transactionSignature.verify(input.getBytes(), this)) {
            throw new IllegalStateException("Generated signature failed verification");
        }

        return transactionSignature;
    }

    /**
     * Signs the given hash and returns the R and S components as an ECDSASignature.
     */
    public ECDSASignature signEcdsa(Sha256Hash input) {
        if(priv == null) {
            throw new IllegalArgumentException("Private key cannot be null");
        }

        ECDSASignature signature;
        Integer counter = null;
        do {
            ECDSASigner signer = new ECDSASigner(new HMacDSANonceKCalculator(new SHA256Digest(), counter));
            ECPrivateKeyParameters privKey = new ECPrivateKeyParameters(priv, CURVE);
            signer.init(true, privKey);
            BigInteger[] components = signer.generateSignature(input.getBytes());
            signature = new ECDSASignature(components[0], components[1]).toCanonicalised();
            counter = (counter == null ? 1 : counter+1);
        } while(!signature.hasLowR());

        return signature;
    }

    /**
     * Signs the given hash and returns the R and S components as a SchnorrSignature.
     */
    public SchnorrSignature signSchnorr(Sha256Hash input) {
        throw new IllegalStateException("libsecp256k1 is not enabled");
    }

    /**
     * Verifies the given TransactionSignature against the provided byte array using the public key.
     */
    public boolean verify(byte[] data, TransactionSignature signature) {
        return signature.verify(data, this);
    }

    /**
     * Verifies the given R/S pair (signature) against a hash using the public key.
     */
    public boolean verify(Sha256Hash sigHash, TransactionSignature signature) {
        return verify(sigHash.getBytes(), signature);
    }

    public ECKey getTweakedOutputKey() {
        TaprootPubKey taprootPubKey = liftX(getPubKeyXCoord());
        ECPoint internalKey = taprootPubKey.ecPoint;
        byte[] taggedHash = Utils.taggedHash("TapTweak", internalKey.getXCoord().getEncoded());
        ECKey tweakValue = ECKey.fromPrivate(taggedHash);
        ECPoint outputKey = internalKey.add(tweakValue.getPubKeyPoint());

        if(hasPrivKey()) {
            BigInteger taprootPriv = priv;
            BigInteger tweakedPrivKey = taprootPriv.add(tweakValue.getPrivKey()).mod(CURVE_PARAMS.getCurve().getOrder());
            //TODO: Improve on this hack. How do we know whether to negate the private key before tweaking it?
            if(!ECKey.fromPrivate(tweakedPrivKey).getPubKeyPoint().equals(outputKey)) {
                taprootPriv = CURVE_PARAMS.getCurve().getOrder().subtract(priv);
                tweakedPrivKey = taprootPriv.add(tweakValue.getPrivKey()).mod(CURVE_PARAMS.getCurve().getOrder());
            }

            return new ECKey(tweakedPrivKey, outputKey, true);
        }

        return ECKey.fromPublicOnly(outputKey, true);
    }

    private static TaprootPubKey liftX(byte[] bytes) {
        SecP256K1Curve secP256K1Curve = (SecP256K1Curve)CURVE_PARAMS.getCurve();
        BigInteger x = new BigInteger(1, bytes);
        BigInteger p = secP256K1Curve.getQ();
        if(x.compareTo(p) > -1) {
            throw new IllegalArgumentException("Provided bytes must be less than secp256k1 field size");
        }

        BigInteger y_sq = x.modPow(BigInteger.valueOf(3), p).add(BigInteger.valueOf(7)).mod(p);
        BigInteger y = y_sq.modPow(p.add(BigInteger.valueOf(1)).divide(BigInteger.valueOf(4)), p);
        if(!y.modPow(BigInteger.valueOf(2), p).equals(y_sq)) {
            throw new IllegalStateException("Calculated invalid y_sq when solving for y co-ordinate");
        }

        return y.and(BigInteger.ONE).equals(BigInteger.ZERO) ? new TaprootPubKey(secP256K1Curve.createPoint(x, y), false) : new TaprootPubKey(secP256K1Curve.createPoint(x, p.subtract(y)), true);
    }

    private static class TaprootPubKey {
        public final ECPoint ecPoint;
        public final boolean negated;

        public TaprootPubKey(ECPoint ecPoint, boolean negated) {
            this.ecPoint = ecPoint;
            this.negated = negated;
        }
    }

    /**
     * Returns true if the given pubkey is canonical, i.e. the correct length taking into account compression.
     */
    public static boolean isPubKeyCanonical(byte[] pubkey) {
        if (pubkey.length < 32)
            return false;
        if (pubkey.length == 32)
            return true;
        if (pubkey[0] == 0x04) {
            // Uncompressed pubkey
            if (pubkey.length != 65)
                return false;
        } else if (pubkey[0] == 0x02 || pubkey[0] == 0x03) {
            // Compressed pubkey
            if (pubkey.length != 33)
                return false;
        } else
            return false;
        return true;
    }

    /**
     * Returns true if the given pubkey is in its compressed form.
     */
    public static boolean isPubKeyCompressed(byte[] encoded) {
        if (encoded.length == 32 || (encoded.length == 33 && (encoded[0] == 0x02 || encoded[0] == 0x03)))
            return true;
        else if (encoded.length == 65 && encoded[0] == 0x04)
            return false;
        else
            throw new IllegalArgumentException(Hex.toHexString(encoded));
    }

    /**
     * Signs a text message using the standard Bitcoin messaging signing format and returns the signature as a base64
     * encoded string.
     *
     * @throws IllegalStateException if this ECKey does not have the private part.
     */
    public String signMessage(String message, ScriptType scriptType) {
        return signMessage(message, scriptType, this::signEcdsa);
    }

    public String signMessage(String message, ScriptType scriptType, ECDSAHashSigner ecdsaHashSigner) {
        byte[] data = formatMessageForSigning(message);
        Sha256Hash hash = Sha256Hash.twiceOf(data);
        ECDSASignature sig = ecdsaHashSigner.sign(hash);
        byte recId = findRecoveryId(hash, sig);
        int headerByte = recId + getSigningTypeConstant(scriptType);
        byte[] sigData = new byte[65];  // 1 header + 32 bytes for R + 32 bytes for S
        sigData[0] = (byte) headerByte;
        System.arraycopy(Utils.bigIntegerToBytes(sig.r, 32), 0, sigData, 1, 32);
        System.arraycopy(Utils.bigIntegerToBytes(sig.s, 32), 0, sigData, 33, 32);
        return new String(android.util.Base64.encode(sigData, android.util.Base64.NO_WRAP), StandardCharsets.UTF_8);
    }

    /**
     * Although no standard has yet been decided on, we follow Trezor's approach for now as documented in
     * https://github.com/bitcoin/bips/blob/master/bip-0137.mediawiki
     *
     * @param scriptType The script type of the address used to sign
     * @return A constant used to alter the header of the signature in order to distinguish between different script types
     */
    private int getSigningTypeConstant(ScriptType scriptType) {
        if(scriptType == ScriptType.P2PKH) {
            return 27 + (isCompressed() ? 4 : 0);
        } else if(scriptType == ScriptType.P2SH_P2WPKH) {
            return 35;
        } else if(scriptType == ScriptType.P2WPKH) {
            return 39;
        }

        throw new IllegalArgumentException("Script type of " + scriptType + " is not supported for message signing");
    }

    /**
     * Given an arbitrary piece of text and a Bitcoin-format message signature encoded in base64, returns an ECKey
     * containing the public key that was used to sign it. This can then be compared to the expected public key to
     * determine if the signature was correct. These sorts of signatures are compatible with the Bitcoin-Qt/bitcoind
     * format generated by signmessage/verifymessage RPCs and GUI menu options. They are intended for humans to verify
     * their communications with each other, hence the base64 format and the fact that the input is text.
     *
     * @param message         Some piece of human readable text.
     * @param signatureBase64 The Bitcoin-format message signature in base64
     * @param electrumFormat  Whether to generate a key following Electrum's approach of regarding P2SH-P2WSH as the same as P2PKH uncompressed
     * @throws SignatureException If the public key could not be recovered or if there was a signature format error.
     */
    public static ECKey signedMessageToKey(String message, String signatureBase64, boolean electrumFormat) throws SignatureException {
        byte[] signatureEncoded;
        try {
            signatureEncoded = android.util.Base64.decode(signatureBase64, android.util.Base64.NO_WRAP);
        } catch(RuntimeException e) {
            // This is what you get back from Bouncy Castle if base64 doesn't decode :(
            throw new SignatureException("Could not decode base64", e);
        }
        byte[] messageBytes = formatMessageForSigning(message);
        // Note that the C++ code doesn't actually seem to specify any character encoding. Presumably it's whatever
        // JSON-SPIRIT hands back. Assume UTF-8 for now.
        Sha256Hash messageHash = Sha256Hash.twiceOf(messageBytes);
        return signedHashToKey(messageHash, signatureEncoded, electrumFormat);
    }

    public static ECKey signedHashToKey(Sha256Hash messageHash, byte[] signatureEncoded, boolean electrumFormat) throws SignatureException {
        // Parse the signature bytes into r/s and the selector value.
        if(signatureEncoded.length < 65) {
            throw new SignatureException("Signature truncated, expected 65 bytes and got " + signatureEncoded.length);
        }
        int header = signatureEncoded[0] & 0xFF;
        // The header byte: 0x1B = first key with even y, 0x1C = first key with odd y,
        //                  0x1D = second key with even y, 0x1E = second key with odd y
        if(header < 27 || header > 42) {
            throw new SignatureException("Header byte out of range: " + header);
        }
        BigInteger r = new BigInteger(1, Arrays.copyOfRange(signatureEncoded, 1, 33));
        BigInteger s = new BigInteger(1, Arrays.copyOfRange(signatureEncoded, 33, 65));
        ECDSASignature sig = new ECDSASignature(r, s);

        boolean compressed = false;
        if(header >= 39) { // this is a bech32 signature
            header -= 12;
            compressed = true;
        }
        else if(header >= 35 && !electrumFormat) { // this is a segwit p2sh signature
            compressed = true;
            header -= 8;
        }
        else if(header >= 31) { // this is a compressed key signature
            compressed = true;
            header -= 4;
        }
        int recId = header - 27;
        ECKey key = ECKey.recoverFromSignature(recId, sig, messageHash, compressed);
        if(key == null) {
            throw new SignatureException("Could not recover public key from signature");
        }
        return key;
    }

    /**
     * Convenience wrapper around {@link ECKey#signedMessageToKey(String, String, boolean)}. If the key derived from the
     * signature is not the same as this one, throws a SignatureException.
     */
    public void verifyMessage(String message, String signatureBase64) throws SignatureException {
        ECKey key = ECKey.signedMessageToKey(message, signatureBase64, false);
        if(!key.pub.equals(pub)) {
            throw new SignatureException("Signature did not match for message");
        }
    }

    /**
     * Returns the recovery ID, a byte with value between 0 and 3, inclusive, that specifies which of 4 possible
     * curve points was used to sign a message. This value is also referred to as "v".
     *
     * @throws RuntimeException if no recovery ID can be found.
     */
    public byte findRecoveryId(Sha256Hash hash, ECDSASignature sig) {
        byte recId = -1;
        for(byte i = 0; i < 4; i++) {
            ECKey k = ECKey.recoverFromSignature(i, sig, hash, isCompressed());
            if(k != null && k.pub.equals(pub)) {
                recId = i;
                break;
            }
        }
        if(recId == -1) {
            throw new RuntimeException("Could not construct a recoverable key. This should never happen.");
        }
        return recId;
    }

    /**
     * <p>Given the components of a signature and a selector value, recover and return the public key
     * that generated the signature according to the algorithm in SEC1v2 section 4.1.6.</p>
     *
     * <p>The recId is an index from 0 to 3 which indicates which of the 4 possible keys is the correct one. Because
     * the key recovery operation yields multiple potential keys, the correct key must either be stored alongside the
     * signature, or you must be willing to try each recId in turn until you find one that outputs the key you are
     * expecting.</p>
     *
     * <p>If this method returns null it means recovery was not possible and recId should be iterated.</p>
     *
     * <p>Given the above two points, a correct usage of this method is inside a for loop from 0 to 3, and if the
     * output is null OR a key that is not the one you expect, you try again with the next recId.</p>
     *
     * @param recId      Which possible key to recover.
     * @param sig        the R and S components of the signature, wrapped.
     * @param message    Hash of the data that was signed.
     * @param compressed Whether or not the original pubkey was compressed.
     * @return An ECKey containing only the public part, or null if recovery wasn't possible.
     */
    public static ECKey recoverFromSignature(int recId, ECDSASignature sig, Sha256Hash message, boolean compressed) {
        if(recId < 0) {
            throw new IllegalArgumentException("recId must be positive");
        }
        if(sig.r.signum() < 0 || sig.s.signum() < 0) {
            throw new IllegalArgumentException("Signature values r and s must both be positive");
        }
        if(message == null) {
            throw new IllegalArgumentException("Message cannot be null");
        }

        // 1.0 For j from 0 to h   (h == recId here and the loop is outside this function)
        //   1.1 Let x = r + jn
        BigInteger n = CURVE.getN();  // Curve order.
        BigInteger i = BigInteger.valueOf((long) recId / 2);
        BigInteger x = sig.r.add(i.multiply(n));
        //   1.2. Convert the integer x to an octet string X of length mlen using the conversion routine
        //        specified in Section 2.3.7, where mlen = ⌈(log2 p)/8⌉ or mlen = ⌈m/8⌉.
        //   1.3. Convert the octet string (16 set binary digits)||X to an elliptic curve point R using the
        //        conversion routine specified in Section 2.3.4. If this conversion routine outputs "invalid", then
        //        do another iteration of Step 1.
        //
        // More concisely, what these points mean is to use X as a compressed public key.
        BigInteger prime = SecP256K1Curve.q;
        if(x.compareTo(prime) >= 0) {
            // Cannot have point co-ordinates larger than this as everything takes place modulo Q.
            return null;
        }
        // Compressed keys require you to know an extra bit of data about the y-coord as there are two possibilities.
        // So it's encoded in the recId.
        ECPoint R = decompressKey(x, (recId & 1) == 1);
        //   1.4. If nR != point at infinity, then do another iteration of Step 1 (callers responsibility).
        if(!R.multiply(n).isInfinity()) {
            return null;
        }
        //   1.5. Compute e from M using Steps 2 and 3 of ECDSA signature verification.
        BigInteger e = message.toBigInteger();
        //   1.6. For k from 1 to 2 do the following.   (loop is outside this function via iterating recId)
        //   1.6.1. Compute a candidate public key as:
        //               Q = mi(r) * (sR - eG)
        //
        // Where mi(x) is the modular multiplicative inverse. We transform this into the following:
        //               Q = (mi(r) * s ** R) + (mi(r) * -e ** G)
        // Where -e is the modular additive inverse of e, that is z such that z + e = 0 (mod n). In the above equation
        // ** is point multiplication and + is point addition (the EC group operator).
        //
        // We can find the additive inverse by subtracting e from zero then taking the mod. For example the additive
        // inverse of 3 modulo 11 is 8 because 3 + 8 mod 11 = 0, and -3 mod 11 = 8.
        BigInteger eInv = BigInteger.ZERO.subtract(e).mod(n);
        BigInteger rInv = sig.r.modInverse(n);
        BigInteger srInv = rInv.multiply(sig.s).mod(n);
        BigInteger eInvrInv = rInv.multiply(eInv).mod(n);
        ECPoint q = ECAlgorithms.sumOfTwoMultiplies(CURVE.getG(), eInvrInv, R, srInv);
        return ECKey.fromPublicOnly(q, compressed);
    }

    /**
     * Decompress a compressed public key (x co-ord and low-bit of y-coord).
     */
    private static ECPoint decompressKey(BigInteger xBN, boolean yBit) {
        X9IntegerConverter x9 = new X9IntegerConverter();
        byte[] compEnc = x9.integerToBytes(xBN, 1 + x9.getByteLength(CURVE.getCurve()));
        compEnc[0] = (byte) (yBit ? 0x03 : 0x02);
        return CURVE.getCurve().decodePoint(compEnc);
    }

    /**
     * Returns a 32 byte array containing the private key.
     * @throws MissingPrivateKeyException if the private key bytes are missing/encrypted.
     */
    public byte[] getPrivKeyBytes() {
        return Utils.bigIntegerToBytes(getPrivKey(), 32);
    }

    public void clear() {
        for(int i = 0; i < priv.bitLength(); i++) {
            priv.clearBit(i);
        }
    }

    public static class MissingPrivateKeyException extends RuntimeException {
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ECKey)) return false;
        ECKey other = (ECKey) o;
        return Objects.equals(this.priv, other.priv)
                && Objects.equals(this.pub, other.pub);
    }

    @Override
    public int hashCode() {
        return pub.hashCode();
    }

    @Override
    public String toString() {
        return pub.toString();
    }

    public static class LexicographicECKeyComparator implements Comparator<ECKey> {
        @Override
        public int compare(ECKey leftKey, ECKey rightKey) {
            byte[] left = leftKey.getPubKey();
            byte[] right = rightKey.getPubKey();

            int minLength = Math.min(left.length, right.length);
            for (int i = 0; i < minLength; i++) {
                int result = compare(left[i], right[i]);
                if (result != 0) {
                    return result;
                }
            }

            return left.length - right.length;
        }

        public static int compare(byte a, byte b) {
            int unsignedA = a & 0xFF;
            int unsignedB = b & 0xFF;
            return unsignedA - unsignedB;
        }
    }

    /** The string that prefixes all text messages signed using Bitcoin keys. */
    private static final String BITCOIN_SIGNED_MESSAGE_HEADER = "Bitcoin Signed Message:\n";
    private static final byte[] BITCOIN_SIGNED_MESSAGE_HEADER_BYTES = BITCOIN_SIGNED_MESSAGE_HEADER.getBytes(StandardCharsets.UTF_8);

    /**
     * <p>Given a textual message, returns a byte buffer formatted as follows:</p>
     * <p>{@code [24] "Bitcoin Signed Message:\n" [message.length as a varint] message}</p>
     */
    private static byte[] formatMessageForSigning(String message) {
        try {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            bos.write(BITCOIN_SIGNED_MESSAGE_HEADER_BYTES.length);
            bos.write(BITCOIN_SIGNED_MESSAGE_HEADER_BYTES);
            byte[] messageBytes = message.getBytes(StandardCharsets.UTF_8);
            VarInt size = new VarInt(messageBytes.length);
            bos.write(size.encode());
            bos.write(messageBytes);
            return bos.toByteArray();
        } catch (IOException e) {
            throw new RuntimeException(e);  // Cannot happen.
        }
    }

    public interface ECDSAHashSigner {
        ECDSASignature sign(Sha256Hash hash);
    }
}
