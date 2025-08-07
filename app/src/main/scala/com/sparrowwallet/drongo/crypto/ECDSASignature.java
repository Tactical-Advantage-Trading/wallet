package com.sparrowwallet.drongo.crypto;

import com.sparrowwallet.drongo.protocol.SigHash;
import com.sparrowwallet.drongo.protocol.SignatureDecodeException;
import com.sparrowwallet.drongo.protocol.TransactionSignature;
import com.sparrowwallet.drongo.protocol.VerificationException;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Integer;
import org.bouncycastle.asn1.ASN1Primitive;
import org.bouncycastle.asn1.DERSequenceGenerator;
import org.bouncycastle.asn1.DLSequence;
import org.bouncycastle.crypto.params.ECPublicKeyParameters;
import org.bouncycastle.crypto.signers.ECDSASigner;
import org.bouncycastle.util.Properties;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.util.Objects;

import static com.sparrowwallet.drongo.crypto.ECKey.CURVE;

/**
 * Groups the two components that make up a signature, and provides a way to encode to DER form, which is
 * how ECDSA signatures are represented when embedded in other data structures in the Bitcoin protocol. The raw
 * components can be useful for doing further EC maths on them.
 */
public class ECDSASignature {
    /**
     * The two components of the signature.
     */
    public final BigInteger r, s;

    /**
     * Constructs a signature with the given components. Does NOT automatically canonicalise the signature.
     */
    public ECDSASignature(BigInteger r, BigInteger s) {
        this.r = r;
        this.s = s;
    }

    /**
     * Returns true if the S component is "low", that means it is below {@link ECKey#HALF_CURVE_ORDER}. See <a
     * href="https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#Low_S_values_in_signatures">BIP62</a>.
     */
    public boolean isCanonical() {
        return s.compareTo(ECKey.HALF_CURVE_ORDER) <= 0;
    }

    /**
     * Will automatically adjust the S component to be less than or equal to half the curve order, if necessary.
     * This is required because for every signature (r,s) the signature (r, -s (mod N)) is a valid signature of
     * the same message. However, we dislike the ability to modify the bits of a Bitcoin transaction after it's
     * been signed, as that violates various assumed invariants. Thus in future only one of those forms will be
     * considered legal and the other will be banned.
     */
    public ECDSASignature toCanonicalised() {
        if(!isCanonical()) {
            // The order of the curve is the number of valid points that exist on that curve. If S is in the upper
            // half of the number of valid points, then bring it back to the lower half. Otherwise, imagine that
            //    N = 10
            //    s = 8, so (-8 % 10 == 2) thus both (r, 8) and (r, 2) are valid solutions.
            //    10 - 8 == 2, giving us always the latter solution, which is canonical.
            return new ECDSASignature(r, CURVE.getN().subtract(s));
        } else {
            return this;
        }
    }

    /**
     * DER is an international standard for serializing data structures which is widely used in cryptography.
     * It's somewhat like protocol buffers but less convenient. This method returns a standard DER encoding
     * of the signature, as recognized by OpenSSL and other libraries.
     */
    public byte[] encodeToDER() {
        try {
            return derByteStream().toByteArray();
        } catch(IOException e) {
            throw new RuntimeException(e);  // Cannot happen.
        }
    }

    /**
     * @throws SignatureDecodeException if the signature is unparseable in some way.
     */
    public static ECDSASignature decodeFromDER(byte[] bytes) throws SignatureDecodeException {
        ASN1InputStream decoder = null;
        try {
            // BouncyCastle by default is strict about parsing ASN.1 integers. We relax this check, because some
            // Bitcoin signatures would not parse.
            Properties.setThreadOverride("org.bouncycastle.asn1.allow_unsafe_integer", true);
            decoder = new ASN1InputStream(bytes);
            final ASN1Primitive seqObj = decoder.readObject();
            if(seqObj == null) {
                throw new SignatureDecodeException("Reached past end of ASN.1 stream.");
            }
            if(!(seqObj instanceof DLSequence)) {
                throw new SignatureDecodeException("Read unexpected class: " + seqObj.getClass().getName());
            }
            final DLSequence seq = (DLSequence) seqObj;
            ASN1Integer r, s;
            try {
                r = (ASN1Integer) seq.getObjectAt(0);
                s = (ASN1Integer) seq.getObjectAt(1);
            } catch(ClassCastException e) {
                throw new SignatureDecodeException(e);
            }
            // OpenSSL deviates from the DER spec by interpreting these values as unsigned, though they should not be
            // Thus, we always use the positive versions. See: http://r6.ca/blog/20111119T211504Z.html
            return new ECDSASignature(r.getPositiveValue(), s.getPositiveValue());
        } catch(IOException e) {
            throw new SignatureDecodeException(e);
        } finally {
            if(decoder != null) {
                try {
                    decoder.close();
                } catch(IOException x) {
                }
            }
            Properties.removeThreadOverride("org.bouncycastle.asn1.allow_unsafe_integer");
        }
    }

    /**
     * <p>Verifies the given ECDSA signature against the message bytes using the public key bytes.</p>
     *
     * <p>When using native ECDSA verification, data must be 32 bytes, and no element may be
     * larger than 520 bytes.</p>
     *
     * @param data      Hash of the data to verify.
     * @param pub       The public key bytes to use.
     */
    public boolean verify(byte[] data, byte[] pub) {
        ECDSASigner signer = new ECDSASigner();
        ECPublicKeyParameters params = new ECPublicKeyParameters(CURVE.getCurve().decodePoint(pub), CURVE);
        signer.init(false, params);
        try {
            return signer.verifySignature(data, r, s);
        } catch (NullPointerException e) {
            // Bouncy Castle contains a bug that can cause NPEs given specially crafted signatures. Those signatures
            // are inherently invalid/attack sigs so we just fail them here rather than crash the thread.
            return false;
        }
    }

    public ByteArrayOutputStream derByteStream() throws IOException {
        // Usually 70-72 bytes.
        ByteArrayOutputStream bos = new ByteArrayOutputStream(72);
        DERSequenceGenerator seq = new DERSequenceGenerator(bos);
        seq.addObject(new ASN1Integer(r));
        seq.addObject(new ASN1Integer(s));
        seq.close();
        return bos;
    }

    protected boolean hasLowR() {
        //A low R signature will have less than 71 bytes when encoded to DER
        return toCanonicalised().encodeToDER().length < 71;
    }

    @Override
    public boolean equals(Object o) {
        if(this == o) {
            return true;
        }
        if(o == null || getClass() != o.getClass()) {
            return false;
        }
        ECDSASignature other = (ECDSASignature) o;
        return r.equals(other.r) && s.equals(other.s);
    }

    @Override
    public int hashCode() {
        return Objects.hash(r, s);
    }

    /**
     * Returns a decoded signature.
     *
     * @param requireCanonicalEncoding if the encoding of the signature must
     * be canonical.
     * @param requireCanonicalSValue if the S-value must be canonical (below half
     * the order of the curve).
     * @throws SignatureDecodeException if the signature is unparseable in some way.
     * @throws VerificationException if the signature is invalid.
     */
    public static TransactionSignature decodeFromBitcoin(byte[] bytes, boolean requireCanonicalEncoding,
                                                         boolean requireCanonicalSValue) throws SignatureDecodeException, VerificationException {
        // Bitcoin encoding is DER signature + sighash byte.
        if (requireCanonicalEncoding && !isEncodingCanonical(bytes))
            throw new VerificationException.NoncanonicalSignature();
        ECDSASignature sig = ECDSASignature.decodeFromDER(bytes);
        if (requireCanonicalSValue && !sig.isCanonical())
            throw new VerificationException("S-value is not canonical.");

        // In Bitcoin, any value of the final byte is valid, but not necessarily canonical. See javadocs for
        // isEncodingCanonical to learn more about this. So we must store the exact byte found.
        return new TransactionSignature(sig.r, sig.s, TransactionSignature.Type.ECDSA, bytes[bytes.length - 1]);
    }

    /**
     * Returns true if the given signature is has canonical encoding, and will thus be accepted as standard by
     * Bitcoin Core. DER and the SIGHASH encoding allow for quite some flexibility in how the same structures
     * are encoded, and this can open up novel attacks in which a man in the middle takes a transaction and then
     * changes its signature such that the transaction hash is different but it's still valid. This can confuse wallets
     * and generally violates people's mental model of how Bitcoin should work, thus, non-canonical signatures are now
     * not relayed by default.
     */
    public static boolean isEncodingCanonical(byte[] signature) {
        // See Bitcoin Core's IsCanonicalSignature, https://bitcointalk.org/index.php?topic=8392.msg127623#msg127623
        // A canonical signature exists of: <30> <total len> <02> <len R> <R> <02> <len S> <S> <hashtype>
        // Where R and S are not negative (their first byte has its highest bit not set), and not
        // excessively padded (do not start with a 0 byte, unless an otherwise negative number follows,
        // in which case a single 0 byte is necessary and even required).

        // Empty signatures, while not strictly DER encoded, are allowed.
        if (signature.length == 0)
            return true;

        if (signature.length < 9 || signature.length > 73)
            return false;

        int hashType = (signature[signature.length-1] & 0xff) & ~SigHash.ANYONECANPAY.value; // mask the byte to prevent sign-extension hurting us
        if (hashType < SigHash.ALL.value || hashType > SigHash.SINGLE.value)
            return false;

        //                   "wrong type"                  "wrong length marker"
        if ((signature[0] & 0xff) != 0x30 || (signature[1] & 0xff) != signature.length-3)
            return false;

        int lenR = signature[3] & 0xff;
        if (5 + lenR >= signature.length || lenR == 0)
            return false;
        int lenS = signature[5+lenR] & 0xff;
        if (lenR + lenS + 7 != signature.length || lenS == 0)
            return false;

        //    R value type mismatch          R value negative
        if (signature[4-2] != 0x02 || (signature[4] & 0x80) == 0x80)
            return false;
        if (lenR > 1 && signature[4] == 0x00 && (signature[4+1] & 0x80) != 0x80)
            return false; // R value excessively padded

        //       S value type mismatch                    S value negative
        if (signature[6 + lenR - 2] != 0x02 || (signature[6 + lenR] & 0x80) == 0x80)
            return false;
        if (lenS > 1 && signature[6 + lenR] == 0x00 && (signature[6 + lenR + 1] & 0x80) != 0x80)
            return false; // S value excessively padded

        return true;
    }
}
