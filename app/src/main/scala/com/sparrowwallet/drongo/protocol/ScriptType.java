package com.sparrowwallet.drongo.protocol;

import com.sparrowwallet.drongo.KeyDerivation;
import com.sparrowwallet.drongo.Network;
import com.sparrowwallet.drongo.Utils;
import com.sparrowwallet.drongo.address.Address;
import com.sparrowwallet.drongo.address.P2PKAddress;
import com.sparrowwallet.drongo.address.P2PKHAddress;
import com.sparrowwallet.drongo.address.P2SHAddress;
import com.sparrowwallet.drongo.address.P2TRAddress;
import com.sparrowwallet.drongo.address.P2WPKHAddress;
import com.sparrowwallet.drongo.address.P2WSHAddress;
import com.sparrowwallet.drongo.crypto.ChildNumber;
import com.sparrowwallet.drongo.crypto.ECKey;
import com.sparrowwallet.drongo.policy.PolicyType;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.StringJoiner;
import java.util.stream.Collectors;

import static com.sparrowwallet.drongo.policy.PolicyType.CUSTOM;
import static com.sparrowwallet.drongo.policy.PolicyType.MULTI;
import static com.sparrowwallet.drongo.policy.PolicyType.SINGLE;
import static com.sparrowwallet.drongo.protocol.Script.decodeFromOpN;
import static com.sparrowwallet.drongo.protocol.ScriptOpCodes.OP_0;
import static com.sparrowwallet.drongo.protocol.ScriptOpCodes.OP_1;
import static com.sparrowwallet.drongo.protocol.ScriptOpCodes.OP_CHECKMULTISIG;
import static com.sparrowwallet.drongo.protocol.ScriptOpCodes.OP_CHECKMULTISIGVERIFY;
import static com.sparrowwallet.drongo.protocol.ScriptOpCodes.OP_CHECKSIG;
import static com.sparrowwallet.drongo.protocol.ScriptOpCodes.OP_DUP;
import static com.sparrowwallet.drongo.protocol.ScriptOpCodes.OP_EQUAL;
import static com.sparrowwallet.drongo.protocol.ScriptOpCodes.OP_EQUALVERIFY;
import static com.sparrowwallet.drongo.protocol.ScriptOpCodes.OP_HASH160;
import static com.sparrowwallet.drongo.protocol.Transaction.WITNESS_SCALE_FACTOR;

public enum ScriptType {
    P2PK("P2PK", "Legacy (P2PK)", "m/44'/0'/0'") {
        @Override
        public Address getAddress(byte[] pubKey) {
            return new P2PKAddress(pubKey);
        }

        @Override
        public Address getAddress(ECKey key) {
            return getAddress(key.getPubKey());
        }

        @Override
        public Address getAddress(Script script) {
            throw new ProtocolException("No script derived address for non pay to script type");
        }

        @Override
        public Address[] getAddresses(Script script) {
            return new Address[] { getAddress(getPublicKeyFromScript(script).getPubKey()) };
        }

        @Override
        public Script getOutputScript(byte[] pubKey) {
            List<ScriptChunk> chunks = new ArrayList<>();
            chunks.add(new ScriptChunk(pubKey.length, pubKey));
            chunks.add(new ScriptChunk(ScriptOpCodes.OP_CHECKSIG, null));

            return new Script(chunks);
        }

        @Override
        public Script getOutputScript(ECKey key) {
            return getOutputScript(key.getPubKey());
        }

        @Override
        public Script getOutputScript(Script script) {
            throw new ProtocolException("No script derived output script for non pay to script type");
        }

        @Override
        public String getOutputDescriptor(ECKey key) {
            return getDescriptor() + Utils.bytesToHex(key.getPubKey()) + getCloseDescriptor();
        }

        @Override
        public String getOutputDescriptor(Script script) {
            throw new ProtocolException("No script derived output descriptor for non pay to script type");
        }

        @Override
        public String getDescriptor() {
            return "pk(";
        }

        @Override
        public boolean isScriptType(Script script) {
            List<ScriptChunk> chunks = script.chunks;
            if (chunks.size() != 2)
                return false;
            if (!chunks.get(0).equalsOpCode(0x21) && !chunks.get(0).equalsOpCode(0x41))
                return false;
            byte[] chunk2data = chunks.get(0).data;
            if (chunk2data == null)
                return false;
            if (chunk2data.length != 33 && chunk2data.length != 65)
                return false;
            if (!chunks.get(1).equalsOpCode(OP_CHECKSIG))
                return false;
            return true;
        }

        @Override
        public byte[] getHashFromScript(Script script) {
            throw new ProtocolException("P2PK script does contain hash, use getPublicKeyFromScript(script) to retreive public key");
        }

        @Override
        public ECKey getPublicKeyFromScript(Script script) {
            return ECKey.fromPublicOnly(script.chunks.get(0).data);
        }

        @Override
        public Script getScriptSig(Script scriptPubKey, ECKey pubKey, TransactionSignature signature) {
            if(!isScriptType(scriptPubKey)) {
                throw new ProtocolException("Provided scriptPubKey is not a " + getName() + " script");
            }

            byte[] signatureBytes = signature.encodeToBitcoin();
            ScriptChunk signatureChunk = ScriptChunk.fromData(signatureBytes);
            return new Script(Collections.singletonList(signatureChunk));
        }

        @Override
        public TransactionInput addSpendingInput(Transaction transaction, TransactionOutput prevOutput, ECKey pubKey, TransactionSignature signature) {
            Script scriptSig = getScriptSig(prevOutput.getScript(), pubKey, signature);
            return transaction.addInput(prevOutput.getHash(), prevOutput.getIndex(), scriptSig);
        }

        @Override
        public Script getMultisigScriptSig(Script scriptPubKey, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures) {
            throw new ProtocolException(getName() + " is not a multisig script type");
        }

        @Override
        public TransactionInput addMultisigSpendingInput(Transaction transaction, TransactionOutput prevOutput, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures) {
            throw new ProtocolException(getName() + " is not a multisig script type");
        }

        @Override
        public TransactionSignature.Type getSignatureType() {
            return TransactionSignature.Type.ECDSA;
        };

        @Override
        public List<PolicyType> getAllowedPolicyTypes() {
            return Collections.singletonList(SINGLE);
        }
    },
    P2PKH("P2PKH", "Legacy (P2PKH)", "m/44'/0'/0'") {
        @Override
        public Address getAddress(byte[] pubKeyHash) {
            return new P2PKHAddress(pubKeyHash);
        }

        @Override
        public Address getAddress(ECKey key) {
            return getAddress(key.getPubKeyHash());
        }

        @Override
        public Address getAddress(Script script) {
            throw new ProtocolException("No script derived address for non pay to script type");
        }

        @Override
        public Script getOutputScript(byte[] pubKeyHash) {
            List<ScriptChunk> chunks = new ArrayList<>();
            chunks.add(new ScriptChunk(ScriptOpCodes.OP_DUP, null));
            chunks.add(new ScriptChunk(ScriptOpCodes.OP_HASH160, null));
            chunks.add(new ScriptChunk(pubKeyHash.length, pubKeyHash));
            chunks.add(new ScriptChunk(ScriptOpCodes.OP_EQUALVERIFY, null));
            chunks.add(new ScriptChunk(ScriptOpCodes.OP_CHECKSIG, null));

            return new Script(chunks);
        }

        @Override
        public Script getOutputScript(ECKey key) {
            return getOutputScript(key.getPubKeyHash());
        }

        @Override
        public Script getOutputScript(Script script) {
            throw new ProtocolException("No script derived output script for non pay to script type");
        }

        @Override
        public String getOutputDescriptor(ECKey key) {
            return getDescriptor() + Utils.bytesToHex(key.getPubKey()) + getCloseDescriptor();
        }

        @Override
        public String getOutputDescriptor(Script script) {
            throw new ProtocolException("No script derived output descriptor for non pay to script type");
        }

        @Override
        public String getDescriptor() {
            return "pkh(";
        }

        @Override
        public boolean isScriptType(Script script) {
            List<ScriptChunk> chunks = script.chunks;
            if (chunks.size() != 5)
                return false;
            if (!chunks.get(0).equalsOpCode(OP_DUP))
                return false;
            if (!chunks.get(1).equalsOpCode(OP_HASH160))
                return false;
            byte[] chunk2data = chunks.get(2).data;
            if (chunk2data == null)
                return false;
            if (chunk2data.length != 20)
                return false;
            if (!chunks.get(3).equalsOpCode(OP_EQUALVERIFY))
                return false;
            if (!chunks.get(4).equalsOpCode(OP_CHECKSIG))
                return false;
            return true;
        }

        @Override
        public byte[] getHashFromScript(Script script) {
            return script.chunks.get(2).data;
        }

        @Override
        public Script getScriptSig(Script scriptPubKey, ECKey pubKey, TransactionSignature signature) {
            if(!isScriptType(scriptPubKey)) {
                throw new ProtocolException("Provided scriptPubKey is not a " + getName() + " script");
            }

            byte[] signatureBytes = signature.encodeToBitcoin();
            ScriptChunk signatureChunk = ScriptChunk.fromData(signatureBytes);
            byte[] pubKeyBytes = pubKey.getPubKey();
            ScriptChunk pubKeyChunk = ScriptChunk.fromData(pubKeyBytes);
            return new Script(Arrays.asList(signatureChunk, pubKeyChunk));
        }

        @Override
        public TransactionInput addSpendingInput(Transaction transaction, TransactionOutput prevOutput, ECKey pubKey, TransactionSignature signature) {
            Script scriptSig = getScriptSig(prevOutput.getScript(), pubKey, signature);
            return transaction.addInput(prevOutput.getHash(), prevOutput.getIndex(), scriptSig);
        }

        @Override
        public Script getMultisigScriptSig(Script scriptPubKey, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures) {
            throw new ProtocolException(getName() + " is not a multisig script type");
        }

        @Override
        public TransactionInput addMultisigSpendingInput(Transaction transaction, TransactionOutput prevOutput, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures) {
            throw new ProtocolException(getName() + " is not a multisig script type");
        }

        @Override
        public TransactionSignature.Type getSignatureType() {
            return TransactionSignature.Type.ECDSA;
        };

        @Override
        public List<PolicyType> getAllowedPolicyTypes() {
            return Collections.singletonList(SINGLE);
        }
    },
    MULTISIG("Bare Multisig", "Bare Multisig", "m/44'/0'/0'") {
        @Override
        public Address getAddress(byte[] bytes) {
            throw new ProtocolException("No single address for multisig script type");
        }

        @Override
        public Address getAddress(Script script) {
            throw new ProtocolException("No single address for multisig script type");
        }

        @Override
        public Address getAddress(ECKey key) {
            throw new ProtocolException("No single key address for multisig script type");
        }

        @Override
        public Address[] getAddresses(Script script) {
            ECKey[] pubKeys = getPublicKeysFromScript(script);
            Address[] addresses = new Address[pubKeys.length];

            for (int i = 0; i < pubKeys.length; i++) {
                addresses[i] = new P2PKAddress(pubKeys[i].getPubKey());
            }

            return addresses;
        }

        @Override
        public Script getOutputScript(byte[] bytes) {
            throw new ProtocolException("Output script for multisig script type must be constructed with method getOutputScript(int threshold, List<ECKey> pubKeys)");
        }

        @Override
        public Script getOutputScript(ECKey key) {
            throw new ProtocolException("Output script for multisig script type must be constructed with method getOutputScript(int threshold, List<ECKey> pubKeys)");
        }

        @Override
        public Script getOutputScript(Script script) {
            if(isScriptType(script)) {
                return script;
            }

            throw new ProtocolException("No script derived output script for non pay to script type");
        }

        @Override
        public Script getOutputScript(int threshold, Collection<ECKey> pubKeys) {
            if(threshold > pubKeys.size()) {
                throw new ProtocolException("Threshold of " + threshold + " is greater than number of pubKeys provided (" + pubKeys.size() + ")");
            }

            List<byte[]> pubKeyBytes = new ArrayList<>();
            for(ECKey key : pubKeys) {
                pubKeyBytes.add(key.getPubKey());
            }

            Collections.sort(pubKeyBytes, new Utils.LexicographicByteArrayComparator());

            List<ScriptChunk> chunks = new ArrayList<>();
            chunks.add(new ScriptChunk(Script.encodeToOpN(threshold), null));
            for(byte[] pubKey : pubKeyBytes) {
                chunks.add(new ScriptChunk(pubKey.length, pubKey));
            }
            chunks.add(new ScriptChunk(Script.encodeToOpN(pubKeys.size()), null));
            chunks.add(new ScriptChunk(ScriptOpCodes.OP_CHECKMULTISIG, null));
            return new Script(chunks);
        }

        @Override
        public String getOutputDescriptor(ECKey key) {
            throw new ProtocolException("No single key output descriptor for multisig script type");
        }

        @Override
        public String getOutputDescriptor(Script script) {
            if(!isScriptType(script)) {
                throw new IllegalArgumentException("Can only create output descriptor from multisig script");
            }

            int threshold = getThreshold(script);
            ECKey[] pubKeys = getPublicKeysFromScript(script);

            List<byte[]> pubKeyBytes = new ArrayList<>();
            for(ECKey key : pubKeys) {
                pubKeyBytes.add(key.getPubKey());
            }

            Collections.sort(pubKeyBytes, new Utils.LexicographicByteArrayComparator());

            StringBuilder joiner = new StringBuilder();
            boolean first = true;
            for (byte[] pubKey : pubKeyBytes) {
                if (!first) {
                    joiner.append(",");
                }
                joiner.append(Utils.bytesToHex(pubKey));
                first = false;
            }

            return getDescriptor() + threshold + "," + joiner.toString() + getCloseDescriptor();
        }

        @Override
        public String getDescriptor() {
            return "sortedmulti(";
        }

        @Override
        public boolean isScriptType(Script script) {
            List<ScriptChunk> chunks = script.chunks;
            if (chunks.size() < 4) return false;
            ScriptChunk chunk = chunks.get(chunks.size() - 1);
            // Must end in OP_CHECKMULTISIG[VERIFY].
            if (!chunk.isOpCode()) return false;
            if (!(chunk.equalsOpCode(OP_CHECKMULTISIG) || chunk.equalsOpCode(OP_CHECKMULTISIGVERIFY))) return false;
            try {
                // Second to last chunk must be an OP_N opcode and there should be that many data chunks (keys).
                ScriptChunk m = chunks.get(chunks.size() - 2);
                if (!m.isOpCode()) return false;
                int numKeys = Script.decodeFromOpN(m.opcode);
                if (numKeys < 1 || chunks.size() != 3 + numKeys) return false;
                for (int i = 1; i < chunks.size() - 2; i++) {
                    if (chunks.get(i).isOpCode()) return false;
                }
                // First chunk must be an OP_N opcode too.
                if (Script.decodeFromOpN(chunks.get(0).opcode) < 1) return false;
            } catch (IllegalStateException e) {
                return false;   // Not an OP_N opcode.
            }
            return true;
        }

        @Override
        public byte[] getHashFromScript(Script script) {
            throw new ProtocolException("Public keys for bare multisig script type must be retrieved with method getPublicKeysFromScript(Script script)");
        }

        @Override
        public ECKey[] getPublicKeysFromScript(Script script) {
            List<ECKey> pubKeys = new ArrayList<>();

            List<ScriptChunk> chunks = script.chunks;
            for (int i = 1; i < chunks.size() - 2; i++) {
                byte[] pubKey = chunks.get(i).data;
                pubKeys.add(ECKey.fromPublicOnly(pubKey));
            }

            return pubKeys.toArray(new ECKey[pubKeys.size()]);
        }

        @Override
        public int getThreshold(Script script) {
            return decodeFromOpN(script.chunks.get(0).opcode);
        }

        @Override
        public Script getScriptSig(Script scriptPubKey, ECKey pubKey, TransactionSignature signature) {
            throw new ProtocolException(getName() + " is a multisig script type");
        }

        @Override
        public TransactionInput addSpendingInput(Transaction transaction, TransactionOutput prevOutput, ECKey pubKey, TransactionSignature signature) {
            throw new ProtocolException(getName() + " is a multisig script type");
        }

        @Override
        public Script getMultisigScriptSig(Script scriptPubKey, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures) {
            if(!isScriptType(scriptPubKey)) {
                throw new ProtocolException("Provided scriptPubKey is not a " + getName() + " script");
            }

            List<TransactionSignature> signatures = new ArrayList<>();
            Collection<TransactionSignature> values = pubKeySignatures.values();
            for (TransactionSignature signature : values) {
                if (signature != null) {
                    signatures.add(signature);
                }
            }

            if(signatures.size() < threshold) {
                throw new ProtocolException("Only " + signatures.size() + " signatures provided to meet a multisig threshold of " + threshold);
            }

            List<ScriptChunk> chunks = new ArrayList<>(signatures.size() + 1);
            ScriptChunk opZero = ScriptChunk.fromOpcode(OP_0);
            chunks.add(opZero);
            for(TransactionSignature signature : signatures) {
                byte[] signatureBytes = signature.encodeToBitcoin();
                chunks.add(ScriptChunk.fromData(signatureBytes));
            }

            return new Script(chunks);
        }

        @Override
        public TransactionInput addMultisigSpendingInput(Transaction transaction, TransactionOutput prevOutput, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures) {
            Script scriptSig = getMultisigScriptSig(prevOutput.getScript(), threshold, pubKeySignatures);
            return transaction.addInput(prevOutput.getHash(), prevOutput.getIndex(), scriptSig);
        }

        @Override
        public TransactionSignature.Type getSignatureType() {
            return TransactionSignature.Type.ECDSA;
        };

        @Override
        public List<PolicyType> getAllowedPolicyTypes() {
            return Collections.singletonList(MULTI);
        }
    },
    P2SH("P2SH", "Legacy (P2SH)", "m/45'") {
        @Override
        public Address getAddress(byte[] scriptHash) {
            return new P2SHAddress(scriptHash);
        }

        @Override
        public Address getAddress(ECKey key) {
            throw new ProtocolException("No single key address for script hash type");
        }

        @Override
        public Address getAddress(Script script) {
            return getAddress(Utils.sha256hash160(script.getProgram()));
        }

        @Override
        public Script getOutputScript(byte[] scriptHash) {
            List<ScriptChunk> chunks = new ArrayList<>();
            chunks.add(new ScriptChunk(ScriptOpCodes.OP_HASH160, null));
            chunks.add(new ScriptChunk(scriptHash.length, scriptHash));
            chunks.add(new ScriptChunk(ScriptOpCodes.OP_EQUAL, null));

            return new Script(chunks);
        }

        @Override
        public Script getOutputScript(ECKey key) {
            throw new ProtocolException("No single key output script for script hash type");
        }

        @Override
        public Script getOutputScript(Script script) {
            return getOutputScript(Utils.sha256hash160(script.getProgram()));
        }

        @Override
        public String getOutputDescriptor(ECKey key) {
            throw new ProtocolException("No single key output descriptor for script hash type");
        }

        @Override
        public String getOutputDescriptor(Script script) {
            if(!MULTISIG.isScriptType(script)) {
                throw new IllegalArgumentException("Can only create output descriptor from multisig script");
            }

            return getDescriptor() + MULTISIG.getOutputDescriptor(script) + getCloseDescriptor();
        }

        @Override
        public String getDescriptor() {
            return "sh(";
        }

        @Override
        public boolean isScriptType(Script script) {
            List<ScriptChunk> chunks = script.chunks;
            // We check for the effective serialized form because BIP16 defines a P2SH output using an exact byte
            // template, not the logical program structure. Thus you can have two programs that look identical when
            // printed out but one is a P2SH script and the other isn't! :(
            // We explicitly test that the op code used to load the 20 bytes is 0x14 and not something logically
            // equivalent like {@code OP_HASH160 OP_PUSHDATA1 0x14 <20 bytes of script hash> OP_EQUAL}
            if (chunks.size() != 3)
                return false;
            if (!chunks.get(0).equalsOpCode(OP_HASH160))
                return false;
            ScriptChunk chunk1 = chunks.get(1);
            if (chunk1.opcode != 0x14)
                return false;
            byte[] chunk1data = chunk1.data;
            if (chunk1data == null)
                return false;
            if (chunk1data.length != 20)
                return false;
            if (!chunks.get(2).equalsOpCode(OP_EQUAL))
                return false;
            return true;
        }

        @Override
        public byte[] getHashFromScript(Script script) {
            return script.chunks.get(1).data;
        }

        @Override
        public Script getScriptSig(Script scriptPubKey, ECKey pubKey, TransactionSignature signature) {
            throw new ProtocolException("Only multisig scriptSigs supported for " + getName() + " scriptPubKeys");
        }

        @Override
        public TransactionInput addSpendingInput(Transaction transaction, TransactionOutput prevOutput, ECKey pubKey, TransactionSignature signature) {
            throw new ProtocolException("Only multisig scriptSigs supported for " + getName() + " scriptPubKeys");
        }

        @Override
        public Script getMultisigScriptSig(Script scriptPubKey, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures) {
            if(!isScriptType(scriptPubKey)) {
                throw new ProtocolException("Provided scriptPubKey is not a " + getName() + " script");
            }

            Script redeemScript = MULTISIG.getOutputScript(threshold, pubKeySignatures.keySet());
            if(!scriptPubKey.equals(getOutputScript(redeemScript))) {
                throw new ProtocolException("P2SH scriptPubKey hash does not match constructed redeem script hash");
            }

            Script multisigScript = MULTISIG.getMultisigScriptSig(redeemScript, threshold, pubKeySignatures);
            List<ScriptChunk> chunks = new ArrayList<>(multisigScript.getChunks());
            ScriptChunk redeemScriptChunk = ScriptChunk.fromData(redeemScript.getProgram());
            chunks.add(redeemScriptChunk);

            return new Script(chunks);
        }

        @Override
        public TransactionInput addMultisigSpendingInput(Transaction transaction, TransactionOutput prevOutput, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures) {
            Script scriptSig = getMultisigScriptSig(prevOutput.getScript(), threshold, pubKeySignatures);
            return transaction.addInput(prevOutput.getHash(), prevOutput.getIndex(), scriptSig);
        }

        @Override
        public TransactionSignature.Type getSignatureType() {
            return TransactionSignature.Type.ECDSA;
        };

        @Override
        public List<PolicyType> getAllowedPolicyTypes() {
            return Collections.singletonList(MULTI);
        }
    },
    P2SH_P2WPKH("P2SH-P2WPKH", "Nested Segwit (P2SH-P2WPKH)", "m/49'/0'/0'") {
        @Override
        public Address getAddress(byte[] scriptHash) {
            return P2SH.getAddress(scriptHash);
        }

        @Override
        public Address getAddress(ECKey key) {
            Script p2wpkhScript = P2WPKH.getOutputScript(key.getPubKeyHash());
            return P2SH.getAddress(p2wpkhScript);
        }

        @Override
        public Address getAddress(Script script) {
            if(P2WPKH.isScriptType(script)) {
                return P2SH.getAddress(script);
            }

            throw new ProtocolException("Provided script is not a P2WPKH script");
        }

        @Override
        public Script getOutputScript(byte[] scriptHash) {
            return P2SH.getOutputScript(scriptHash);
        }

        @Override
        public Script getOutputScript(ECKey key) {
            Script p2wpkhScript = P2WPKH.getOutputScript(key.getPubKeyHash());
            return P2SH.getOutputScript(p2wpkhScript);
        }

        @Override
        public Script getOutputScript(Script script) {
            if(P2WPKH.isScriptType(script)) {
                return P2SH.getOutputScript(script);
            }

            throw new ProtocolException("Provided script is not a P2WPKH script");
        }

        @Override
        public String getOutputDescriptor(ECKey key) {
            return getDescriptor() + Utils.bytesToHex(key.getPubKey()) + getCloseDescriptor();
        }

        @Override
        public String getOutputDescriptor(Script script) {
            throw new ProtocolException("No script derived output descriptor for non pay to script type");
        }

        @Override
        public String getDescriptor() {
            return "sh(wpkh(";
        }

        @Override
        public boolean isScriptType(Script script) {
            return P2SH.isScriptType(script);
        }

        @Override
        public byte[] getHashFromScript(Script script) {
            return P2SH.getHashFromScript(script);
        }

        @Override
        public Script getScriptSig(Script scriptPubKey, ECKey pubKey, TransactionSignature signature) {
            if(!isScriptType(scriptPubKey)) {
                throw new ProtocolException("Provided scriptPubKey is not a " + getName() + " script");
            }

            Script redeemScript = P2WPKH.getOutputScript(pubKey);
            if(!scriptPubKey.equals(P2SH.getOutputScript(redeemScript))) {
                throw new ProtocolException(getName() + " scriptPubKey hash does not match constructed redeem script hash");
            }

            ScriptChunk redeemScriptChunk = ScriptChunk.fromData(redeemScript.getProgram());
            return new Script(Collections.singletonList(redeemScriptChunk));
        }

        @Override
        public TransactionInput addSpendingInput(Transaction transaction, TransactionOutput prevOutput, ECKey pubKey, TransactionSignature signature) {
            Script scriptSig = getScriptSig(prevOutput.getScript(), pubKey, signature);
            TransactionWitness witness = new TransactionWitness(transaction, pubKey, signature);
            return transaction.addInput(prevOutput.getHash(), prevOutput.getIndex(), scriptSig, witness);
        }

        @Override
        public Script getMultisigScriptSig(Script scriptPubKey, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures) {
            throw new ProtocolException(getName() + " is not a multisig script type");
        }

        @Override
        public TransactionInput addMultisigSpendingInput(Transaction transaction, TransactionOutput prevOutput, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures) {
            throw new ProtocolException(getName() + " is not a multisig script type");
        }

        @Override
        public TransactionSignature.Type getSignatureType() {
            return TransactionSignature.Type.ECDSA;
        };

        @Override
        public List<PolicyType> getAllowedPolicyTypes() {
            return Collections.singletonList(SINGLE);
        }
    },
    P2SH_P2WSH("P2SH-P2WSH", "Nested Segwit (P2SH-P2WSH)", "m/48'/0'/0'/1'") {
        @Override
        public Address getAddress(byte[] scriptHash) {
            return P2SH.getAddress(scriptHash);
        }

        @Override
        public Address getAddress(ECKey key) {
            throw new ProtocolException("No single key address for wrapped witness script hash type");
        }

        @Override
        public Address getAddress(Script script) {
            Script p2wshScript = P2WSH.getOutputScript(script);
            return P2SH.getAddress(p2wshScript);
        }

        @Override
        public Script getOutputScript(byte[] scriptHash) {
            return P2SH.getOutputScript(scriptHash);
        }

        @Override
        public Script getOutputScript(ECKey key) {
            throw new ProtocolException("No single key output script for wrapped witness script hash type");
        }

        @Override
        public Script getOutputScript(Script script) {
            Script p2wshScript = P2WSH.getOutputScript(script);
            return P2SH.getOutputScript(p2wshScript);
        }

        @Override
        public String getOutputDescriptor(ECKey key) {
            throw new ProtocolException("No single key output descriptor for script hash type");
        }

        @Override
        public String getOutputDescriptor(Script script) {
            if(!MULTISIG.isScriptType(script)) {
                throw new IllegalArgumentException("Can only create output descriptor from multisig script");
            }

            return getDescriptor() + MULTISIG.getOutputDescriptor(script) + getCloseDescriptor();
        }

        @Override
        public String getDescriptor() {
            return "sh(wsh(";
        }

        @Override
        public boolean isScriptType(Script script) {
            return P2SH.isScriptType(script);
        }

        @Override
        public byte[] getHashFromScript(Script script) {
            return P2SH.getHashFromScript(script);
        }

        @Override
        public Script getScriptSig(Script scriptPubKey, ECKey pubKey, TransactionSignature signature) {
            throw new ProtocolException("Only multisig scriptSigs supported for " + getName() + " scriptPubKeys");
        }

        @Override
        public TransactionInput addSpendingInput(Transaction transaction, TransactionOutput prevOutput, ECKey pubKey, TransactionSignature signature) {
            throw new ProtocolException("Only multisig scriptSigs supported for " + getName() + " scriptPubKeys");
        }

        @Override
        public Script getMultisigScriptSig(Script scriptPubKey, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures) {
            if(!isScriptType(scriptPubKey)) {
                throw new ProtocolException("Provided scriptPubKey is not a " + getName() + " script");
            }

            Script witnessScript = MULTISIG.getOutputScript(threshold, pubKeySignatures.keySet());
            Script redeemScript = P2WSH.getOutputScript(witnessScript);
            if(!scriptPubKey.equals(P2SH.getOutputScript(redeemScript))) {
                throw new ProtocolException("P2SH scriptPubKey hash does not match constructed redeem script hash");
            }

            ScriptChunk redeemScriptChunk = ScriptChunk.fromData(redeemScript.getProgram());
            return new Script(Collections.singletonList(redeemScriptChunk));
        }

        @Override
        public TransactionInput addMultisigSpendingInput(Transaction transaction,
                                                         TransactionOutput prevOutput,
                                                         int threshold,
                                                         Map<ECKey, TransactionSignature> pubKeySignatures) {
            Script scriptSig = getMultisigScriptSig(prevOutput.getScript(), threshold, pubKeySignatures);
            Script witnessScript = MULTISIG.getOutputScript(threshold, pubKeySignatures.keySet());

            List<TransactionSignature> filteredSignatures = new ArrayList<>();
            for (TransactionSignature signature : pubKeySignatures.values()) {
                if (signature != null) {
                    filteredSignatures.add(signature);
                }
            }

            TransactionWitness witness = new TransactionWitness(transaction, filteredSignatures, witnessScript);
            return transaction.addInput(prevOutput.getHash(), prevOutput.getIndex(), scriptSig, witness);
        }

        @Override
        public TransactionSignature.Type getSignatureType() {
            return TransactionSignature.Type.ECDSA;
        };

        @Override
        public List<PolicyType> getAllowedPolicyTypes() {
            return Arrays.asList(MULTI, CUSTOM);
        }
    },
    P2WPKH("P2WPKH", "Native Segwit (P2WPKH)", "m/84'/0'/0'") {
        @Override
        public Address getAddress(byte[] pubKeyHash) {
            return new P2WPKHAddress(pubKeyHash);
        }

        @Override
        public Address getAddress(ECKey key) {
            return getAddress(key.getPubKeyHash());
        }

        @Override
        public Address getAddress(Script script) {
            throw new ProtocolException("No script derived address for non pay to script type");
        }

        @Override
        public Script getOutputScript(byte[] pubKeyHash) {
            List<ScriptChunk> chunks = new ArrayList<>();
            chunks.add(new ScriptChunk(OP_0, null));
            chunks.add(new ScriptChunk(pubKeyHash.length, pubKeyHash));

            return new Script(chunks);
        }

        @Override
        public Script getOutputScript(ECKey key) {
            return getOutputScript(key.getPubKeyHash());
        }

        @Override
        public Script getOutputScript(Script script) {
            throw new ProtocolException("No script derived output script for non pay to script type");
        }

        @Override
        public String getOutputDescriptor(ECKey key) {
            return getDescriptor() + Utils.bytesToHex(key.getPubKey()) + getCloseDescriptor();
        }

        @Override
        public String getOutputDescriptor(Script script) {
            throw new ProtocolException("No script derived output descriptor for non pay to script type");
        }

        @Override
        public String getDescriptor() {
            return "wpkh(";
        }

        @Override
        public boolean isScriptType(Script script) {
            List<ScriptChunk> chunks = script.chunks;
            if (chunks.size() != 2)
                return false;
            if (!chunks.get(0).equalsOpCode(OP_0))
                return false;
            byte[] chunk1data = chunks.get(1).data;
            if (chunk1data == null)
                return false;
            if (chunk1data.length != 20)
                return false;
            return true;
        }

        @Override
        public byte[] getHashFromScript(Script script) {
            return script.chunks.get(1).data;
        }

        @Override
        public Script getScriptSig(Script scriptPubKey, ECKey pubKey, TransactionSignature signature) {
            if(!isScriptType(scriptPubKey)) {
                throw new ProtocolException("Provided scriptPubKey is not a " + getName() + " script");
            }

            if(!scriptPubKey.equals(getOutputScript(pubKey))) {
                throw new ProtocolException("P2WPKH scriptPubKey hash does not match constructed pubkey script hash");
            }

            return new Script(new byte[0]);
        }

        @Override
        public TransactionInput addSpendingInput(Transaction transaction, TransactionOutput prevOutput, ECKey pubKey, TransactionSignature signature) {
            Script scriptSig = getScriptSig(prevOutput.getScript(), pubKey, signature);
            TransactionWitness witness = new TransactionWitness(transaction, pubKey, signature);
            return transaction.addInput(prevOutput.getHash(), prevOutput.getIndex(), scriptSig, witness);
        }

        @Override
        public Script getMultisigScriptSig(Script scriptPubKey, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures) {
            throw new ProtocolException(getName() + " is not a multisig script type");
        }

        @Override
        public TransactionInput addMultisigSpendingInput(Transaction transaction, TransactionOutput prevOutput, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures) {
            throw new ProtocolException(getName() + " is not a multisig script type");
        }

        @Override
        public TransactionSignature.Type getSignatureType() {
            return TransactionSignature.Type.ECDSA;
        };

        @Override
        public List<PolicyType> getAllowedPolicyTypes() {
            return Collections.singletonList(SINGLE);
        }
    },
    P2WSH("P2WSH", "Native Segwit (P2WSH)", "m/48'/0'/0'/2'") {
        @Override
        public Address getAddress(byte[] scriptHash) {
            return new P2WSHAddress(scriptHash);
        }

        @Override
        public Address getAddress(ECKey key) {
            throw new ProtocolException("No single key address for witness script hash type");
        }

        @Override
        public Address getAddress(Script script) {
            return getAddress(Sha256Hash.hash(script.getProgram()));
        }

        @Override
        public Script getOutputScript(byte[] scriptHash) {
            List<ScriptChunk> chunks = new ArrayList<>();
            chunks.add(new ScriptChunk(OP_0, null));
            chunks.add(new ScriptChunk(scriptHash.length, scriptHash));

            return new Script(chunks);
        }

        @Override
        public Script getOutputScript(ECKey key) {
            throw new ProtocolException("No single key output script for witness script hash type");
        }

        @Override
        public Script getOutputScript(Script script) {
            return getOutputScript(Sha256Hash.hash(script.getProgram()));
        }

        @Override
        public String getOutputDescriptor(ECKey key) {
            throw new ProtocolException("No single key output descriptor for script hash type");
        }

        @Override
        public String getOutputDescriptor(Script script) {
            if(!MULTISIG.isScriptType(script)) {
                throw new IllegalArgumentException("Can only create output descriptor from multisig script");
            }

            return getDescriptor() + MULTISIG.getOutputDescriptor(script) + getCloseDescriptor();
        }

        @Override
        public String getDescriptor() {
            return "wsh(";
        }

        @Override
        public boolean isScriptType(Script script) {
            List<ScriptChunk> chunks = script.chunks;
            if (chunks.size() != 2)
                return false;
            if (!chunks.get(0).equalsOpCode(OP_0))
                return false;
            byte[] chunk1data = chunks.get(1).data;
            if (chunk1data == null)
                return false;
            if (chunk1data.length != 32)
                return false;
            return true;
        }

        @Override
        public byte[] getHashFromScript(Script script) {
            return script.chunks.get(1).data;
        }

        @Override
        public Script getScriptSig(Script scriptPubKey, ECKey pubKey, TransactionSignature signature) {
            throw new ProtocolException("Only multisig scriptSigs supported for " + getName() + " scriptPubKeys");
        }

        @Override
        public TransactionInput addSpendingInput(Transaction transaction, TransactionOutput prevOutput, ECKey pubKey, TransactionSignature signature) {
            throw new ProtocolException("Only multisig scriptSigs supported for " + getName() + " scriptPubKeys");
        }

        @Override
        public Script getMultisigScriptSig(Script scriptPubKey, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures) {
            if(!isScriptType(scriptPubKey)) {
                throw new ProtocolException("Provided scriptPubKey is not a " + getName() + " script");
            }

            Script witnessScript = MULTISIG.getOutputScript(threshold, pubKeySignatures.keySet());
            if(!scriptPubKey.equals(P2WSH.getOutputScript(witnessScript))) {
                throw new ProtocolException("P2WSH scriptPubKey hash does not match constructed witness script hash");
            }

            return new Script(new byte[0]);
        }

        @Override
        public TransactionInput addMultisigSpendingInput(Transaction transaction,
                                                         TransactionOutput prevOutput,
                                                         int threshold,
                                                         Map<ECKey, TransactionSignature> pubKeySignatures) {
            Script scriptSig = getMultisigScriptSig(prevOutput.getScript(), threshold, pubKeySignatures);
            Script witnessScript = MULTISIG.getOutputScript(threshold, pubKeySignatures.keySet());

            List<TransactionSignature> nonNullSignatures = new ArrayList<>();
            for (TransactionSignature signature : pubKeySignatures.values()) {
                if (signature != null) {
                    nonNullSignatures.add(signature);
                }
            }

            TransactionWitness witness = new TransactionWitness(transaction, nonNullSignatures, witnessScript);
            return transaction.addInput(prevOutput.getHash(), prevOutput.getIndex(), scriptSig, witness);
        }

        @Override
        public TransactionSignature.Type getSignatureType() {
            return TransactionSignature.Type.ECDSA;
        };

        @Override
        public List<PolicyType> getAllowedPolicyTypes() {
            return Arrays.asList(MULTI, CUSTOM);
        }
    },
    P2TR("P2TR", "Taproot (P2TR)", "m/86'/0'/0'") {
        @Override
        public ECKey getOutputKey(ECKey derivedKey) {
            return derivedKey.getTweakedOutputKey();
        }

        @Override
        public Address getAddress(byte[] pubKey) {
            return new P2TRAddress(pubKey);
        }

        @Override
        public Address getAddress(ECKey derivedKey) {
            return getAddress(getOutputKey(derivedKey).getPubKeyXCoord());
        }

        @Override
        public Address getAddress(Script script) {
            throw new ProtocolException("Cannot create a taproot address without a keypath");
        }

        @Override
        public Script getOutputScript(byte[] pubKey) {
            List<ScriptChunk> chunks = new ArrayList<>();
            chunks.add(new ScriptChunk(OP_1, null));
            chunks.add(new ScriptChunk(pubKey.length, pubKey));

            return new Script(chunks);
        }

        @Override
        public Script getOutputScript(ECKey derivedKey) {
            return getOutputScript(getOutputKey(derivedKey).getPubKeyXCoord());
        }

        @Override
        public Script getOutputScript(Script script) {
            throw new ProtocolException("Cannot create a taproot output script without a keypath");
        }

        @Override
        public String getOutputDescriptor(ECKey derivedKey) {
            return getDescriptor() + Utils.bytesToHex(derivedKey.getPubKeyXCoord()) + getCloseDescriptor();
        }

        @Override
        public String getOutputDescriptor(Script script) {
            throw new ProtocolException("Cannot create a taproot output descriptor without a keypath");
        }

        @Override
        public String getDescriptor() {
            return "tr(";
        }

        @Override
        public boolean isScriptType(Script script) {
            List<ScriptChunk> chunks = script.chunks;
            if (chunks.size() != 2)
                return false;
            if (!chunks.get(0).equalsOpCode(OP_1))
                return false;
            byte[] chunk1data = chunks.get(1).data;
            if (chunk1data == null)
                return false;
            if (chunk1data.length != 32)
                return false;
            return true;
        }

        @Override
        public byte[] getHashFromScript(Script script) {
            throw new ProtocolException("P2TR script does not contain a hash, use getPublicKeyFromScript(script) to retrieve public key");
        }

        @Override
        public ECKey getPublicKeyFromScript(Script script) {
            return ECKey.fromPublicOnly(script.chunks.get(1).data);
        }

        @Override
        public Script getScriptSig(Script scriptPubKey, ECKey pubKey, TransactionSignature signature) {
            if(!isScriptType(scriptPubKey)) {
                throw new ProtocolException("Provided scriptPubKey is not a " + getName() + " script");
            }

            if(!scriptPubKey.equals(getOutputScript(pubKey))) {
                throw new ProtocolException("Provided P2TR scriptPubKey does not match constructed pubkey script");
            }

            return new Script(new byte[0]);
        }

        @Override
        public TransactionInput addSpendingInput(Transaction transaction, TransactionOutput prevOutput, ECKey pubKey, TransactionSignature signature) {
            Script scriptSig = getScriptSig(prevOutput.getScript(), pubKey, signature);
            TransactionWitness witness = new TransactionWitness(transaction, signature);
            return transaction.addInput(prevOutput.getHash(), prevOutput.getIndex(), scriptSig, witness);
        }

        @Override
        public Script getMultisigScriptSig(Script scriptPubKey, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures) {
            throw new UnsupportedOperationException("Constructing Taproot inputs is not yet supported");
        }

        @Override
        public TransactionInput addMultisigSpendingInput(Transaction transaction, TransactionOutput prevOutput, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures) {
            throw new UnsupportedOperationException("Constructing Taproot inputs is not yet supported");
        }

        @Override
        public TransactionSignature.Type getSignatureType() {
            return TransactionSignature.Type.SCHNORR;
        };

        @Override
        public List<PolicyType> getAllowedPolicyTypes() {
            return Collections.singletonList(SINGLE);
        }
    };

    private final String name;
    private final String description;
    private final String defaultDerivationPath;

    ScriptType(String name, String description, String defaultDerivationPath) {
        this.name = name;
        this.description = description;
        this.defaultDerivationPath = defaultDerivationPath;
    }

    public String getName() {
        return name;
    }

    public String getDescription() {
        return description;
    }

    public String getDefaultDerivationPath() {
        return Network.get() != Network.MAINNET ? defaultDerivationPath.replace("/0'/0'", "/1'/0'") : defaultDerivationPath;
    }

    public List<ChildNumber> getDefaultDerivation() {
        return KeyDerivation.parsePath(getDefaultDerivationPath());
    }

    public List<ChildNumber> getDefaultDerivation(int account) {
        List<ChildNumber> copy = new ArrayList<>(KeyDerivation.parsePath(getDefaultDerivationPath()));
        if(copy.size() > 2) {
            ChildNumber accountChildNumber = new ChildNumber(account, true);
            copy.set(2, accountChildNumber);
        }

        return Collections.unmodifiableList(copy);
    }

    public int getAccount(String derivationPath) {
        if(KeyDerivation.isValid(derivationPath)) {
            List<ChildNumber> derivation = new ArrayList<>(KeyDerivation.parsePath(derivationPath));
            if(derivation.size() > 2) {
                int account = derivation.get(2).num();
                List<ChildNumber> defaultDerivation = getDefaultDerivation(account);
                if(defaultDerivation.equals(derivation)) {
                    return account;
                }
            }
        }

        return -1;
    }

    public abstract List<PolicyType> getAllowedPolicyTypes();

    public boolean isAllowed(PolicyType policyType) {
        return getAllowedPolicyTypes().contains(policyType);
    }

    public ECKey getOutputKey(ECKey derivedKey) {
        return derivedKey;
    }

    public abstract Address getAddress(byte[] bytes);

    public abstract Address getAddress(ECKey key);

    public abstract Address getAddress(Script script);

    public abstract Script getOutputScript(byte[] bytes);

    public abstract Script getOutputScript(ECKey key);

    public abstract Script getOutputScript(Script script);

    public Script getOutputScript(int threshold, Collection<ECKey> pubKeys) {
        throw new UnsupportedOperationException("Only defined for MULTISIG script type");
    }

    public abstract String getOutputDescriptor(ECKey key);

    public abstract String getOutputDescriptor(Script script);

    public abstract String getDescriptor();

    public String getCloseDescriptor() {
        StringBuilder sb = new StringBuilder();
        String descriptor = getDescriptor();

        for (int i = 0; i < descriptor.length(); i++) {
            char ch = descriptor.charAt(i);
            if (ch == '(') {
                sb.append(")");
            }
        }

        return sb.toString();
    }

    public abstract boolean isScriptType(Script script);

    public abstract byte[] getHashFromScript(Script script);

    public Address[] getAddresses(Script script) {
        return new Address[] { getAddress(getHashFromScript(script)) };
    }

    public ECKey getPublicKeyFromScript(Script script) {
        throw new ProtocolException("Script type " + this + " does not contain a public key");
    }

    public ECKey[] getPublicKeysFromScript(Script script) {
        throw new ProtocolException("Script type " + this + " does not contain public keys");
    }

    public int getThreshold(Script script) {
        throw new ProtocolException("Script type " + this + " is not a multisig script");
    }

    public abstract Script getScriptSig(Script scriptPubKey, ECKey pubKey, TransactionSignature signature);

    public abstract TransactionInput addSpendingInput(Transaction transaction, TransactionOutput prevOutput, ECKey pubKey, TransactionSignature signature);

    public abstract Script getMultisigScriptSig(Script scriptPubKey, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures);

    public abstract TransactionInput addMultisigSpendingInput(Transaction transaction, TransactionOutput prevOutput, int threshold, Map<ECKey, TransactionSignature> pubKeySignatures);

    public abstract TransactionSignature.Type getSignatureType();

    public static final ScriptType[] SINGLE_KEY_TYPES = {P2PK, P2TR};

    public static final ScriptType[] SINGLE_HASH_TYPES = {P2PKH, P2SH, P2SH_P2WPKH, P2SH_P2WSH, P2WPKH, P2WSH};

    public static final ScriptType[] ADDRESSABLE_TYPES = {P2PKH, P2SH, P2SH_P2WPKH, P2SH_P2WSH, P2WPKH, P2WSH, P2TR};

    public static final ScriptType[] NON_WITNESS_TYPES = {P2PK, P2PKH, P2SH};

    public static final ScriptType[] WITNESS_TYPES = {P2SH_P2WPKH, P2SH_P2WSH, P2WPKH, P2WSH, P2TR};

    public static ScriptType getType(Script script) {
        for(ScriptType type : values()) {
            if(type.isScriptType(script)) {
                return type;
            }
        }

        return null;
    }

    /**
     * Determines the dust threshold for the given output for this script type.
     *
     * @param output The output under consideration
     * @return the minimum viable value than the provided output must have in order to not be dust
     */
    public long getDustThreshold(TransactionOutput output, Double feeRate) {
        return getFee(output, feeRate, Transaction.DUST_RELAY_TX_FEE);
    }

    /**
     * Determines the minimum incremental fee necessary to pay for added the provided output to a transaction
     * This is done by calculating the sum of multiplying the size of the output at the current fee rate,
     * and the size of the input needed to spend it in future at the long term fee rate
     *
     * @param output The output to be added
     * @param feeRate The transaction's fee rate
     * @param longTermFeeRate The long term minimum fee rate
     * @return The fee that adding this output would add
     */
    public long getFee(TransactionOutput output, Double feeRate, Double longTermFeeRate) {
        //Start with length of output
        int outputVbytes = output.getLength();
        //Add length of spending input (with or without discount depending on script type)
        double inputVbytes = getInputVbytes();

        //Return fee rate in sats/vByte multiplied by the calculated output and input vByte lengths
        return (long)(feeRate * outputVbytes + longTermFeeRate * inputVbytes);
    }

    /**
     * Return a coarse estimation of the minimum number of vBytes required to spend an input of this script type.
     * Because we don't know the nature of the scriptSig/witnessScript required, pay to script inputs will likely be underestimated.
     * Use Wallet.getInputVbytes() for an accurate value to spend a wallet UTXO.
     *
     * @return The number of vBytes required for an input of this script type
     */
    public double getInputVbytes() {
        if(P2SH_P2WPKH.equals(this)) {
            return (32 + 4 + 1 + 13 + ((double)107 / WITNESS_SCALE_FACTOR) + 4);
        } else if(P2SH_P2WSH.equals(this)) {
            return (32 + 4 + 1 + 35 + ((double)107 / WITNESS_SCALE_FACTOR) + 4);
        } else if(P2TR.equals(this)) {
            //Assume a default keypath spend
            return (32 + 4 + 1 + ((double)66 / WITNESS_SCALE_FACTOR) + 4);
        } else if(Arrays.asList(WITNESS_TYPES).contains(this)) {
            //Return length of spending input with 75% discount to script size
            return (32 + 4 + 1 + ((double)107 / WITNESS_SCALE_FACTOR) + 4);
        } else if(Arrays.asList(NON_WITNESS_TYPES).contains(this)) {
            //Return length of spending input with no discount
            return (32 + 4 + 1 + 107 + 4);
        } else {
            throw new UnsupportedOperationException("Cannot determine dust threshold for script type " + this.getName());
        }
    }

    @Override
    public String toString() {
        return name;
    }
}
