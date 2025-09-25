package com.sparrowwallet.drongo.psbt;

import com.sparrowwallet.drongo.crypto.ECKey;
import com.sparrowwallet.drongo.protocol.Sha256Hash;
import com.sparrowwallet.drongo.protocol.SigHash;
import com.sparrowwallet.drongo.protocol.TransactionSignature;

public interface PSBTInputSigner {
    TransactionSignature sign(Sha256Hash hash, SigHash sigHash, TransactionSignature.Type signatureType);
    ECKey getPubKey();
}
