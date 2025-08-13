A format to make BIP322 signing/verification data portable so it can be easily copy-pasted or displayed as QR code. The idea is to pack all the needed parts into a single string.

We have two data formats. 

The first one is `BIP322Sign` which is a request to sign a given message with a given address, it looks like this:  
`bip322sign<message in base64>|<Bitcoin address>`.  
Upon receiving this request signer wallet is supposed to check if a message can be signed with requested address (that is, if wallet has a corresponding private key) and then start a signing procedure.

The second one is `BIP322Verify` which includes all the required parts to verify a message hash plus optionally a message itself, it looks like this when message itself is included:  
`bip322verify<Bitcoin address>|<BIP322 message hash>|<signature in base64>|<message in base64>`  
and like this if message itself is not included:  
`bip322verify<Bitcoin address>|<BIP322 message hash>|<signature in base64>|-`.  
Upon receiving this response a verifier wallet should do the verification and present results appropriately.