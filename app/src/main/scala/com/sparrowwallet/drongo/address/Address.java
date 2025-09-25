package com.sparrowwallet.drongo.address;

import com.sparrowwallet.drongo.Network;
import com.sparrowwallet.drongo.protocol.Base58;
import com.sparrowwallet.drongo.protocol.Bech32;
import com.sparrowwallet.drongo.protocol.Script;
import com.sparrowwallet.drongo.protocol.ScriptType;

import java.util.Arrays;
import java.util.Locale;

public abstract class Address {
    protected final byte[] data;

    public Address(byte[] data) {
        this.data = data;
    }

    public byte[] getData() {
        return data;
    }

    public String getAddress() {
        return getAddress(Network.get());
    }

    public String getAddress(Network network) {
        return Base58.encodeChecked(getVersion(network), data);
    }

    public String toString() {
        return getAddress(Network.get());
    }

    public String toString(Network network) {
        return getAddress(network);
    }

    public int getVersion() {
        return getVersion(Network.get());
    }

    public abstract int getVersion(Network network);

    public abstract ScriptType getScriptType();

    public Script getOutputScript() {
        return getScriptType().getOutputScript(data);
    }

    public byte[] getOutputScriptData() {
        return data;
    }

    public abstract String getOutputScriptDataType();

    public boolean equals(Object obj) {
        if (!(obj instanceof Address)) {
            return false;
        }

        Address address = (Address) obj;

        return Arrays.equals(data, address.data) && getVersion(Network.get()) == address.getVersion(Network.get());
    }

    public int hashCode() {
        return Arrays.hashCode(data) + getVersion(Network.get());
    }

    public static Address fromString(String address) throws InvalidAddressException {
        try {
            return fromString(Network.get(), address);
        } catch(InvalidAddressException e) {
            for(Network network : Network.values()) {
                try {
                    fromString(network, address);
                    if(network != Network.get()) {
                        throw new InvalidAddressException("Provided " + network.getName() + " address invalid on configured " + Network.get().getName() + " network: " + address + ". Use a " + network.getName() + " configuration to use this address.");
                    }
                } catch(InvalidAddressException i) {
                    //ignore
                }
            }

            throw e;
        }
    }

    public static Address fromString(Network network, String address) throws InvalidAddressException {
        Exception nested = null;

        if(address != null) {
            if(network.hasP2PKHAddressPrefix(address) || network.hasP2SHAddressPrefix(address)) {
                try {
                    byte[] decodedBytes = Base58.decodeChecked(address);
                    if(decodedBytes.length == 21) {
                        int version = decodedBytes[0] & 0xFF;
                        byte[] hash = Arrays.copyOfRange(decodedBytes, 1, 21);
                        if(version == network.getP2PKHAddressHeader()) {
                            return new P2PKHAddress(hash);
                        }
                        if(version == network.getP2SHAddressHeader()) {
                            return new P2SHAddress(hash);
                        }
                    }
                } catch (Exception e) {
                    nested = e;
                }
            }

            if(address.toLowerCase(Locale.ROOT).startsWith(network.getBech32AddressHRP())) {
                try {
                    Bech32.Bech32Data data = Bech32.decode(address);
                    if(data.hrp.equals(network.getBech32AddressHRP())) {
                        int witnessVersion = data.data[0];
                        if(witnessVersion == 0) {
                            if(data.encoding != Bech32.Encoding.BECH32) {
                                throw new InvalidAddressException("Invalid address - witness version is 0 but encoding is " + data.encoding);
                            }

                            byte[] convertedProgram = Arrays.copyOfRange(data.data, 1, data.data.length);
                            byte[] witnessProgram = Bech32.convertBits(convertedProgram, 0, convertedProgram.length, 5, 8, false);
                            if(witnessProgram.length == 20) {
                                return new P2WPKHAddress(witnessProgram);
                            }
                            if(witnessProgram.length == 32) {
                                return new P2WSHAddress(witnessProgram);
                            }
                        } else if(witnessVersion == 1) {
                            if(data.encoding != Bech32.Encoding.BECH32M) {
                                throw new InvalidAddressException("Invalid address - witness version is 1 but encoding is " + data.encoding);
                            }

                            byte[] convertedProgram = Arrays.copyOfRange(data.data, 1, data.data.length);
                            byte[] witnessProgram = Bech32.convertBits(convertedProgram, 0, convertedProgram.length, 5, 8, false);
                            if(witnessProgram.length == 32) {
                                return new P2TRAddress(witnessProgram);
                            }
                        }
                    }
                } catch (Exception e) {
                    nested = e;
                }
            }
        }

        if(nested != null) {
            throw new InvalidAddressException("Could not parse invalid address " + address, nested);
        }

        throw new InvalidAddressException("Could not parse invalid address " + address);
    }
}
