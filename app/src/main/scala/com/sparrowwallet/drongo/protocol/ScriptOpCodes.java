/*
 * Copyright 2013 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.sparrowwallet.drongo.protocol;

import java.util.HashMap;
import java.util.Map;

/**
 * Various constants that define the assembly-like scripting language that forms part of the Bitcoin protocol.
 * See {@link Script} for details. Also provides a method to convert them to a string.
 */
public class ScriptOpCodes {
    // push value
    public static final int OP_0 = 0x00; // push empty vector
    public static final int OP_FALSE = OP_0;
    public static final int OP_PUSHDATA1 = 0x4c;
    public static final int OP_PUSHDATA2 = 0x4d;
    public static final int OP_PUSHDATA4 = 0x4e;
    public static final int OP_1NEGATE = 0x4f;
    public static final int OP_RESERVED = 0x50;
    public static final int OP_1 = 0x51;
    public static final int OP_TRUE = OP_1;
    public static final int OP_2 = 0x52;
    public static final int OP_3 = 0x53;
    public static final int OP_4 = 0x54;
    public static final int OP_5 = 0x55;
    public static final int OP_6 = 0x56;
    public static final int OP_7 = 0x57;
    public static final int OP_8 = 0x58;
    public static final int OP_9 = 0x59;
    public static final int OP_10 = 0x5a;
    public static final int OP_11 = 0x5b;
    public static final int OP_12 = 0x5c;
    public static final int OP_13 = 0x5d;
    public static final int OP_14 = 0x5e;
    public static final int OP_15 = 0x5f;
    public static final int OP_16 = 0x60;

    // control
    public static final int OP_NOP = 0x61;
    public static final int OP_VER = 0x62;
    public static final int OP_IF = 0x63;
    public static final int OP_NOTIF = 0x64;
    public static final int OP_VERIF = 0x65;
    public static final int OP_VERNOTIF = 0x66;
    public static final int OP_ELSE = 0x67;
    public static final int OP_ENDIF = 0x68;
    public static final int OP_VERIFY = 0x69;
    public static final int OP_RETURN = 0x6a;

    // stack ops
    public static final int OP_TOALTSTACK = 0x6b;
    public static final int OP_FROMALTSTACK = 0x6c;
    public static final int OP_2DROP = 0x6d;
    public static final int OP_2DUP = 0x6e;
    public static final int OP_3DUP = 0x6f;
    public static final int OP_2OVER = 0x70;
    public static final int OP_2ROT = 0x71;
    public static final int OP_2SWAP = 0x72;
    public static final int OP_IFDUP = 0x73;
    public static final int OP_DEPTH = 0x74;
    public static final int OP_DROP = 0x75;
    public static final int OP_DUP = 0x76;
    public static final int OP_NIP = 0x77;
    public static final int OP_OVER = 0x78;
    public static final int OP_PICK = 0x79;
    public static final int OP_ROLL = 0x7a;
    public static final int OP_ROT = 0x7b;
    public static final int OP_SWAP = 0x7c;
    public static final int OP_TUCK = 0x7d;

    // splice ops
    public static final int OP_CAT = 0x7e;
    public static final int OP_SUBSTR = 0x7f;
    public static final int OP_LEFT = 0x80;
    public static final int OP_RIGHT = 0x81;
    public static final int OP_SIZE = 0x82;

    // bit logic
    public static final int OP_INVERT = 0x83;
    public static final int OP_AND = 0x84;
    public static final int OP_OR = 0x85;
    public static final int OP_XOR = 0x86;
    public static final int OP_EQUAL = 0x87;
    public static final int OP_EQUALVERIFY = 0x88;
    public static final int OP_RESERVED1 = 0x89;
    public static final int OP_RESERVED2 = 0x8a;

    // numeric
    public static final int OP_1ADD = 0x8b;
    public static final int OP_1SUB = 0x8c;
    public static final int OP_2MUL = 0x8d;
    public static final int OP_2DIV = 0x8e;
    public static final int OP_NEGATE = 0x8f;
    public static final int OP_ABS = 0x90;
    public static final int OP_NOT = 0x91;
    public static final int OP_0NOTEQUAL = 0x92;
    public static final int OP_ADD = 0x93;
    public static final int OP_SUB = 0x94;
    public static final int OP_MUL = 0x95;
    public static final int OP_DIV = 0x96;
    public static final int OP_MOD = 0x97;
    public static final int OP_LSHIFT = 0x98;
    public static final int OP_RSHIFT = 0x99;
    public static final int OP_BOOLAND = 0x9a;
    public static final int OP_BOOLOR = 0x9b;
    public static final int OP_NUMEQUAL = 0x9c;
    public static final int OP_NUMEQUALVERIFY = 0x9d;
    public static final int OP_NUMNOTEQUAL = 0x9e;
    public static final int OP_LESSTHAN = 0x9f;
    public static final int OP_GREATERTHAN = 0xa0;
    public static final int OP_LESSTHANOREQUAL = 0xa1;
    public static final int OP_GREATERTHANOREQUAL = 0xa2;
    public static final int OP_MIN = 0xa3;
    public static final int OP_MAX = 0xa4;
    public static final int OP_WITHIN = 0xa5;

    // crypto
    public static final int OP_RIPEMD160 = 0xa6;
    public static final int OP_SHA1 = 0xa7;
    public static final int OP_SHA256 = 0xa8;
    public static final int OP_HASH160 = 0xa9;
    public static final int OP_HASH256 = 0xaa;
    public static final int OP_CODESEPARATOR = 0xab;
    public static final int OP_CHECKSIG = 0xac;
    public static final int OP_CHECKSIGVERIFY = 0xad;
    public static final int OP_CHECKMULTISIG = 0xae;
    public static final int OP_CHECKMULTISIGVERIFY = 0xaf;

    // block state
    /** Check lock time of the block. Introduced in BIP 65, replacing OP_NOP2 */
    public static final int OP_CHECKLOCKTIMEVERIFY = 0xb1;
    public static final int OP_CHECKSEQUENCEVERIFY = 0xb2;

    // expansion
    public static final int OP_NOP1 = 0xb0;
    /** Deprecated by BIP 65 */
    @Deprecated
    public static final int OP_NOP2 = OP_CHECKLOCKTIMEVERIFY;
    /** Deprecated by BIP 112 */
    @Deprecated
    public static final int OP_NOP3 = OP_CHECKSEQUENCEVERIFY;
    public static final int OP_NOP4 = 0xb3;
    public static final int OP_NOP5 = 0xb4;
    public static final int OP_NOP6 = 0xb5;
    public static final int OP_NOP7 = 0xb6;
    public static final int OP_NOP8 = 0xb7;
    public static final int OP_NOP9 = 0xb8;
    public static final int OP_NOP10 = 0xb9;
    public static final int OP_INVALIDOPCODE = 0xff;

    private static final Map<Integer, String> opCodeNameMap;
    private static final Map<String, Integer> nameOpCodeMap;

    static {
        opCodeNameMap = new HashMap<Integer, String>();
        opCodeNameMap.put(OP_0, "0");
        opCodeNameMap.put(OP_PUSHDATA1, "PUSHDATA1");
        opCodeNameMap.put(OP_PUSHDATA2, "PUSHDATA2");
        opCodeNameMap.put(OP_PUSHDATA4, "PUSHDATA4");
        opCodeNameMap.put(OP_1NEGATE, "1NEGATE");
        opCodeNameMap.put(OP_RESERVED, "RESERVED");
        opCodeNameMap.put(OP_1, "1");
        opCodeNameMap.put(OP_2, "2");
        opCodeNameMap.put(OP_3, "3");
        opCodeNameMap.put(OP_4, "4");
        opCodeNameMap.put(OP_5, "5");
        opCodeNameMap.put(OP_6, "6");
        opCodeNameMap.put(OP_7, "7");
        opCodeNameMap.put(OP_8, "8");
        opCodeNameMap.put(OP_9, "9");
        opCodeNameMap.put(OP_10, "10");
        opCodeNameMap.put(OP_11, "11");
        opCodeNameMap.put(OP_12, "12");
        opCodeNameMap.put(OP_13, "13");
        opCodeNameMap.put(OP_14, "14");
        opCodeNameMap.put(OP_15, "15");
        opCodeNameMap.put(OP_16, "16");
        opCodeNameMap.put(OP_NOP, "NOP");
        opCodeNameMap.put(OP_VER, "VER");
        opCodeNameMap.put(OP_IF, "IF");
        opCodeNameMap.put(OP_NOTIF, "NOTIF");
        opCodeNameMap.put(OP_VERIF, "VERIF");
        opCodeNameMap.put(OP_VERNOTIF, "VERNOTIF");
        opCodeNameMap.put(OP_ELSE, "ELSE");
        opCodeNameMap.put(OP_ENDIF, "ENDIF");
        opCodeNameMap.put(OP_VERIFY, "VERIFY");
        opCodeNameMap.put(OP_RETURN, "RETURN");
        opCodeNameMap.put(OP_TOALTSTACK, "TOALTSTACK");
        opCodeNameMap.put(OP_FROMALTSTACK, "FROMALTSTACK");
        opCodeNameMap.put(OP_2DROP, "2DROP");
        opCodeNameMap.put(OP_2DUP, "2DUP");
        opCodeNameMap.put(OP_3DUP, "3DUP");
        opCodeNameMap.put(OP_2OVER, "2OVER");
        opCodeNameMap.put(OP_2ROT, "2ROT");
        opCodeNameMap.put(OP_2SWAP, "2SWAP");
        opCodeNameMap.put(OP_IFDUP, "IFDUP");
        opCodeNameMap.put(OP_DEPTH, "DEPTH");
        opCodeNameMap.put(OP_DROP, "DROP");
        opCodeNameMap.put(OP_DUP, "DUP");
        opCodeNameMap.put(OP_NIP, "NIP");
        opCodeNameMap.put(OP_OVER, "OVER");
        opCodeNameMap.put(OP_PICK, "PICK");
        opCodeNameMap.put(OP_ROLL, "ROLL");
        opCodeNameMap.put(OP_ROT, "ROT");
        opCodeNameMap.put(OP_SWAP, "SWAP");
        opCodeNameMap.put(OP_TUCK, "TUCK");
        opCodeNameMap.put(OP_CAT, "CAT");
        opCodeNameMap.put(OP_SUBSTR, "SUBSTR");
        opCodeNameMap.put(OP_LEFT, "LEFT");
        opCodeNameMap.put(OP_RIGHT, "RIGHT");
        opCodeNameMap.put(OP_SIZE, "SIZE");
        opCodeNameMap.put(OP_INVERT, "INVERT");
        opCodeNameMap.put(OP_AND, "AND");
        opCodeNameMap.put(OP_OR, "OR");
        opCodeNameMap.put(OP_XOR, "XOR");
        opCodeNameMap.put(OP_EQUAL, "EQUAL");
        opCodeNameMap.put(OP_EQUALVERIFY, "EQUALVERIFY");
        opCodeNameMap.put(OP_RESERVED1, "RESERVED1");
        opCodeNameMap.put(OP_RESERVED2, "RESERVED2");
        opCodeNameMap.put(OP_1ADD, "1ADD");
        opCodeNameMap.put(OP_1SUB, "1SUB");
        opCodeNameMap.put(OP_2MUL, "2MUL");
        opCodeNameMap.put(OP_2DIV, "2DIV");
        opCodeNameMap.put(OP_NEGATE, "NEGATE");
        opCodeNameMap.put(OP_ABS, "ABS");
        opCodeNameMap.put(OP_NOT, "NOT");
        opCodeNameMap.put(OP_0NOTEQUAL, "0NOTEQUAL");
        opCodeNameMap.put(OP_ADD, "ADD");
        opCodeNameMap.put(OP_SUB, "SUB");
        opCodeNameMap.put(OP_MUL, "MUL");
        opCodeNameMap.put(OP_DIV, "DIV");
        opCodeNameMap.put(OP_MOD, "MOD");
        opCodeNameMap.put(OP_LSHIFT, "LSHIFT");
        opCodeNameMap.put(OP_RSHIFT, "RSHIFT");
        opCodeNameMap.put(OP_BOOLAND, "BOOLAND");
        opCodeNameMap.put(OP_BOOLOR, "BOOLOR");
        opCodeNameMap.put(OP_NUMEQUAL, "NUMEQUAL");
        opCodeNameMap.put(OP_NUMEQUALVERIFY, "NUMEQUALVERIFY");
        opCodeNameMap.put(OP_NUMNOTEQUAL, "NUMNOTEQUAL");
        opCodeNameMap.put(OP_LESSTHAN, "LESSTHAN");
        opCodeNameMap.put(OP_GREATERTHAN, "GREATERTHAN");
        opCodeNameMap.put(OP_LESSTHANOREQUAL, "LESSTHANOREQUAL");
        opCodeNameMap.put(OP_GREATERTHANOREQUAL, "GREATERTHANOREQUAL");
        opCodeNameMap.put(OP_MIN, "MIN");
        opCodeNameMap.put(OP_MAX, "MAX");
        opCodeNameMap.put(OP_WITHIN, "WITHIN");
        opCodeNameMap.put(OP_RIPEMD160, "RIPEMD160");
        opCodeNameMap.put(OP_SHA1, "SHA1");
        opCodeNameMap.put(OP_SHA256, "SHA256");
        opCodeNameMap.put(OP_HASH160, "HASH160");
        opCodeNameMap.put(OP_HASH256, "HASH256");
        opCodeNameMap.put(OP_CODESEPARATOR, "CODESEPARATOR");
        opCodeNameMap.put(OP_CHECKSIG, "CHECKSIG");
        opCodeNameMap.put(OP_CHECKSIGVERIFY, "CHECKSIGVERIFY");
        opCodeNameMap.put(OP_CHECKMULTISIG, "CHECKMULTISIG");
        opCodeNameMap.put(OP_CHECKMULTISIGVERIFY, "CHECKMULTISIGVERIFY");
        opCodeNameMap.put(OP_NOP1, "NOP1");
        opCodeNameMap.put(OP_CHECKLOCKTIMEVERIFY, "CHECKLOCKTIMEVERIFY");
        opCodeNameMap.put(OP_CHECKSEQUENCEVERIFY, "CHECKSEQUENCEVERIFY");
        opCodeNameMap.put(OP_NOP4, "NOP4");
        opCodeNameMap.put(OP_NOP5, "NOP5");
        opCodeNameMap.put(OP_NOP6, "NOP6");
        opCodeNameMap.put(OP_NOP7, "NOP7");
        opCodeNameMap.put(OP_NOP8, "NOP8");
        opCodeNameMap.put(OP_NOP9, "NOP9");
        opCodeNameMap.put(OP_NOP10, "NOP10");

        nameOpCodeMap = new HashMap<String, Integer>();
        for(Map.Entry<Integer, String> e : opCodeNameMap.entrySet()) {
            nameOpCodeMap.put(e.getValue(), e.getKey());
        }
        nameOpCodeMap.put("OP_FALSE", OP_FALSE);
        nameOpCodeMap.put("OP_TRUE", OP_TRUE);
        nameOpCodeMap.put("NOP2", OP_NOP2);
        nameOpCodeMap.put("NOP3", OP_NOP3);
    }

    /**
     * Converts the given OpCode into a string (eg "0", "PUSHDATA", or "RETURN_10")
     */
    public static String getOpCodeName(int opcode) {
        if (opCodeNameMap.containsKey((Integer)opcode)) {
            return opCodeNameMap.get(opcode);
        }

        return "RETURN_" + opcode;
    }

    /**
     * Converts the given pushdata OpCode into a string (eg "PUSHDATA_2", or "PUSHDATA_23")
     */
    public static String getPushDataName(int opcode) {
        if (opCodeNameMap.containsKey(opcode)) {
            return opCodeNameMap.get(opcode);
        }

        return "PUSHDATA_" + opcode;
    }

    /**
     * Converts the given OpCodeName into an int
     */
    public static int getOpCode(String opCodeName) {
        if (opCodeNameMap.containsKey(opCodeName)) {
            return nameOpCodeMap.get(opCodeName);
        }

        return OP_INVALIDOPCODE;
    }
}