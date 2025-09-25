package fr.acinq.bitcoin

import fr.acinq.bitcoin.Crypto.PublicKey
import scodec.bits.ByteVector

// @formatter:off
abstract class ScriptElt
case object OP_0 extends ScriptElt
case object OP_PUSHDATA1 extends ScriptElt
case object OP_PUSHDATA2 extends ScriptElt
case object OP_PUSHDATA4 extends ScriptElt
case object OP_1NEGATE extends ScriptElt
case object OP_RESERVED extends ScriptElt
case object OP_1 extends ScriptElt
case object OP_2 extends ScriptElt
case object OP_3 extends ScriptElt
case object OP_4 extends ScriptElt
case object OP_5 extends ScriptElt
case object OP_6 extends ScriptElt
case object OP_7 extends ScriptElt
case object OP_8 extends ScriptElt
case object OP_9 extends ScriptElt
case object OP_10 extends ScriptElt
case object OP_11 extends ScriptElt
case object OP_12 extends ScriptElt
case object OP_13 extends ScriptElt
case object OP_14 extends ScriptElt
case object OP_15 extends ScriptElt
case object OP_16 extends ScriptElt
case object OP_NOP extends ScriptElt
case object OP_VER extends ScriptElt
case object OP_IF extends ScriptElt
case object OP_NOTIF extends ScriptElt
case object OP_VERIF extends ScriptElt
case object OP_VERNOTIF extends ScriptElt
case object OP_ELSE extends ScriptElt
case object OP_ENDIF extends ScriptElt
case object OP_VERIFY extends ScriptElt
case object OP_RETURN extends ScriptElt
case object OP_TOALTSTACK extends ScriptElt
case object OP_FROMALTSTACK extends ScriptElt
case object OP_2DROP extends ScriptElt
case object OP_2DUP extends ScriptElt
case object OP_3DUP extends ScriptElt
case object OP_2OVER extends ScriptElt
case object OP_2ROT extends ScriptElt
case object OP_2SWAP extends ScriptElt
case object OP_IFDUP extends ScriptElt
case object OP_DEPTH extends ScriptElt
case object OP_DROP extends ScriptElt
case object OP_DUP extends ScriptElt
case object OP_NIP extends ScriptElt
case object OP_OVER extends ScriptElt
case object OP_PICK extends ScriptElt
case object OP_ROLL extends ScriptElt
case object OP_ROT extends ScriptElt
case object OP_SWAP extends ScriptElt
case object OP_TUCK extends ScriptElt
case object OP_CAT extends ScriptElt
case object OP_SUBSTR extends ScriptElt
case object OP_LEFT extends ScriptElt
case object OP_RIGHT extends ScriptElt
case object OP_SIZE extends ScriptElt
case object OP_INVERT extends ScriptElt
case object OP_AND extends ScriptElt
case object OP_OR extends ScriptElt
case object OP_XOR extends ScriptElt
case object OP_EQUAL extends ScriptElt
case object OP_EQUALVERIFY extends ScriptElt
case object OP_RESERVED1 extends ScriptElt
case object OP_RESERVED2 extends ScriptElt
case object OP_1ADD extends ScriptElt
case object OP_1SUB extends ScriptElt
case object OP_2MUL extends ScriptElt
case object OP_2DIV extends ScriptElt
case object OP_NEGATE extends ScriptElt
case object OP_ABS extends ScriptElt
case object OP_NOT extends ScriptElt
case object OP_0NOTEQUAL extends ScriptElt
case object OP_ADD extends ScriptElt
case object OP_SUB extends ScriptElt
case object OP_MUL extends ScriptElt
case object OP_DIV extends ScriptElt
case object OP_MOD extends ScriptElt
case object OP_LSHIFT extends ScriptElt
case object OP_RSHIFT extends ScriptElt
case object OP_BOOLAND extends ScriptElt
case object OP_BOOLOR extends ScriptElt
case object OP_NUMEQUAL extends ScriptElt
case object OP_NUMEQUALVERIFY extends ScriptElt
case object OP_NUMNOTEQUAL extends ScriptElt
case object OP_LESSTHAN extends ScriptElt
case object OP_GREATERTHAN extends ScriptElt
case object OP_LESSTHANOREQUAL extends ScriptElt
case object OP_GREATERTHANOREQUAL extends ScriptElt
case object OP_MIN extends ScriptElt
case object OP_MAX extends ScriptElt
case object OP_WITHIN extends ScriptElt
case object OP_RIPEMD160 extends ScriptElt
case object OP_SHA1 extends ScriptElt
case object OP_SHA256 extends ScriptElt
case object OP_HASH160 extends ScriptElt
case object OP_HASH256 extends ScriptElt
case object OP_CODESEPARATOR extends ScriptElt
case object OP_CHECKSIG extends ScriptElt
case object OP_CHECKSIGVERIFY extends ScriptElt
case object OP_CHECKMULTISIG extends ScriptElt
case object OP_CHECKMULTISIGVERIFY extends ScriptElt
case object OP_NOP1 extends ScriptElt
case object OP_CHECKLOCKTIMEVERIFY extends ScriptElt
case object OP_CHECKSEQUENCEVERIFY extends ScriptElt
case object OP_NOP4 extends ScriptElt
case object OP_NOP5 extends ScriptElt
case object OP_NOP6 extends ScriptElt
case object OP_NOP7 extends ScriptElt
case object OP_NOP8 extends ScriptElt
case object OP_NOP9 extends ScriptElt
case object OP_NOP10 extends ScriptElt
case object OP_SMALLINTEGER extends ScriptElt
case object OP_INVALIDOPCODE extends ScriptElt
object OP_PUSHDATA {
  def apply(data: ByteVector) = if (data.length < 0x4c) new OP_PUSHDATA(data, data.size.toInt)
  else if (data.length < 0xff) new OP_PUSHDATA(data, 0x4c)
  else if (data.length < 0xffff) new OP_PUSHDATA(data, 0x4d)
  else if (data.length < 0xffffffff) new OP_PUSHDATA(data, 0x4e)
  else throw new IllegalArgumentException(s"data is ${data.length}, too big for OP_PUSHDATA")

  def apply(pub: PublicKey): OP_PUSHDATA = OP_PUSHDATA(pub.value)

  def isMinimal(data: ByteVector, code: Int): Boolean = if (data.length == 0) code == ScriptElt.elt2code(OP_0)
  else if (data.length == 1 && data(0) >= 1 && data(0) <= 16) code == ScriptElt.elt2code(OP_1) + (data(0) - 1)
  else if (data.length == 1 && data(0) == 0x81.toByte) code == ScriptElt.elt2code(OP_1NEGATE)
  else if (data.length <= 75) code == data.length
  else if (data.length <= 255) code == ScriptElt.elt2code(OP_PUSHDATA1)
  else if (data.length <= 65535) code == ScriptElt.elt2code(OP_PUSHDATA2)
  else true
}
case class OP_PUSHDATA(data: ByteVector, code: Int) extends ScriptElt {
  override def toString = data.toString
}
case class OP_INVALID(code: Int) extends ScriptElt
// @formatter:off

object ScriptElt {
  // code -> ScriptElt
  val code2elt: Map[Int, ScriptElt] = Map(
    0x00 -> OP_0,
    0x4c -> OP_PUSHDATA1,
    0x4d -> OP_PUSHDATA2,
    0x4e -> OP_PUSHDATA4,
    0x4f -> OP_1NEGATE,
    0x50 -> OP_RESERVED,
    0x51 -> OP_1,
    0x52 -> OP_2,
    0x53 -> OP_3,
    0x54 -> OP_4,
    0x55 -> OP_5,
    0x56 -> OP_6,
    0x57 -> OP_7,
    0x58 -> OP_8,
    0x59 -> OP_9,
    0x5a -> OP_10,
    0x5b -> OP_11,
    0x5c -> OP_12,
    0x5d -> OP_13,
    0x5e -> OP_14,
    0x5f -> OP_15,
    0x60 -> OP_16,
    0x61 -> OP_NOP,
    0x62 -> OP_VER,
    0x63 -> OP_IF,
    0x64 -> OP_NOTIF,
    0x65 -> OP_VERIF,
    0x66 -> OP_VERNOTIF,
    0x67 -> OP_ELSE,
    0x68 -> OP_ENDIF,
    0x69 -> OP_VERIFY,
    0x6a -> OP_RETURN,
    0x6b -> OP_TOALTSTACK,
    0x6c -> OP_FROMALTSTACK,
    0x6d -> OP_2DROP,
    0x6e -> OP_2DUP,
    0x6f -> OP_3DUP,
    0x70 -> OP_2OVER,
    0x71 -> OP_2ROT,
    0x72 -> OP_2SWAP,
    0x73 -> OP_IFDUP,
    0x74 -> OP_DEPTH,
    0x75 -> OP_DROP,
    0x76 -> OP_DUP,
    0x77 -> OP_NIP,
    0x78 -> OP_OVER,
    0x79 -> OP_PICK,
    0x7a -> OP_ROLL,
    0x7b -> OP_ROT,
    0x7c -> OP_SWAP,
    0x7d -> OP_TUCK,
    0x7e -> OP_CAT,
    0x7f -> OP_SUBSTR,
    0x80 -> OP_LEFT,
    0x81 -> OP_RIGHT,
    0x82 -> OP_SIZE,
    0x83 -> OP_INVERT,
    0x84 -> OP_AND,
    0x85 -> OP_OR,
    0x86 -> OP_XOR,
    0x87 -> OP_EQUAL,
    0x88 -> OP_EQUALVERIFY,
    0x89 -> OP_RESERVED1,
    0x8a -> OP_RESERVED2,
    0x8b -> OP_1ADD,
    0x8c -> OP_1SUB,
    0x8d -> OP_2MUL,
    0x8e -> OP_2DIV,
    0x8f -> OP_NEGATE,
    0x90 -> OP_ABS,
    0x91 -> OP_NOT,
    0x92 -> OP_0NOTEQUAL,
    0x93 -> OP_ADD,
    0x94 -> OP_SUB,
    0x95 -> OP_MUL,
    0x96 -> OP_DIV,
    0x97 -> OP_MOD,
    0x98 -> OP_LSHIFT,
    0x99 -> OP_RSHIFT,
    0x9a -> OP_BOOLAND,
    0x9b -> OP_BOOLOR,
    0x9c -> OP_NUMEQUAL,
    0x9d -> OP_NUMEQUALVERIFY,
    0x9e -> OP_NUMNOTEQUAL,
    0x9f -> OP_LESSTHAN,
    0xa0 -> OP_GREATERTHAN,
    0xa1 -> OP_LESSTHANOREQUAL,
    0xa2 -> OP_GREATERTHANOREQUAL,
    0xa3 -> OP_MIN,
    0xa4 -> OP_MAX,
    0xa5 -> OP_WITHIN,
    0xa6 -> OP_RIPEMD160,
    0xa7 -> OP_SHA1,
    0xa8 -> OP_SHA256,
    0xa9 -> OP_HASH160,
    0xaa -> OP_HASH256,
    0xab -> OP_CODESEPARATOR,
    0xac -> OP_CHECKSIG,
    0xad -> OP_CHECKSIGVERIFY,
    0xae -> OP_CHECKMULTISIG,
    0xaf -> OP_CHECKMULTISIGVERIFY,
    0xb0 -> OP_NOP1,
    0xb1 -> OP_CHECKLOCKTIMEVERIFY,
    0xb2 -> OP_CHECKSEQUENCEVERIFY,
    0xb3 -> OP_NOP4,
    0xb4 -> OP_NOP5,
    0xb5 -> OP_NOP6,
    0xb6 -> OP_NOP7,
    0xb7 -> OP_NOP8,
    0xb8 -> OP_NOP9,
    0xb9 -> OP_NOP10,
    0xfa -> OP_SMALLINTEGER,
    0xff -> OP_INVALIDOPCODE)

  // ScriptElt -> code
  val elt2code: Map[ScriptElt, Int] = code2elt.map(_.swap)

  // name -> code
  val name2code = code2elt.mapValues(_.asInstanceOf[Product].productPrefix.stripPrefix("OP_")).map(_.swap) + ("NOP2" -> 0xb1) + ("NOP3" -> 0xb2)

  def isPush(op: ScriptElt, size: Int): Boolean = {
    op match {
      case OP_PUSHDATA(data, _) => data.length == size
      case _ => false
    }
  }

}
