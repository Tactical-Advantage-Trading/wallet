package fr.acinq.bitcoin

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}

import fr.acinq.bitcoin.Crypto._
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
 * script execution flags
 */
object ScriptFlags {
  val SCRIPT_VERIFY_NONE = 0

  // Evaluate P2SH subscripts (softfork safe, BIP16).
  val SCRIPT_VERIFY_P2SH = 1 << 0

  // Passing a non-strict-DER signature or one with undefined hashtype to a checksig operation causes script failure.
  // Evaluating a pubkey that is not (0x04 + 64 bytes) or (0x02 or 0x03 + 32 bytes) by checksig causes script failure.
  // (softfork safe, but not used or intended as a consensus rule).
  val SCRIPT_VERIFY_STRICTENC = 1 << 1

  // Passing a non-strict-DER signature to a checksig operation causes script failure (softfork safe, BIP62 rule 1)
  val SCRIPT_VERIFY_DERSIG = 1 << 2

  // Passing a non-strict-DER signature or one with S > order/2 to a checksig operation causes script failure
  // (softfork safe, BIP62 rule 5).
  val SCRIPT_VERIFY_LOW_S = 1 << 3

  // verify dummy stack item consumed by CHECKMULTISIG is of zero-length (softfork safe, BIP62 rule 7).
  val SCRIPT_VERIFY_NULLDUMMY = 1 << 4

  // Using a non-push operator in the scriptSig causes script failure (softfork safe, BIP62 rule 2).
  val SCRIPT_VERIFY_SIGPUSHONLY = 1 << 5

  // Require minimal encodings for all push operations (OP_0... OP_16, OP_1NEGATE where possible, direct
  // pushes up to 75 bytes, OP_PUSHDATA up to 255 bytes, OP_PUSHDATA2 for anything larger). Evaluating
  // any other push causes the script to fail (BIP62 rule 3).
  // In addition, whenever a stack element is interpreted as a number, it must be of minimal length (BIP62 rule 4).
  // (softfork safe)
  val SCRIPT_VERIFY_MINIMALDATA = 1 << 6

  // Discourage use of NOPs reserved for upgrades (NOP1-10)
  //
  // Provided so that nodes can avoid accepting or mining transactions
  // containing executed NOP's whose meaning may change after a soft-fork,
  // thus rendering the script invalid; with this flag set executing
  // discouraged NOPs fails the script. This verification flag will never be
  // a mandatory flag applied to scripts in a block. NOPs that are not
  // executed, e.g.  within an unexecuted IF ENDIF block, are *not* rejected.
  val SCRIPT_VERIFY_DISCOURAGE_UPGRADABLE_NOPS = 1 << 7

  // Require that only a single stack element remains after evaluation. This changes the success criterion from
  // "At least one stack element must remain, and when interpreted as a boolean, it must be true" to
  // "Exactly one stack element must remain, and when interpreted as a boolean, it must be true".
  // (softfork safe, BIP62 rule 6)
  // Note: CLEANSTACK should never be used without P2SH.
  val SCRIPT_VERIFY_CLEANSTACK = 1 << 8

  // Verify CHECKLOCKTIMEVERIFY
  //
  // See BIP65 for details.
  val SCRIPT_VERIFY_CHECKLOCKTIMEVERIFY = 1 << 9


  // See BIP112 for details
  val SCRIPT_VERIFY_CHECKSEQUENCEVERIFY = 1 << 10

  // support CHECKSEQUENCEVERIFY opcode
  //
  // Support segregated witness
  //
  val SCRIPT_VERIFY_WITNESS = 1 << 11

  // Making v2-v16 witness program non-standard
  //
  val SCRIPT_VERIFY_DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM = 1 << 12


  // Segwit script only: Require the argument of OP_IF/NOTIF to be exactly 0x01 or empty vector
  //
  val SCRIPT_VERIFY_MINIMALIF = 1 << 13

  // Signature(s) must be empty vector if an CHECK(MULTI)SIG operation failed
  //
  val SCRIPT_VERIFY_NULLFAIL = 1 << 14

  // Public keys in segregated witness scripts must be compressed
  //
  val SCRIPT_VERIFY_WITNESS_PUBKEYTYPE = 1 << 15

  // Making OP_CODESEPARATOR and FindAndDelete fail any non-segwit scripts
  //
  val SCRIPT_VERIFY_CONST_SCRIPTCODE = 1 << 16

  /**
   * Mandatory script verification flags that all new blocks must comply with for
   * them to be valid. (but old blocks may not comply with) Currently just P2SH,
   * but in the future other flags may be added, such as a soft-fork to enforce
   * strict DER encoding.
   *
   * Failing one of these tests may trigger a DoS ban - see CheckInputs() for
   * details.
   */
  val MANDATORY_SCRIPT_VERIFY_FLAGS = SCRIPT_VERIFY_P2SH

  /**
   * Standard script verification flags that standard transactions will comply
   * with. However scripts violating these flags may still be present in valid
   * blocks and we must accept those blocks.
   */
  val STANDARD_SCRIPT_VERIFY_FLAGS = MANDATORY_SCRIPT_VERIFY_FLAGS |
    SCRIPT_VERIFY_DERSIG |
    SCRIPT_VERIFY_STRICTENC |
    SCRIPT_VERIFY_MINIMALDATA |
    SCRIPT_VERIFY_NULLDUMMY |
    SCRIPT_VERIFY_DISCOURAGE_UPGRADABLE_NOPS |
    SCRIPT_VERIFY_CLEANSTACK |
    SCRIPT_VERIFY_MINIMALIF |
    SCRIPT_VERIFY_NULLFAIL |
    SCRIPT_VERIFY_CHECKLOCKTIMEVERIFY |
    SCRIPT_VERIFY_CHECKSEQUENCEVERIFY |
    SCRIPT_VERIFY_LOW_S |
    SCRIPT_VERIFY_WITNESS |
    SCRIPT_VERIFY_DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM |
    SCRIPT_VERIFY_WITNESS_PUBKEYTYPE |
    SCRIPT_VERIFY_CONST_SCRIPTCODE

  /** For convenience, standard but not mandatory verify flags. */
  val STANDARD_NOT_MANDATORY_VERIFY_FLAGS = STANDARD_SCRIPT_VERIFY_FLAGS & ~MANDATORY_SCRIPT_VERIFY_FLAGS
}

object Script {

  import Protocol._
  import ScriptFlags._
  import fr.acinq.bitcoin.ScriptElt._

  type Stack = List[ByteVector]

  private val True = ByteVector.fromByte(1)

  private val False = ByteVector.empty

  /**
   * parse a script from a input stream of binary data
   *
   * @param input input stream
   * @param stack initial command stack
   * @return an updated command stack
   */
  @tailrec
  def parse(input: InputStream, stack: collection.immutable.Vector[ScriptElt] = Vector.empty[ScriptElt]): List[ScriptElt] = {
    val code = input.read()
    code match {
      case -1 => stack.toList
      case 0 => parse(input, stack :+ OP_0)
      case opCode if opCode > 0 && opCode < 0x4c => parse(input, stack :+ OP_PUSHDATA(bytes(input, opCode), opCode))
      case 0x4c => parse(input, stack :+ OP_PUSHDATA(bytes(input, uint8(input)), 0x4c))
      case 0x4d => parse(input, stack :+ OP_PUSHDATA(bytes(input, uint16(input)), 0x4d))
      case 0x4e => parse(input, stack :+ OP_PUSHDATA(bytes(input, uint32(input)), 0x4e))
      case opCode if code2elt.contains(opCode) => parse(input, stack :+ code2elt(opCode))
      case opCode => parse(input, stack :+ OP_INVALID(opCode)) // unknown/invalid ops can be parsed but not executed
    }
  }

  def parse(blob: ByteVector): List[ScriptElt] = if (blob.length > 10000) throw new RuntimeException("script is too large") else parse(new ByteArrayInputStream(blob.toArray))

  def parse(blob: Array[Byte]): List[ScriptElt] = parse(ByteVector.view(blob))

  @tailrec
  def write(script: Seq[ScriptElt], out: OutputStream): Unit = script match {
    case Nil => ()
    case OP_PUSHDATA(data, length) :: tail if data.length < 0x4c && data.length == length => out.write(data.length.toInt); out.write(data.toArray); write(tail, out)
    case OP_PUSHDATA(data, 0x4c) :: tail if data.length < 0xff => writeUInt8(0x4c, out); writeUInt8(data.length.toInt, out); out.write(data.toArray); write(tail, out)
    case OP_PUSHDATA(data, 0x4d) :: tail if data.length < 0xffff => writeUInt8(0x4d, out); writeUInt16(data.length.toInt, out); out.write(data.toArray); write(tail, out)
    case OP_PUSHDATA(data, 0x4e) :: tail if data.length < 0xffffffff => writeUInt8(0x4e, out); writeUInt32(data.length, out); out.write(data.toArray); write(tail, out)
    case op@OP_PUSHDATA(data, code) :: tail => throw new RuntimeException(s"invalid element $op")
    case head :: tail => out.write(elt2code(head)); write(tail, out)
  }

  def write(script: Seq[ScriptElt]): ByteVector = {
    val out = new ByteArrayOutputStream()
    write(script, out)
    ByteVector.view(out.toByteArray)
  }

  def isUpgradableNop(op: ScriptElt) = op match {
    case OP_NOP1 | OP_NOP4 | OP_NOP5 | OP_NOP6 | OP_NOP7 | OP_NOP8 | OP_NOP9 | OP_NOP10 => true
    case _ => false
  }

  def isSimpleValue(op: ScriptElt) = op match {
    case OP_1NEGATE | OP_0 | OP_1 | OP_2 | OP_3 | OP_4 | OP_5 | OP_6 | OP_7 | OP_8 | OP_9 | OP_10 | OP_11 | OP_12 | OP_13 | OP_14 | OP_15 | OP_16 => true
    case _ => false
  }

  def simpleValue(op: ScriptElt): Byte = {
    require(isSimpleValue(op))
    if (op == OP_0) 0 else (elt2code(op) - 0x50).toByte
  }

  def isDisabled(op: ScriptElt) = op match {
    case OP_CAT | OP_SUBSTR | OP_LEFT | OP_RIGHT | OP_INVERT | OP_AND | OP_OR | OP_XOR | OP_2MUL | OP_2DIV | OP_MUL | OP_DIV | OP_MOD | OP_LSHIFT | OP_RSHIFT => true
    case _ => false
  }

  def cost(op: ScriptElt): Int = op match {
    case _ if isSimpleValue(op) => 0
    case OP_PUSHDATA(_, _) => 0
    case OP_RESERVED => 0
    case _ => 1
  }

  def encodeNumber(value: Long): ByteVector = {
    if (value == 0) ByteVector.empty
    else {
      val result = ArrayBuffer.empty[Byte]
      val neg = value < 0
      var absvalue = if (neg) -value else value

      while (absvalue > 0) {
        result += (absvalue & 0xff).toByte
        absvalue >>= 8
      }

      //    - If the most significant byte is >= 0x80 and the value is positive, push a
      //    new zero-byte to make the significant byte < 0x80 again.

      //    - If the most significant byte is >= 0x80 and the value is negative, push a
      //    new 0x80 byte that will be popped off when converting to an integral.

      //    - If the most significant byte is < 0x80 and the value is negative, add
      //    0x80 to it, since it will be subtracted and interpreted as a negative when
      //    converting to an integral.

      if ((result.last & 0x80) != 0) {
        result += {
          if (neg) 0x80.toByte else 0
        }
      }
      else if (neg) {
        result(result.length - 1) = (result(result.length - 1) | 0x80).toByte
      }
      ByteVector.view(result.toArray)
    }
  }

  def decodeNumber(input: ByteVector, checkMinimalEncoding: Boolean, maximumSize: Int = 4): Long = {
    if (input.isEmpty) 0
    else if (input.length > maximumSize) throw new RuntimeException(s"number cannot be encoded on more than $maximumSize bytes")
    else {
      if (checkMinimalEncoding) {
        // Check that the number is encoded with the minimum possible
        // number of bytes.
        //
        // If the most-significant-byte - excluding the sign bit - is zero
        // then we're not minimal. Note how this test also rejects the
        // negative-zero encoding, 0x80.
        if ((input.last & 0x7f) == 0) {
          // One exception: if there's more than one byte and the most
          // significant bit of the second-most-significant-byte is set
          // it would conflict with the sign bit. An example of this case
          // is +-255, which encode to 0xff00 and 0xff80 respectively.
          // (big-endian).
          if (input.size <= 1 || (input(input.size - 2) & 0x80) == 0) {
            throw new RuntimeException("non-minimally encoded script number")
          }
        }
      }
      var result = 0L
      for (i <- input.toSeq.indices) {
        result |= (input(i) & 0xffL) << (8 * i)
      }

      // If the input vector's most significant byte is 0x80, remove it from
      // the result's msb and return a negative.
      if ((input.last & 0x80) != 0)
        -(result & ~(0x80L << (8 * (input.size - 1))))
      else
        result
    }
  }

  def castToBoolean(input: ByteVector): Boolean = input.toSeq.reverse match {
    case head +: tail if head == 0x80.toByte && tail.forall(_ == 0) => false
    case something if something.exists(_ != 0) => true
    case _ => false
  }

  def isPushOnly(script: Seq[ScriptElt]): Boolean = !script.exists {
    case op if isSimpleValue(op) => false
    case OP_PUSHDATA(_, _) => false
    case _ => true
  }

  def isPayToScript(script: Seq[ScriptElt]): Boolean = script match {
    case OP_HASH160 :: OP_PUSHDATA(multisigAddress, _) :: OP_EQUAL :: Nil if multisigAddress.length == 20 => true
    case _ => false
  }

  def isPayToScript(script: ByteVector): Boolean = script.length == 23 && script(0) == elt2code(OP_HASH160).toByte && script(1) == 0x14 && script(22) == elt2code(OP_EQUAL).toByte

  def isNativeWitnessScript(script: Seq[ScriptElt]): Boolean = script match {
    case (OP_0 | OP_1 | OP_2 | OP_3 | OP_4 | OP_5 | OP_6 | OP_7 | OP_8 | OP_9 | OP_10 | OP_11 | OP_12 | OP_13 | OP_14 | OP_15 | OP_16) :: OP_PUSHDATA(witnessProgram, _) :: Nil if witnessProgram.length >= 2 && witnessProgram.length <= 40 => true
    case _ => false
  }

  def isNativeWitnessScript(script: ByteVector): Boolean = isNativeWitnessScript(parse(script))

  def removeSignature(script: List[ScriptElt], signature: ByteVector): List[ScriptElt] = {
    val toRemove = OP_PUSHDATA(signature)
    script.filterNot(_ == toRemove)
  }

  def removeSignatures(script: List[ScriptElt], sigs: List[ByteVector]): List[ScriptElt] = sigs.foldLeft(script)(removeSignature)

  def checkLockTime(lockTime: Long, tx: Transaction, inputIndex: Int): Boolean = {
    // There are two kinds of nLockTime: lock-by-blockheight
    // and lock-by-blocktime, distinguished by whether
    // nLockTime < LOCKTIME_THRESHOLD.
    //
    // We want to compare apples to apples, so fail the script
    // unless the type of nLockTime being tested is the same as
    // the nLockTime in the transaction.
    if (!(
      (tx.lockTime < Transaction.LOCKTIME_THRESHOLD && lockTime < Transaction.LOCKTIME_THRESHOLD) ||
        (tx.lockTime >= Transaction.LOCKTIME_THRESHOLD && lockTime >= Transaction.LOCKTIME_THRESHOLD)
      )) {
      return false
    }

    // Now that we know we're comparing apples-to-apples, the
    // comparison is a simple numeric one.
    if (lockTime > tx.lockTime)
      return false

    // Finally the nLockTime feature can be disabled and thus
    // CHECKLOCKTIMEVERIFY bypassed if every txin has been
    // finalized by setting nSequence to maxint. The
    // transaction would be allowed into the blockchain, making
    // the opcode ineffective.
    //
    // Testing if this vin is not final is sufficient to
    // prevent this condition. Alternatively we could test all
    // inputs, but testing just this input minimizes the data
    // required to prove correct CHECKLOCKTIMEVERIFY execution.
    if (tx.txIn(inputIndex).isFinal)
      return false

    true
  }

  def checkSequence(sequence: Long, tx: Transaction, inputIndex: Int): Boolean = {
    // Relative lock times are supported by comparing the passed
    // in operand to the sequence number of the input.
    val txToSequence = tx.txIn(inputIndex).sequence

    // Fail if the transaction's version number is not set high
    // enough to trigger BIP 68 rules.
    if (tx.version < 2)
      return false

    // Sequence numbers with their most significant bit set are not
    // consensus constrained. Testing that the transaction's sequence
    // number do not have this bit set prevents using this property
    // to get around a CHECKSEQUENCEVERIFY check.
    if ((txToSequence & TxIn.SEQUENCE_LOCKTIME_DISABLE_FLAG) != 0)
      return false

    // Mask off any bits that do not have consensus-enforced meaning
    // before doing the integer comparisons
    val nLockTimeMask = TxIn.SEQUENCE_LOCKTIME_TYPE_FLAG | TxIn.SEQUENCE_LOCKTIME_MASK
    val txToSequenceMasked = txToSequence & nLockTimeMask
    val nSequenceMasked = sequence & nLockTimeMask

    // There are two kinds of nSequence: lock-by-blockheight
    // and lock-by-blocktime, distinguished by whether
    // nSequenceMasked < CTxIn::SEQUENCE_LOCKTIME_TYPE_FLAG.
    //
    // We want to compare apples to apples, so fail the script
    // unless the type of nSequenceMasked being tested is the same as
    // the nSequenceMasked in the transaction.
    if (!(
      (txToSequenceMasked < TxIn.SEQUENCE_LOCKTIME_TYPE_FLAG && nSequenceMasked < TxIn.SEQUENCE_LOCKTIME_TYPE_FLAG) ||
        (txToSequenceMasked >= TxIn.SEQUENCE_LOCKTIME_TYPE_FLAG && nSequenceMasked >= TxIn.SEQUENCE_LOCKTIME_TYPE_FLAG)
      )) {
      return false
    }

    // Now that we know we're comparing apples-to-apples, the
    // comparison is a simple numeric one.
    if (nSequenceMasked > txToSequenceMasked)
      return false

    true
  }

  /**
   * Execution context of a tx script. A script is always executed in the "context" of a transaction that is being
   * verified.
   *
   * @param tx         transaction that is being verified
   * @param inputIndex 0-based index of the tx input that is being processed
   */
  case class Context(tx: Transaction, inputIndex: Int, amount: Satoshi) {
    require(inputIndex >= 0 && inputIndex < tx.txIn.length, "invalid input index")
  }

  object Runner {

    /**
     * This class represents the state of the script execution engine
     *
     * @param conditions current "position" wrt if/notif/else/endif
     * @param altstack   initial alternate stack
     * @param opCount    initial op count
     * @param scriptCode initial script (can be modified by OP_CODESEPARATOR for example)
     */
    case class State(conditions: List[Boolean], altstack: Stack, opCount: Int, scriptCode: List[ScriptElt])

    type Callback = (List[ScriptElt], Stack, State) => Boolean
  }

  /**
   * Bitcoin script runner
   *
   * @param context    script execution context
   * @param scriptFlag script flags
   * @param callback   optional callback
   */
  class Runner(context: Context, scriptFlag: Int = MANDATORY_SCRIPT_VERIFY_FLAGS, callback: Option[Runner.Callback] = None) {

    import Runner._

    def checkSignature(pubKey: ByteVector, sigBytes: ByteVector, scriptCode: ByteVector, signatureVersion: Int): Boolean = {
      if (sigBytes.isEmpty) false
      else if (!Crypto.checkSignatureEncoding(sigBytes, scriptFlag)) throw new RuntimeException("invalid signature")
      else if (!Crypto.checkPubKeyEncoding(pubKey, scriptFlag, signatureVersion)) throw new RuntimeException("invalid public key")
      else if (!Crypto.isPubKeyValidLax(pubKey)) false // see how this is different from above ?
      else {
        val sigHashFlags = sigBytes.last & 0xff
        // sig hash is the last byte
        val sigBytes1 = sigBytes.take(sigBytes.length - 1) // drop sig hash
        if (sigBytes1.isEmpty) false
        else {
          val hash = Transaction.hashForSigning(context.tx, context.inputIndex, scriptCode, sigHashFlags, context.amount, signatureVersion)
          val result = Crypto.verifySignature(hash, Crypto.der2compact(sigBytes1), PublicKey.fromBin(pubKey))
          result
        }
      }
    }

    def checkSignatures(pubKeys: Seq[ByteVector], sigs: Seq[ByteVector], scriptCode: ByteVector, signatureVersion: Int): Boolean = sigs match {
      case Nil => true
      case _ if sigs.length > pubKeys.length => false
      case sig :: _ if !Crypto.checkSignatureEncoding(sig, scriptFlag) => throw new RuntimeException("invalid signature")
      case sig :: _ =>
        if (checkSignature(pubKeys.head, sig, scriptCode, signatureVersion))
          checkSignatures(pubKeys.tail, sigs.tail, scriptCode, signatureVersion)
        else
          checkSignatures(pubKeys.tail, sigs, scriptCode, signatureVersion)
    }

    def checkMinimalEncoding: Boolean = (scriptFlag & SCRIPT_VERIFY_MINIMALDATA) != 0

    def decodeNumber(input: ByteVector, maximumSize: Int = 4): Long = Script.decodeNumber(input, checkMinimalEncoding, maximumSize)

    /**
     * execute a serialized script, starting from an empty stack
     *
     * @param script serialized script
     * @return the stack created by the script
     */
    def run(script: ByteVector): Stack = run(parse(script))

    /**
     * execute a script, starting from an empty stack
     *
     * @param script
     * @return the stack created by the script
     */
    def run(script: List[ScriptElt]): Stack = run(script, List.empty[ByteVector])

    /**
     * execute a serialized script, starting from an existing stack
     *
     * @param script serialized script
     * @param stack  initial stack
     * @return the stack updated by the script
     */
    def run(script: ByteVector, stack: Stack): Stack = run(parse(script), stack)

    def run(script: List[ScriptElt], stack: Stack): Stack = run(script, stack, SigVersion.SIGVERSION_BASE)

    /**
     * execute a script, starting from an existing stack
     *
     * @param script           serialized script
     * @param stack            initial stack
     * @param signatureVersion signature version (0: use pre-segwit tx hash, 1: use segwit tx hash)
     * @return the stack updated by the script
     */
    def run(script: List[ScriptElt], stack: Stack, signatureVersion: Int): Stack =
      run(script, stack, State(conditions = List.empty[Boolean], altstack = List.empty[ByteVector], opCount = 0, scriptCode = script), signatureVersion)

    /**
     * execute a bitcoin script
     *
     * @param script script
     * @param stack  initial stack
     * @param state  initial state
     * @return the stack updated by the script
     */
    @tailrec
    final def run(script: List[ScriptElt], stack: Stack, state: State, signatureVersion: Int): Stack = {
      import state._
      callback.map(f => f(script, stack, state))
      if ((stack.length + altstack.length) > 1000) throw new RuntimeException(s"stack is too large: stack size = ${stack.length} alt stack size = ${altstack.length}")
      if (opCount > 201) throw new RuntimeException("operation count is over the limit")
      script match {
        // first, things that are always checked even in non-executed IF branches
        case Nil if conditions.nonEmpty => throw new RuntimeException("IF/ENDIF imbalance")
        case Nil => stack
        case op :: _ if isDisabled(op) => throw new RuntimeException(s"$op isdisabled")
        case OP_CODESEPARATOR :: _ if signatureVersion == SigVersion.SIGVERSION_BASE && (scriptFlag & SCRIPT_VERIFY_CONST_SCRIPTCODE) != 0 =>
          throw new RuntimeException("Using OP_CODESEPARATOR in non-witness script")
        case OP_VERIF :: _ => throw new RuntimeException("OP_VERIF is always invalid")
        case OP_VERNOTIF :: _ => throw new RuntimeException("OP_VERNOTIF is always invalid")
        case OP_PUSHDATA(data, _) :: _ if data.size > MaxScriptElementSize => throw new RuntimeException("Push value size limit exceeded")
        // check whether we are in a non-executed IF branch
        case OP_IF :: tail if conditions.contains(false) => run(tail, stack, state.copy(conditions = false :: conditions, opCount = opCount + 1), signatureVersion)
        case OP_IF :: tail => stack match {
          case True :: stacktail if signatureVersion == SigVersion.SIGVERSION_WITNESS_V0 && (scriptFlag & SCRIPT_VERIFY_MINIMALIF) != 0 => run(tail, stacktail, state.copy(conditions = true :: conditions, opCount = opCount + 1), signatureVersion)
          case False :: stacktail if signatureVersion == SigVersion.SIGVERSION_WITNESS_V0 && (scriptFlag & SCRIPT_VERIFY_MINIMALIF) != 0 => run(tail, stacktail, state.copy(conditions = false :: conditions, opCount = opCount + 1), signatureVersion)
          case _ :: stacktail if signatureVersion == SigVersion.SIGVERSION_WITNESS_V0 && (scriptFlag & SCRIPT_VERIFY_MINIMALIF) != 0 => throw new RuntimeException("OP_IF argument must be minimal")
          case head :: stacktail if castToBoolean(head) => run(tail, stacktail, state.copy(conditions = true :: conditions, opCount = opCount + 1), signatureVersion)
          case head :: stacktail => run(tail, stacktail, state.copy(conditions = false :: conditions, opCount = opCount + 1), signatureVersion)
        }
        case OP_NOTIF :: tail if conditions.contains(false) => run(tail, stack, state.copy(conditions = true :: conditions, opCount = opCount + 1), signatureVersion)
        case OP_NOTIF :: tail => stack match {
          case False :: stacktail if signatureVersion == SigVersion.SIGVERSION_WITNESS_V0 && (scriptFlag & SCRIPT_VERIFY_MINIMALIF) != 0 => run(tail, stacktail, state.copy(conditions = true :: conditions, opCount = opCount + 1), signatureVersion)
          case True :: stacktail if signatureVersion == SigVersion.SIGVERSION_WITNESS_V0 && (scriptFlag & SCRIPT_VERIFY_MINIMALIF) != 0 => run(tail, stacktail, state.copy(conditions = false :: conditions, opCount = opCount + 1), signatureVersion)
          case _ :: stacktail if signatureVersion == SigVersion.SIGVERSION_WITNESS_V0 && (scriptFlag & SCRIPT_VERIFY_MINIMALIF) != 0 => throw new RuntimeException("OP_NOTIF argument must be minimal")
          case head :: stacktail if castToBoolean(head) => run(tail, stacktail, state.copy(conditions = false :: conditions, opCount = opCount + 1), signatureVersion)
          case head :: stacktail => run(tail, stacktail, state.copy(conditions = true :: conditions, opCount = opCount + 1), signatureVersion)
        }
        case OP_ELSE :: tail => run(tail, stack, state.copy(conditions = !conditions.head :: conditions.tail, opCount = opCount + 1), signatureVersion)
        case OP_ENDIF :: tail => run(tail, stack, state.copy(conditions = conditions.tail, opCount = opCount + 1), signatureVersion)
        case head :: tail if conditions.contains(false) => run(tail, stack, state.copy(opCount = opCount + cost(head)), signatureVersion)
        // and now, things that are checked only in an executed IF branch
        case OP_0 :: tail => run(tail, ByteVector.empty :: stack, state, signatureVersion)
        case op :: tail if isSimpleValue(op) => run(tail, encodeNumber(simpleValue(op)) :: stack, state, signatureVersion)
        case OP_NOP :: tail => run(tail, stack, state.copy(opCount = opCount + 1), signatureVersion)
        case op :: tail if isUpgradableNop(op) && ((scriptFlag & SCRIPT_VERIFY_DISCOURAGE_UPGRADABLE_NOPS) != 0) => throw new RuntimeException("use of upgradable NOP is discouraged")
        case op :: tail if isUpgradableNop(op) => run(tail, stack, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_1ADD :: tail if stack.isEmpty => throw new RuntimeException("cannot run OP_1ADD on am empty stack")
        case OP_1ADD :: tail => run(tail, encodeNumber(decodeNumber(stack.head) + 1) :: stack.tail, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_1SUB :: tail if stack.isEmpty => throw new RuntimeException("cannot run OP_1SUB on am empty stack")
        case OP_1SUB :: tail => run(tail, encodeNumber(decodeNumber(stack.head) - 1) :: stack.tail, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_ABS :: tail if stack.isEmpty => throw new RuntimeException("cannot run OP_ABS on am empty stack")
        case OP_ABS :: tail => run(tail, encodeNumber(Math.abs(decodeNumber(stack.head))) :: stack.tail, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_ADD :: tail => stack match {
          case a :: b :: stacktail =>
            val x = decodeNumber(a)
            val y = decodeNumber(b)
            val result = x + y
            run(tail, encodeNumber(result) :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("cannot run OP_ADD on a stack with less than 2 elements")
        }
        case OP_BOOLAND :: tail => stack match {
          case x1 :: x2 :: stacktail =>
            val n1 = decodeNumber(x1)
            val n2 = decodeNumber(x2)
            val result = if (n1 != 0 && n2 != 0) 1 else 0
            run(tail, encodeNumber(result) :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("cannot run OP_BOOLAND on a stack with less than 2 elements")
        }
        case OP_BOOLOR :: tail => stack match {
          case x1 :: x2 :: stacktail =>
            val n1 = decodeNumber(x1)
            val n2 = decodeNumber(x2)
            val result = if (n1 != 0 || n2 != 0) 1 else 0
            run(tail, encodeNumber(result) :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("cannot run OP_BOOLOR on a stack with less than 2 elements")
        }
        case OP_CHECKLOCKTIMEVERIFY :: tail if ((scriptFlag & SCRIPT_VERIFY_CHECKLOCKTIMEVERIFY) != 0) => stack match {
          case head :: _ =>
            // Note that elsewhere numeric opcodes are limited to
            // operands in the range -2**31+1 to 2**31-1, however it is
            // legal for opcodes to produce results exceeding that
            // range. This limitation is implemented by CScriptNum's
            // default 4-byte limit.
            //
            // If we kept to that limit we'd have a year 2038 problem,
            // even though the nLockTime field in transactions
            // themselves is uint32 which only becomes meaningless
            // after the year 2106.
            //
            // Thus as a special case we tell CScriptNum to accept up
            // to 5-byte bignums, which are good until 2**39-1, well
            // beyond the 2**32-1 limit of the nLockTime field itself.
            val locktime = decodeNumber(head, maximumSize = 5)
            if (locktime < 0) throw new RuntimeException("CLTV lock time cannot be negative")
            if (!checkLockTime(locktime, context.tx, context.inputIndex)) throw new RuntimeException("unsatisfied CLTV lock time")
            // stack is not popped: we use stack here and not stacktail !!
            run(tail, stack, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("cannot run OP_CHECKLOCKTIMEVERIFY on an empty stack")
        }
        case OP_CHECKLOCKTIMEVERIFY :: _ if ((scriptFlag & SCRIPT_VERIFY_DISCOURAGE_UPGRADABLE_NOPS) != 0) => throw new RuntimeException("use of upgradable NOP is discouraged")
        case OP_CHECKLOCKTIMEVERIFY :: tail => run(tail, stack, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_CHECKSEQUENCEVERIFY :: tail if ((scriptFlag & SCRIPT_VERIFY_CHECKSEQUENCEVERIFY) != 0) => stack match {
          case head :: _ =>
            // nSequence, like nLockTime, is a 32-bit unsigned integer
            // field. See the comment in CHECKLOCKTIMEVERIFY regarding
            // 5-byte numeric operands.
            val sequence = decodeNumber(head, maximumSize = 5)
            // In the rare event that the argument may be < 0 due to
            // some arithmetic being done first, you can always use
            // 0 MAX CHECKSEQUENCEVERIFY.
            if (sequence < 0) throw new RuntimeException("CSV lock time cannot be negative")

            // To provide for future soft-fork extensibility, if the
            // operand has the disabled lock-time flag set,
            // CHECKSEQUENCEVERIFY behaves as a NOP.
            if ((sequence & TxIn.SEQUENCE_LOCKTIME_DISABLE_FLAG) == 0) {
              // Actually compare the specified inverse sequence number
              // with the input.
              if (!checkSequence(sequence, context.tx, context.inputIndex)) throw new RuntimeException("unsatisfied CSV lock time")
            }

            // stack is not popped: we use stack here and not stacktail !!
            run(tail, stack, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("cannot run OP_CHECKSEQUENCEVERIFY on an empty stack")
        }
        case OP_CHECKSEQUENCEVERIFY :: _ if ((scriptFlag & SCRIPT_VERIFY_DISCOURAGE_UPGRADABLE_NOPS) != 0) => throw new RuntimeException("use of upgradable NOP is discouraged")
        case OP_CHECKSEQUENCEVERIFY :: tail => run(tail, stack, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_CHECKSIG :: tail => stack match {
          case pubKey :: sigBytes :: stacktail => {
            // remove signature from script
            val scriptCode1 = if (signatureVersion == SigVersion.SIGVERSION_BASE) {
              val scriptCode1 = removeSignature(scriptCode, sigBytes)
              if (scriptCode1.length != scriptCode.length && (scriptFlag & SCRIPT_VERIFY_CONST_SCRIPTCODE) != 0)
                throw new RuntimeException("Signature is found in scriptCode")
              scriptCode1
            } else scriptCode
            val success = checkSignature(pubKey, sigBytes, Script.write(scriptCode1), signatureVersion)
            if (!success && (scriptFlag & SCRIPT_VERIFY_NULLFAIL) != 0) {
              require(sigBytes.isEmpty, "Signature must be zero for failed CHECKSIG operation")
            }
            run(tail, (if (success) True else False) :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          }
          case _ => throw new RuntimeException("Cannot perform OP_CHECKSIG on a stack with less than 2 elements")
        }
        case OP_CHECKSIGVERIFY :: tail => run(OP_CHECKSIG :: OP_VERIFY :: tail, stack, state.copy(opCount = opCount - 1), signatureVersion)
        case OP_CHECKMULTISIG :: tail => {
          // pop public keys
          val m = decodeNumber(stack.head).toInt
          if (m < 0 || m > 20) throw new RuntimeException("OP_CHECKMULTISIG: invalid number of public keys")
          val nextOpCount = opCount + 1 + m
          if (nextOpCount > 201) throw new RuntimeException("operation count is over the limit")
          val stack1 = stack.tail
          val pubKeys = stack1.take(m)
          val stack2 = stack1.drop(m)

          // pop signatures
          val n = decodeNumber(stack2.head).toInt
          if (n < 0 || n > m) throw new RuntimeException("OP_CHECKMULTISIG: invalid number of signatures")
          val stack3 = stack2.tail
          // check that we have at least n + 1 items on the stack (+1 because of a bug in the reference client)
          require(stack3.size >= n + 1, "invalid stack operation")
          val sigs = stack3.take(n)
          if ((scriptFlag & ScriptFlags.SCRIPT_VERIFY_NULLDUMMY) != 0) require(stack3(n).isEmpty, "multisig dummy is not empty")
          val stack4 = stack3.drop(n + 1)

          // Drop the signature in pre-segwit scripts but not segwit scripts
          val scriptCode1 = if (signatureVersion == SigVersion.SIGVERSION_BASE) {
            val scriptCode1 = removeSignatures(scriptCode, sigs)
            if (scriptCode1.length != scriptCode.length && (scriptFlag & SCRIPT_VERIFY_CONST_SCRIPTCODE) != 0)
              throw new RuntimeException("Signature is found in scriptCode")
            scriptCode1
          } else scriptCode
          val success = checkSignatures(pubKeys, sigs, Script.write(scriptCode1), signatureVersion)
          if (!success && (scriptFlag & SCRIPT_VERIFY_NULLFAIL) != 0) {
            sigs.foreach(sig => require(sig.isEmpty, "Signature must be zero for failed CHECKMULTISIG operation"))
          }
          run(tail, (if (success) True else False) :: stack4, state.copy(opCount = nextOpCount), signatureVersion)
        }
        case OP_CHECKMULTISIGVERIFY :: tail => run(OP_CHECKMULTISIG :: OP_VERIFY :: tail, stack, state.copy(opCount = opCount - 1), signatureVersion)
        case OP_CODESEPARATOR :: tail => run(tail, stack, state.copy(opCount = opCount + 1, scriptCode = tail), signatureVersion)
        case OP_DEPTH :: tail => run(tail, encodeNumber(stack.length) :: stack, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_SIZE :: _ if stack.isEmpty => throw new RuntimeException("Cannot run OP_SIZE on an empty stack")
        case OP_SIZE :: tail => run(tail, encodeNumber(stack.head.length) :: stack, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_DROP :: tail => run(tail, stack.tail, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_2DROP :: tail => run(tail, stack.tail.tail, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_DUP :: tail => run(tail, stack.head :: stack, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_2DUP :: tail => stack match {
          case x1 :: x2 :: stacktail => run(tail, x1 :: x2 :: x1 :: x2 :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_2DUP on a stack with less than 2 elements")
        }
        case OP_3DUP :: tail => stack match {
          case x1 :: x2 :: x3 :: stacktail => run(tail, x1 :: x2 :: x3 :: x1 :: x2 :: x3 :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_3DUP on a stack with less than 3 elements")
        }
        case OP_EQUAL :: tail => stack match {
          case a :: b :: stacktail if a != b => run(tail, False :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case a :: b :: stacktail => run(tail, True :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_EQUAL on a stack with less than 2 elements")
        }
        case OP_EQUALVERIFY :: tail => stack match {
          case a :: b :: _ if a != b => throw new RuntimeException("OP_EQUALVERIFY failed: elements are different")
          case a :: b :: stacktail => run(tail, stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_EQUALVERIFY on a stack with less than 2 elements")
        }
        case OP_FROMALTSTACK :: tail => run(tail, altstack.head :: stack, state.copy(altstack = altstack.tail), signatureVersion)
        case OP_HASH160 :: tail => run(tail, Crypto.hash160(stack.head) :: stack.tail, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_HASH256 :: tail => run(tail, Crypto.hash256(stack.head) :: stack.tail, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_IFDUP :: tail => stack match {
          case Nil => throw new RuntimeException("Cannot perform OP_IFDUP on an empty stack")
          case head :: _ if castToBoolean(head) => run(tail, head :: stack, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => run(tail, stack, state.copy(opCount = opCount + 1), signatureVersion)
        }
        case OP_LESSTHAN :: tail => stack match {
          case x1 :: x2 :: stacktail =>
            val result = if (decodeNumber(x2) < decodeNumber(x1)) 1 else 0
            run(tail, encodeNumber(result) :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_LESSTHAN on a stack with less than 2 elements")
        }
        case OP_LESSTHANOREQUAL :: tail => stack match {
          case x1 :: x2 :: stacktail =>
            val result = if (decodeNumber(x2) <= decodeNumber(x1)) 1 else 0
            run(tail, encodeNumber(result) :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_LESSTHANOREQUAL on a stack with less than 2 elements")
        }
        case OP_GREATERTHAN :: tail => stack match {
          case x1 :: x2 :: stacktail =>
            val result = if (decodeNumber(x2) > decodeNumber(x1)) 1 else 0
            run(tail, encodeNumber(result) :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_GREATERTHAN on a stack with less than 2 elements")
        }
        case OP_GREATERTHANOREQUAL :: tail => stack match {
          case x1 :: x2 :: stacktail =>
            val result = if (decodeNumber(x2) >= decodeNumber(x1)) 1 else 0
            run(tail, encodeNumber(result) :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_GREATERTHANOREQUAL on a stack with less than 2 elements")
        }
        case OP_MAX :: tail => stack match {
          case x1 :: x2 :: stacktail =>
            val n1 = decodeNumber(x1)
            val n2 = decodeNumber(x2)
            val result = if (n1 > n2) n1 else n2
            run(tail, encodeNumber(result) :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_MAX on a stack with less than 2 elements")
        }
        case OP_MIN :: tail => stack match {
          case x1 :: x2 :: stacktail =>
            val n1 = decodeNumber(x1)
            val n2 = decodeNumber(x2)
            val result = if (n1 < n2) n1 else n2
            run(tail, encodeNumber(result) :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_MIN on a stack with less than 2 elements")
        }
        case OP_NEGATE :: tail if stack.isEmpty => throw new RuntimeException("cannot run OP_NEGATE on am empty stack")
        case OP_NEGATE :: tail => run(tail, encodeNumber(-decodeNumber(stack.head)) :: stack.tail, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_NIP :: tail => stack match {
          case x1 :: x2 :: stacktail => run(tail, x1 :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_NIP on a stack with less than 2 elements")
        }
        case OP_NOT :: tail if stack.isEmpty => throw new RuntimeException("cannot run OP_NOT on am empty stack")
        case OP_NOT :: tail => run(tail, encodeNumber(if (decodeNumber(stack.head) == 0) 1 else 0) :: stack.tail, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_0NOTEQUAL :: tail if stack.isEmpty => throw new RuntimeException("cannot run OP_0NOTEQUAL on am empty stack")
        case OP_0NOTEQUAL :: tail => run(tail, encodeNumber(if (decodeNumber(stack.head) == 0) 0 else 1) :: stack.tail, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_NUMEQUAL :: tail => stack match {
          case x1 :: x2 :: stacktail =>
            val result = if (decodeNumber(x1) == decodeNumber(x2)) 1 else 0
            run(tail, encodeNumber(result) :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_NUMEQUAL on a stack with less than 2 elements")
        }
        case OP_NUMEQUALVERIFY :: tail => stack match {
          case x1 :: x2 :: stacktail =>
            if (decodeNumber(x1) != decodeNumber(x2)) throw new RuntimeException("OP_NUMEQUALVERIFY failed")
            run(tail, stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_NUMEQUALVERIFY on a stack with less than 2 elements")
        }
        case OP_NUMNOTEQUAL :: tail => stack match {
          case x1 :: x2 :: stacktail =>
            val result = if (decodeNumber(x1) != decodeNumber(x2)) 1 else 0
            run(tail, encodeNumber(result) :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_NUMNOTEQUAL on a stack with less than 2 elements")
        }
        case OP_OVER :: tail => stack match {
          case _ :: x2 :: _ => run(tail, x2 :: stack, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_OVER on a stack with less than 2 elements")
        }
        case OP_2OVER :: tail => stack match {
          case _ :: _ :: x3 :: x4 :: _ => run(tail, x3 :: x4 :: stack, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_2OVER on a stack with less than 4 elements")
        }
        case OP_PICK :: tail => stack match {
          case head :: stacktail =>
            val n = decodeNumber(head).toInt
            run(tail, stacktail(n) :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_PICK on a stack with less than 1 elements")
        }
        case OP_PUSHDATA(data, code) :: _ if ((scriptFlag & SCRIPT_VERIFY_MINIMALDATA) != 0) && !OP_PUSHDATA.isMinimal(data, code) => {
          throw new RuntimeException("not minimal push")
        }
        case OP_PUSHDATA(data, _) :: tail => run(tail, data :: stack, state, signatureVersion)
        case OP_ROLL :: tail => stack match {
          case head :: stacktail =>
            val n = decodeNumber(head).toInt
            run(tail, stacktail(n) :: stacktail.take(n) ::: stacktail.takeRight(stacktail.length - 1 - n), state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_ROLL on a stack with less than 1 elements")
        }
        case OP_ROT :: tail => stack match {
          case x1 :: x2 :: x3 :: stacktail => run(tail, x3 :: x1 :: x2 :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_ROT on a stack with less than 3 elements")
        }
        case OP_2ROT :: tail => stack match {
          case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: stacktail => run(tail, x5 :: x6 :: x1 :: x2 :: x3 :: x4 :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_2ROT on a stack with less than 6 elements")
        }
        case OP_RIPEMD160 :: tail => run(tail, Crypto.ripemd160(stack.head) :: stack.tail, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_SHA1 :: tail => run(tail, Crypto.sha1(stack.head) :: stack.tail, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_SHA256 :: tail => run(tail, Crypto.sha256(stack.head) :: stack.tail, state.copy(opCount = opCount + 1), signatureVersion)
        case OP_SUB :: tail => stack match {
          case x1 :: x2 :: stacktail =>
            val result = decodeNumber(x2) - decodeNumber(x1)
            run(tail, encodeNumber(result) :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("cannot run OP_SUB on a stack of less than 2 elements")
        }

        case OP_SWAP :: tail => stack match {
          case x1 :: x2 :: stacktail => run(tail, x2 :: x1 :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_SWAP on a stack with less than 2 elements")
        }
        case OP_2SWAP :: tail => stack match {
          case x1 :: x2 :: x3 :: x4 :: stacktail => run(tail, x3 :: x4 :: x1 :: x2 :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_2SWAP on a stack with less than 4 elements")
        }
        case OP_TOALTSTACK :: tail => run(tail, stack.tail, state.copy(altstack = stack.head :: altstack), signatureVersion)
        case OP_TUCK :: tail => stack match {
          case x1 :: x2 :: stacktail => run(tail, x1 :: x2 :: x1 :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_TUCK on a stack with less than 2 elements")
        }
        case OP_VERIFY :: tail => stack match {
          case Nil => throw new RuntimeException("cannot run OP_VERIFY on an empty stack")
          case head :: _ if !castToBoolean(head) => throw new RuntimeException("OP_VERIFY failed")
          case _ :: stacktail => run(tail, stacktail, state.copy(opCount = opCount + 1), signatureVersion)
        }
        case OP_WITHIN :: tail => stack match {
          case encMax :: encMin :: encN :: stacktail =>
            val max = decodeNumber(encMax)
            val min = decodeNumber(encMin)
            val n = decodeNumber(encN)
            val result = if (n >= min && n < max) 1 else 0
            run(tail, encodeNumber(result) :: stacktail, state.copy(opCount = opCount + 1), signatureVersion)
          case _ => throw new RuntimeException("Cannot perform OP_WITHIN on a stack with less than 3 elements")
        }
      }
    }

    def verifyWitnessProgram(witness: ScriptWitness, witnessVersion: Long, program: ByteVector): Unit = {
      val (stack: Seq[ByteVector], scriptPubKey) = witnessVersion match {
        case 0 if program.length == 20 =>
          // P2WPKH, program is simply the pubkey hash
          require(witness.stack.length == 2, "Invalid witness program, should have 2 items")
          (witness.stack, OP_DUP :: OP_HASH160 :: OP_PUSHDATA(program) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil)
        case 0 if program.length == 32 =>
          // P2WPSH, program is the hash of the script, and witness is the stack + the script
          val check = Crypto.sha256(witness.stack.last)
          require(check.bytes == program, "witness program mismatch")
          (witness.stack.dropRight(1), Script.parse(witness.stack.last))
        case 0 =>
          throw new IllegalArgumentException(s"Invalid witness program length: ${program.length}")
        case _ if (scriptFlag & SCRIPT_VERIFY_DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM) != 0 =>
          throw new IllegalArgumentException(s"Invalid witness version: ${witnessVersion}")
        case _ =>
          // Higher version witness scripts return true for future softfork compatibility
          return
      }
      stack.foreach(item => require(item.length <= MaxScriptElementSize, "item is bigger than maximum push size"))

      val stack1 = run(scriptPubKey, stack.toList.reverse, SigVersion.SIGVERSION_WITNESS_V0)
      require(stack1.length == 1)
      require(castToBoolean(stack1.head))
    }

    def verifyScripts(scriptSig: ByteVector, scriptPubKey: ByteVector): Boolean = verifyScripts(scriptSig, scriptPubKey, ScriptWitness.empty)

    /**
     * verify a script sig/script pubkey pair:
     * <ul>
     * <li>parse and run script sig</li>
     * <li>parse and run script pubkey using the stack generated by the previous step</li>
     * <li>check the final stack</li>
     * <li>extract and run embedded pay2sh scripts if any and check the stack again</li>
     * </ul>
     *
     * @param scriptSig    signature script
     * @param scriptPubKey public key script
     * @return true if the scripts were successfully verified
     */
    def verifyScripts(scriptSig: ByteVector, scriptPubKey: ByteVector, witness: ScriptWitness): Boolean = {
      def checkStack(stack: Stack): Boolean = {
        if (stack.isEmpty) false
        else if (!Script.castToBoolean(stack.head)) false
        else if ((scriptFlag & SCRIPT_VERIFY_CLEANSTACK) != 0) {
          if ((scriptFlag & SCRIPT_VERIFY_P2SH) == 0) throw new RuntimeException("illegal script flag")
          stack.size == 1
        }
        else true
      }

      if ((scriptFlag & SCRIPT_VERIFY_WITNESS) != 0) {
        // We can't check for correct unexpected witness data if P2SH was off, so require
        // that WITNESS implies P2SH. Otherwise, going from WITNESS->P2SH+WITNESS would be
        // possible, which is not a softfork.
        require((scriptFlag & SCRIPT_VERIFY_P2SH) != 0)
      }
      val ssig = Script.parse(scriptSig)
      if (((scriptFlag & SCRIPT_VERIFY_SIGPUSHONLY) != 0) && !Script.isPushOnly(ssig)) throw new RuntimeException("signature script is not PUSH-only")
      val stack = run(ssig)

      val spub = Script.parse(scriptPubKey)
      val stack0 = run(spub, stack)
      require(stack0.nonEmpty, "Script verification failed, stack should not be empty")
      require(castToBoolean(stack0.head), "Script verification failed, stack starts with 'false'")

      var hadWitness = false
      val stack1 = if ((scriptFlag & SCRIPT_VERIFY_WITNESS) != 0) {
        spub match {
          case op :: OP_PUSHDATA(program, code) :: Nil if isSimpleValue(op) && OP_PUSHDATA.isMinimal(program, code) && program.length >= 2 && program.length <= 40 => {
            hadWitness = true
            val witnessVersion = simpleValue(op)
            require(ssig.isEmpty, "Malleated segwit script")
            verifyWitnessProgram(witness, witnessVersion, program)
            stack0.take(1)
          }
          case _ => stack0
        }
      } else stack0

      val stack2 = if (((scriptFlag & SCRIPT_VERIFY_P2SH) != 0) && Script.isPayToScript(scriptPubKey)) {
        // scriptSig must be literals-only or validation fails
        if (!Script.isPushOnly(ssig)) throw new RuntimeException("signature script is not PUSH-only")

        // pay to script:
        // script sig is built as sig1 :: ... :: sigN :: serialized_script :: Nil
        // and script pubkey is HASH160 :: hash :: EQUAL :: Nil
        // if we got here after running script pubkey, it means that hash == HASH160(serialized script)
        // and stack would be serialized_script :: sigN :: ... :: sig1 :: Nil
        // we pop the first element of the stack, deserialize it and run it against the rest of the stack
        val stackp2sh = run(stack.head, stack.tail)
        require(stackp2sh.nonEmpty, "Script verification failed, stack should not be empty")
        require(castToBoolean(stackp2sh.head), "Script verification failed, stack starts with 'false'")

        if ((scriptFlag & SCRIPT_VERIFY_WITNESS) != 0) {
          Script.parse(stack.head) match {
            case op :: OP_PUSHDATA(program, _) :: Nil if isSimpleValue(op) && program.length >= 2 && program.length <= 32 => {
              hadWitness = true
              val witnessVersion = simpleValue(op)
              //require(ssig.isEmpty, "Malleated segwit script")
              verifyWitnessProgram(witness, witnessVersion, program)
              stackp2sh.take(1)
            }
            case _ => stackp2sh
          }
        } else stackp2sh
      } else stack1

      if ((scriptFlag & SCRIPT_VERIFY_WITNESS) != 0 && !hadWitness) {
        require(witness.isNull)
      }
      checkStack(stack2)
    }
  }

  /**
   * extract a public key hash from a public key script
   *
   * @param script public key script
   * @return the public key hash wrapped in the script
   */
  def publicKeyHash(script: List[ScriptElt]): ByteVector = script match {
    case OP_DUP :: OP_HASH160 :: OP_PUSHDATA(data, _) :: OP_EQUALVERIFY :: OP_CHECKSIG :: OP_NOP :: Nil => data // non standard pay to pubkey...
    case OP_DUP :: OP_HASH160 :: OP_PUSHDATA(data, _) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil => data // standard pay to pubkey
    case OP_HASH160 :: OP_PUSHDATA(data, _) :: OP_EQUAL :: Nil if data.size == 20 => data // standard pay to script
  }

  def publicKeyHash(script: ByteVector): ByteVector = publicKeyHash(parse(script))

  /**
   * extract a public key from a signature script
   *
   * @param script signature script
   * @return the public key wrapped in the script
   */
  def publicKey(script: List[ScriptElt]): ByteVector = script match {
    case OP_PUSHDATA(data1, _) :: OP_PUSHDATA(data2, _) :: Nil if data1.length > 2 && data2.length > 2 => data2
    case OP_PUSHDATA(data, _) :: OP_CHECKSIG :: Nil => data
  }

  /**
   * Creates a m-of-n multisig script.
   *
   * @param m       is the number of required signatures
   * @param pubkeys are the public keys signatures will be checked against (there should be at least as many public keys
   *                as required signatures)
   * @return a multisig redeem script
   */
  def createMultiSigMofN(m: Int, pubkeys: Seq[PublicKey]): Seq[ScriptElt] = {
    require(m > 0 && m <= 16, s"number of required signatures is $m, should be between 1 and 16")
    require(pubkeys.nonEmpty && pubkeys.size <= 16, s"number of public keys is ${pubkeys.size}, should be between 1 and 16")
    require(m <= pubkeys.size, "The required number of signatures shouldn't be greater than the number of public keys")
    val op_m = ScriptElt.code2elt(m + 0x50)
    // 1 -> OP_1, 2 -> OP_2, ... 16 -> OP_16
    val op_n = ScriptElt.code2elt(pubkeys.size + 0x50)
    op_m :: pubkeys.toList.map(pub => OP_PUSHDATA(pub.value)) ::: op_n :: OP_CHECKMULTISIG :: Nil
  }

  /**
   * @param pubKeyHash public key hash
   * @return a pay-to-public-key-hash script
   */
  def pay2pkh(pubKeyHash: ByteVector): Seq[ScriptElt] = {
    require(pubKeyHash.length == 20, "pubkey hash length must be 20 bytes")
    OP_DUP :: OP_HASH160 :: OP_PUSHDATA(pubKeyHash) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil
  }

  /**
   * @param pubKey public key
   * @return a pay-to-public-key-hash script
   */
  def pay2pkh(pubKey: PublicKey): Seq[ScriptElt] = pay2pkh(pubKey.hash160)

  def isPay2pkh(script: Seq[ScriptElt]): Boolean = {
    script match {
      case OP_DUP :: OP_HASH160 :: OP_PUSHDATA(data, _) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil if data.length == 20 => true
      case _ => false
    }
  }

  /**
   * @param script bitcoin script
   * @return a pay-to-script script
   */
  def pay2sh(script: Seq[ScriptElt]): Seq[ScriptElt] = pay2sh(Script.write(script))

  /**
   * @param script bitcoin script
   * @return a pay-to-script script
   */
  def pay2sh(script: ByteVector): Seq[ScriptElt] = OP_HASH160 :: OP_PUSHDATA(hash160(script)) :: OP_EQUAL :: Nil

  def isPay2sh(script: Seq[ScriptElt]): Boolean = {
    script match {
      case OP_HASH160 :: OP_PUSHDATA(data, _) :: OP_EQUAL :: Nil if data.length == 20 => true
      case _ => false
    }
  }

  /**
   * @param script bitcoin script
   * @return a pay-to-witness-script script
   */
  def pay2wsh(script: Seq[ScriptElt]): Seq[ScriptElt] = pay2wsh(Script.write(script))

  /**
   * @param script bitcoin script
   * @return a pay-to-witness-script script
   */
  def pay2wsh(script: ByteVector): Seq[ScriptElt] = OP_0 :: OP_PUSHDATA(sha256(script)) :: Nil

  def isPay2wsh(script: Seq[ScriptElt]): Boolean = {
    script match {
      case OP_0 :: OP_PUSHDATA(data, _) :: Nil if data.length == 32 => true
      case _ => false
    }
  }

  /**
   * @param pubKeyHash public key hash
   * @return a pay-to-witness-public-key-hash script
   */
  def pay2wpkh(pubKeyHash: ByteVector): Seq[ScriptElt] = {
    require(pubKeyHash.length == 20, "pubkey hash length must be 20 bytes")
    OP_0 :: OP_PUSHDATA(pubKeyHash) :: Nil
  }

  /**
   * @param pubKey public key
   * @return a pay-to-witness-public-key-hash script
   */
  def pay2wpkh(pubKey: PublicKey): Seq[ScriptElt] = pay2wpkh(pubKey.hash160)

  def isPay2wpkh(script: Seq[ScriptElt]): Boolean = {
    script match {
      case OP_0 :: OP_PUSHDATA(data, _) :: Nil if data.length == 20 => true
      case _ => false
    }
  }

  /**
   * @param pubKey public key
   * @param sig    signature matching the public key
   * @return script witness for the corresponding pay-to-witness-public-key-hash script
   */
  def witnessPay2wpkh(pubKey: PublicKey, sig: ByteVector): ScriptWitness = ScriptWitness(sig :: pubKey.value :: Nil)

}
