package trading.tacticaladvantage

import android.os.Bundle
import android.view.View
import android.widget._
import androidx.appcompat.app.AlertDialog
import androidx.transition.TransitionManager
import fr.acinq.bitcoin.MnemonicCode
import immortan.crypto.Tools.{SEPARATOR, StringList, none}
import immortan.{MasterKeys, WalletSecret}
import trading.tacticaladvantage.R.string._

trait MnemonicActivity { me: BaseActivity =>
  val activityContainer: LinearLayout

  def showMnemonicInput(titleRes: Int)(proceedWithMnemonics: StringList => Unit): Unit = {
    val mnemonicWrap = getLayoutInflater.inflate(R.layout.frag_mnemonic, null).asInstanceOf[LinearLayout]
    val recoveryPhrase = mnemonicWrap.findViewById(R.id.recoveryPhrase).asInstanceOf[com.hootsuite.nachos.NachoTextView]
    recoveryPhrase.addChipTerminator(' ', com.hootsuite.nachos.terminator.ChipTerminatorHandler.BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    recoveryPhrase.addChipTerminator(',', com.hootsuite.nachos.terminator.ChipTerminatorHandler.BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    recoveryPhrase.addChipTerminator('\n', com.hootsuite.nachos.terminator.ChipTerminatorHandler.BEHAVIOR_CHIPIFY_TO_TERMINATOR)
    recoveryPhrase setAdapter new ArrayAdapter(me, android.R.layout.simple_list_item_1, englishWordList)

    def getMnemonicList: StringList = {
      val mnemonic = recoveryPhrase.getText.toString.toLowerCase.trim
      val pureMnemonic = mnemonic.replaceAll("[^a-zA-Z0-9']+", SEPARATOR)
      pureMnemonic.split(SEPARATOR).toList
    }

    val proceed: AlertDialog => Unit = alert => try {
      MnemonicCode.validate(getMnemonicList, englishWordList)
      if (alert.isShowing) proceedWithMnemonics(getMnemonicList)
      alert.dismiss
    } catch {
      case exception: Throwable =>
        val msg = getString(R.string.error_wrong_phrase)
        onFail(msg format exception.getMessage)
    }

    val builder = titleBodyAsViewBuilder(getString(titleRes).asDefView, mnemonicWrap)
    val alert = mkCheckForm(proceed, none, builder, R.string.dialog_ok, R.string.dialog_cancel)
    updatePopupButton(getPositiveButton(alert), isEnabled = false)

    recoveryPhrase addTextChangedListener onTextChange { _ =>
      updatePopupButton(getPositiveButton(alert), getMnemonicList.size > 11)
    }
  }

  lazy val englishWordList: Array[String] = {
    val rawData = getAssets.open("bip39_english_wordlist.txt")
    scala.io.Source.fromInputStream(rawData, "UTF-8").getLines.toArray
  }
}

class SetupActivity extends BaseActivity with MnemonicActivity { me =>
  lazy val activityContainer = findViewById(R.id.activitySetupMain).asInstanceOf[LinearLayout]

  val proceedWithMnemonics: StringList => Unit = mnemonic => {
    val walletSeed = MnemonicCode.toSeed(mnemonic, passphrase = new String)
    val secret = WalletSecret(MasterKeys.fromSeed(walletSeed.toArray), mnemonic, walletSeed)

    // Make local Biconomy bundle accessible to Node
    WalletApp.assetToInternal("server.js", "server.js")

    // Before creating wallets
    WalletApp.extDataBag.putSecret(secret)
    WalletApp.makeOperational(secret)

    // Create wallets
    WalletApp.createBtcWallet(ord = 0L)
//    WalletApp.createUsdtWallet(ord = 0L)
    WalletApp.initWallets

    // Proceed to main activity
    TransitionManager.beginDelayedTransition(activityContainer)
    activityContainer.setVisibility(View.GONE)
    exitTo(ClassNames.mainActivityClass)
  }

  override def START(s: Bundle): Unit = {
    setContentView(R.layout.activity_setup)
  }

  def createNewWallet(view: View): Unit = {
    val twelveWordsEntropy = fr.acinq.eclair.randomBytes(length = 16)
    val mnemonic = MnemonicCode.toMnemonics(twelveWordsEntropy, englishWordList)
    proceedWithMnemonics(mnemonic)
  }

  def showMnemonicPopup(view: View): Unit = {
    showMnemonicInput(action_recovery_phrase_title)(proceedWithMnemonics)
  }
}
