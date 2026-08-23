package trading.tacticaladvantage

import android.os.Bundle
import android.view.View
import android.widget._
import androidx.appcompat.app.AlertDialog
import fr.acinq.bitcoin.MnemonicCode
import Tools._
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet
import trading.tacticaladvantage.BaseActivity.StringOps
import trading.tacticaladvantage.R.string._

trait MnemonicActivity { me: BaseActivity =>
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
    recoveryPhrase addTextChangedListener onTextChange(_ => updatePosButton(alert, getMnemonicList.size > 11).run)
    updatePosButton(alert, isEnabled = false).run
  }

  def viewRecoveryCode: Unit = {
    val content = new TitleView(me getString settings_view_revocery_phrase_ext)
    new AlertDialog.Builder(me).setView(content.view).show

    for (mnemonicWord \ mnemonicIndex <- WalletApp.secret.mnemonic.zipWithIndex) {
      val oneWord = s"<font color=$cardZero>${mnemonicIndex + 1}</font> $mnemonicWord"
      addFlowChip(content.flow, oneWord, R.drawable.border_white, None)
    }
  }

  lazy val englishWordList: Array[String] = {
    val rawData = getAssets.open("bip39_english_wordlist.txt")
    scala.io.Source.fromInputStream(rawData, "UTF-8").getLines.toArray
  }
}

class SetupActivity extends BaseActivity with MnemonicActivity { me =>
  lazy val devInfo = me clickableTextField findViewById(R.id.devInfo).asInstanceOf[TextView]
  lazy val fancyAppName = findViewById(R.id.fancyAppName).asInstanceOf[TextView]

  def proceedWithMnemonics(words: StringList, ecxLegacy: Boolean): Unit = {
    val walletSeed = MnemonicCode.toSeed(mnemonics = words, passphrase = new String)
    val secret = WalletSecret(MasterKeys.fromSeed(walletSeed.toArray), words, walletSeed)

    WalletApp.btc.createWallet(secret.keys.bitcoinMaster, ElectrumWallet.BIP84)
//    WalletApp.ecx.createWallet(secret.keys.bitcoinMaster, ElectrumWallet.BIP84)
//
//    if (ecxLegacy) {
//      WalletApp.ecx.createWallet(secret.keys.bitcoinMaster, ElectrumWallet.BIP44)
//      WalletApp.ecx.createWallet(secret.keys.bitcoinMaster, ElectrumWallet.BIP32)
//    }

    WalletApp.btc.extDataBag.putSecret(secret)
    me exitTo classOf[MainActivity]
  }

  override def START(s: Bundle): Unit = {
    setContentView(R.layout.activity_setup)
    fancyAppName.setText(me getString app_name)
    devInfo.setText(getString(dev_info).html)
  }

  def createNewWallet(view: View): Unit = {
    val twelveWordsEntropy = fr.acinq.eclair.randomBytes(length = 16)
    val words = MnemonicCode.toMnemonics(twelveWordsEntropy, englishWordList)
    proceedWithMnemonics(words, ecxLegacy = false)
  }

  def showMnemonicPopup(view: View): Unit =
    showMnemonicInput(action_recovery_phrase_title) { words =>
      proceedWithMnemonics(words, ecxLegacy = true)
    }
}
