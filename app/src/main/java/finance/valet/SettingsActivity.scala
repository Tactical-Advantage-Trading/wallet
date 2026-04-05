package finance.valet

import android.app.Activity
import android.content.Intent
import android.os.{Build, Bundle}
import android.view.{View, ViewGroup}
import android.widget._
import finance.valet.BaseActivity.StringOps
import finance.valet.BuildConfig.{VERSION_CODE, VERSION_NAME}
import finance.valet.Colors._
import finance.valet.R.string._
import finance.valet.sheets.{BaseChoiceBottomSheet, PairingData}
import finance.valet.utils.{LocalBackup, OnListItemClickListener}
import com.google.android.material.snackbar.Snackbar
import com.guardanis.applock.AppLock
import fr.acinq.bitcoin.Satoshi
import fr.acinq.eclair.MilliSatoshi
import fr.acinq.eclair.blockchain.EclairWallet
import fr.acinq.eclair.blockchain.EclairWallet._
import fr.acinq.eclair.blockchain.electrum.db.{SigningWallet, WatchingWallet}
import fr.acinq.eclair.wire.CommonCodecs.nodeaddress
import fr.acinq.eclair.wire.{Domain, NodeAddress}
import immortan.crypto.Tools._
import immortan.utils.{BtcDenomination, SatDenomination}
import immortan.{ChannelMaster, LNParams}

import scala.util.Success


abstract class SettingsHolder(host: BaseActivity) {
  val view: RelativeLayout = host.getLayoutInflater.inflate(R.layout.frag_switch, null, false).asInstanceOf[RelativeLayout]
  val settingsCheck: CheckBox = view.findViewById(R.id.settingsCheck).asInstanceOf[CheckBox]
  val settingsTitle: TextView = view.findViewById(R.id.settingsTitle).asInstanceOf[TextView]
  val settingsInfo: TextView = view.findViewById(R.id.settingsInfo).asInstanceOf[TextView]
  val REQUEST_CODE_CREATE_LOCK: Int = 103
  def updateView: Unit

  def putBoolAndUpdateView(key: String, value: Boolean): Unit = {
    WalletApp.app.prefs.edit.putBoolean(key, value).commit
    updateView
  }

  def disableIfOldAndroid: Unit =
    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.N) {
      val message = host.getString(error_old_api_level).format(Build.VERSION.SDK_INT)
      host.setVis(isVisible = true, settingsInfo)
      settingsInfo.setText(message)
      settingsTitle.setAlpha(0.5F)
      view.setEnabled(false)
    }
}

class SettingsActivity extends BaseCheckActivity with HasTypicalChainFee with ChoiceReceiver { me =>
  lazy private[this] val settingsContainer = findViewById(R.id.settingsContainer).asInstanceOf[LinearLayout]
  private[this] val fiatSymbols = LNParams.fiatRates.universallySupportedSymbols.toList.sorted
  private[this] val CHOICE_FIAT_DENOMINATION_TAG = "choiceFiatDenominationTag"
  private[this] val CHOICE_BTC_DENOMINATON_TAG = "choiceBtcDenominationTag"
  private[this] val units = List(SatDenomination, BtcDenomination)
  private[this] final val DIRECTORY_REQUEST_CODE = 113

  override def onResume: Unit = {
    storeLocalBackup.updateView
    backupLocation.updateView
    chainWallets.updateView
    electrum.updateView
    setFiat.updateView
    setBtc.updateView

    useBiometric.updateView
    enforceTor.updateView
    super.onResume
  }

  override def onChoiceMade(tag: AnyRef, pos: Int): Unit = tag match {

    case CHOICE_FIAT_DENOMINATION_TAG =>
      val fiatCode ~ _ = fiatSymbols(pos)
      WalletApp.app.prefs.edit.putString(WalletApp.FIAT_CODE, fiatCode).commit
      ChannelMaster.next(ChannelMaster.stateUpdateStream)
      LNParams.fiatRates.focused = Some(fiatCode)
      UITask(LNParams.fiatRates.updateNow).run
      setFiat.updateView


    case CHOICE_BTC_DENOMINATON_TAG =>
      WalletApp.app.prefs.edit.putString(WalletApp.BTC_DENOM, units(pos).sign).commit
      ChannelMaster.next(ChannelMaster.stateUpdateStream)
      setBtc.updateView

    case _ =>
  }

  lazy private[this] val storeLocalBackup = new SettingsHolder(me) {
    setVis(isVisible = false, settingsCheck)

    def updateView: Unit = {
      val backupAllowed = LocalBackup.isAllowed(context = WalletApp.app)
      if (backupAllowed && LNParams.cm.all.nonEmpty) WalletApp.backupSaveWorker.replaceWork(false)
      val title = if (backupAllowed) settings_backup_enabled else settings_backup_disabled
      val info = settings_backup_info
      settingsTitle.setText(title)
      settingsInfo.setText(info)
    }

    view setOnClickListener onButtonTap {
      val intent = (new Intent).setAction(android.provider.Settings.ACTION_APPLICATION_DETAILS_SETTINGS)
      val intent1 = intent setData android.net.Uri.fromParts("package", getPackageName, null)
      startActivity(intent1)
    }
  }

  lazy private[this] val backupLocation = new SettingsHolder(me) {
    setVis(isVisible = false, settingsCheck)

    def updateView: Unit = {
      val customLocation = WalletApp.customBackupLocation
      val info: String = if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
        customLocation match {
          case Some(path) => path.getPath
          case None => me getString settings_backup_where
        }
      } else me getString settings_backup_location_too_old
      settingsTitle.setText(settings_backup_location)
      settingsInfo.setText(info)
    }

    view setOnClickListener onButtonTap {
      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
        val i = new Intent(Intent.ACTION_OPEN_DOCUMENT_TREE)
        i.addCategory(Intent.CATEGORY_DEFAULT)
        i.addFlags(Intent.FLAG_GRANT_WRITE_URI_PERMISSION)
        i.addFlags(Intent.FLAG_GRANT_PERSISTABLE_URI_PERMISSION)
        startActivityForResult(Intent.createChooser(i, me getString settings_choose_directory), DIRECTORY_REQUEST_CODE)
      }
    }
  }

  override def onActivityResult(requestCode: Int, resultCode: Int, resultData: Intent): Unit =
    if (requestCode == DIRECTORY_REQUEST_CODE && resultCode == Activity.RESULT_OK && resultData != null) {
      def saveDirectory(directory: String) = WalletApp.app.prefs.edit.putString(WalletApp.CUSTOM_BACKUP_LOCATION, directory)
      WalletApp.customBackupLocation match {
        case Some(uri) => {
            getContentResolver.releasePersistableUriPermission(uri, Intent.FLAG_GRANT_WRITE_URI_PERMISSION)
        }
        case _ => ()
      }
      val uri = resultData.getData
      getContentResolver.takePersistableUriPermission(uri, Intent.FLAG_GRANT_WRITE_URI_PERMISSION)
      saveDirectory(uri.toString).commit
      backupLocation.updateView
      WalletApp.backupSaveWorker.replaceWork(true)
    }

  lazy private[this] val chainWallets: SettingsHolder = new SettingsHolder(me) {
    setVisMany(false -> settingsCheck, false -> settingsInfo)
    settingsTitle.setText(settings_chain_wallets)
    override def updateView: Unit = none

    private val wallets = Map(
      BIP32 -> ("BRD, legacy wallet", "m/0'/0/n"),
      BIP44 -> ("Bitcoin.com, Mycelium, Exodus...", "m/44'/0'/0'/0/n"),
      BIP49 -> ("JoinMarket, Eclair Mobile, Pine...", "m/49'/0'/0'/0/n"),
      BIP84 -> (getString(settings_chain_modern), "m/84'/0'/0'/0/n")
    )

    val possibleKeys: List[String] = wallets.keys.toList
    val hardcodedPosition: Int = possibleKeys.indexOf(BIP84)

    view setOnClickListener onButtonTap {
      val options = for (Tuple2(tag, info ~ path) <- wallets) yield s"<b>$tag</b> <i>$path</i><br>$info".html
      val adapter = new ArrayAdapter(me, android.R.layout.select_dialog_multichoice, options.toArray) {
        override def isEnabled(itemPosition: Int): Boolean = itemPosition != hardcodedPosition

        override def getView(itemPosition: Int, itemConvertedView: View, itemParentGroup: ViewGroup): View = {
          val bgColor = if (itemPosition == hardcodedPosition) R.color.almostBlack else android.R.color.transparent
          val finalView = super.getView(itemPosition, itemConvertedView, itemParentGroup)
          finalView.setBackgroundResource(bgColor)
          finalView
        }
      }

      val list = selectorList(adapter)
      val listener = new OnListItemClickListener {
        def onItemClicked(itemPosition: Int): Unit = {
          val core = SigningWallet(possibleKeys(itemPosition), isRemovable = true)

          if (list isItemChecked itemPosition) {
            val wallet = LNParams.chainWallets.makeSigningWalletParts(core, Satoshi(0L), label = core.walletType)
            HubActivity.instance.walletCards.resetChainCards(LNParams.chainWallets withFreshWallet wallet)
          } else {
            val affectedWallet = LNParams.chainWallets.wallets.find(wallet => wallet.isSigning && wallet.info.core.walletType == core.walletType)
            affectedWallet.map(LNParams.chainWallets.withoutWallet).foreach(HubActivity.instance.walletCards.resetChainCards)
          }
        }
      }

      list.setOnItemClickListener(listener)
      list.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE)
      new BaseChoiceBottomSheet(list).show(getSupportFragmentManager, "unused-wallet-type-tag")
      for (wallet <- LNParams.chainWallets.wallets) list.setItemChecked(possibleKeys.indexOf(wallet.info.core.walletType), true)
    }
  }

  lazy private[this] val addHardware: SettingsHolder = new SettingsHolder(me) {
    setVisMany(false -> settingsCheck, false -> settingsInfo)
    view setOnClickListener onButtonTap(callUrScanner)
    settingsTitle.setText(settings_hardware_add)
    override def updateView: Unit = none
    disableIfOldAndroid

    def callUrScanner: Unit = {
      def onKey(data: PairingData): Unit = {
        val (container, extraInputLayout, extraInput) = singleInputPopup
        val builder = titleBodyAsViewBuilder(getString(settings_hardware_label).asDefView, container)
        mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, builder, dialog_ok, dialog_cancel)
        extraInputLayout.setHint(dialog_set_label)
        showKeys(extraInput)

        def proceed: Unit = runAnd(finish) {
          if (LNParams.chainWallets.findByPubKey(data.bip84XPub.publicKey).isEmpty) {
            val core = WatchingWallet(EclairWallet.BIP84, data.masterFingerprint, data.bip84XPub, isRemovable = true)
            val label = extraInput.getText.toString.trim.asSome.filter(_.nonEmpty).getOrElse(EclairWallet.BIP84)
            val wallet = LNParams.chainWallets.makeWatchingWallet84Parts(core, lastBalance = Satoshi(0L), label)
            HubActivity.instance.walletCards.resetChainCards(LNParams.chainWallets withFreshWallet wallet)
          }
        }
      }

      val sheet = new sheets.URBottomSheet(me, onKey)
      callScanner(sheet)
    }
  }

  lazy private[this] val electrum: SettingsHolder = new SettingsHolder(me) {
    setVis(isVisible = false, settingsCheck)

    override def updateView: Unit = WalletApp.customElectrumAddress match {
      case Success(nodeAddress) => setTexts(settings_custom_electrum_enabled, nodeAddress.toString)
      case _ => setTexts(settings_custom_electrum_disabled, me getString settings_custom_electrum_disabled_tip)
    }

    view setOnClickListener onButtonTap {
      val (container, extraInputLayout, extraInput) = singleInputPopup
      val builder = titleBodyAsViewBuilder(getString(settings_custom_electrum_disabled).asDefView, container)
      mkCheckForm(alert => runAnd(alert.dismiss)(proceed), none, builder, dialog_ok, dialog_cancel)
      extraInputLayout.setHint(settings_custom_electrum_host_port)
      showKeys(extraInput)

      def proceed: Unit = {
        val input = extraInput.getText.toString.trim
        def saveAddress(address: String) = WalletApp.app.prefs.edit.putString(WalletApp.CUSTOM_ELECTRUM_ADDRESS, address)
        if (input.nonEmpty) runInFutureProcessOnUI(saveUnsafeElectrumAddress, onFail)(_ => warnAndUpdateView)
        else runAnd(saveAddress(new String).commit)(warnAndUpdateView)

        def saveUnsafeElectrumAddress: Unit = {
          val hostOrIP ~ port = input.splitAt(input lastIndexOf ':')
          val nodeAddress = NodeAddress.fromParts(hostOrIP, port.tail.toInt, Domain)
          saveAddress(nodeaddress.encode(nodeAddress).require.toHex).commit
        }

        def warnAndUpdateView: Unit = {
          def onOk(snack: Snackbar): Unit = runAnd(snack.dismiss)(WalletApp.restart)
          val message = getString(settings_custom_electrum_restart_notice).html
          snack(settingsContainer, message, R.string.dialog_ok, onOk)
          updateView
        }
      }
    }

    def setTexts(titleRes: Int, info: String): Unit = {
      settingsTitle.setText(titleRes)
      settingsInfo.setText(info)
    }
  }

  lazy private[this] val setFiat = new SettingsHolder(me) {
    settingsTitle.setText(settings_fiat_currency)
    setVis(isVisible = false, settingsCheck)

    override def updateView: Unit = settingsInfo.setText(WalletApp.fiatCode.toUpperCase)

    view setOnClickListener onButtonTap {
      val options = fiatSymbols.map { case code ~ name => code.toUpperCase + SEPARATOR + name }
      val list = me selectorList new ArrayAdapter(me, android.R.layout.simple_expandable_list_item_1, options.toArray)
      new sheets.ChoiceBottomSheet(list, CHOICE_FIAT_DENOMINATION_TAG, me).show(getSupportFragmentManager, "unused-tag")
    }
  }

  lazy private[this] val setBtc = new SettingsHolder(me) {
    settingsTitle.setText(settings_btc_unit)
    setVis(isVisible = false, settingsCheck)

    view setOnClickListener onButtonTap {
      val options = for (unit <- units) yield unit.parsedWithSign(MilliSatoshi(526800020L), cardIn, cardZero).html
      val list = me selectorList new ArrayAdapter(me, android.R.layout.simple_expandable_list_item_1, options.toArray)
      new sheets.ChoiceBottomSheet(list, CHOICE_BTC_DENOMINATON_TAG, me).show(getSupportFragmentManager, "unused-tag")
    }

    override def updateView: Unit = {
      val short = WalletApp.denom.sign.toUpperCase
      val isSatDenom = WalletApp.denom == SatDenomination
      val text = if (isSatDenom) s"Satoshi ($short)" else s"Bitcoin ($short)"
      settingsInfo.setText(text)
    }
  }

  lazy private[this] val useBiometric: SettingsHolder = new SettingsHolder(me) {
    def updateView: Unit = settingsCheck.setChecked(WalletApp.useAuth)

    view setOnClickListener onButtonTap {
      if (WalletApp.useAuth) runAnd(AppLock.getInstance(me).invalidateEnrollments)(updateView)
      else startActivityForResult(new Intent(me, ClassNames.lockCreationClass), REQUEST_CODE_CREATE_LOCK)
    }

    settingsTitle.setText(settings_use_auth)
    setVis(isVisible = false, settingsInfo)
  }

  lazy private[this] val enforceTor = new SettingsHolder(me) {
    override def updateView: Unit = settingsCheck.setChecked(WalletApp.ensureTor)

    settingsTitle.setText(settings_ensure_tor)
    setVis(isVisible = false, settingsInfo)
    disableIfOldAndroid

    view setOnClickListener onButtonTap {
      putBoolAndUpdateView(WalletApp.ENSURE_TOR, !WalletApp.ensureTor)
      def onOk(snack: Snackbar): Unit = runAnd(snack.dismiss)(WalletApp.restart)
      snack(settingsContainer, getString(settings_custom_electrum_restart_notice).html, R.string.dialog_ok, onOk)
    }
  }

  lazy private[this] val viewCode = new SettingsHolder(me) {
    setVisMany(false -> settingsCheck, false -> settingsInfo)
    view setOnClickListener onButtonTap(viewRecoveryCode)
    settingsTitle.setText(settings_view_revocery_phrase)
    override def updateView: Unit = none
  }

  lazy private[this] val viewStat = new SettingsHolder(me) {
    setVisMany(false -> settingsCheck, false -> settingsInfo)
    view setOnClickListener onButtonTap(me goTo ClassNames.statActivityClass)
    settingsTitle.setText(settings_stats)
    override def updateView: Unit = none
  }

  override def PROCEED(state: Bundle): Unit = {
    setContentView(R.layout.activity_settings)

    val settingsPageitle = new TitleView(s"v$VERSION_NAME-$VERSION_CODE")
    settingsPageitle.view.setOnClickListener(me onButtonTap finish)
    settingsPageitle.backArrow.setVisibility(View.VISIBLE)

      val links = new TitleView("&#9996;")
      addFlowChip(links.flow, getString(manual), R.drawable.border_green, _ => me browse "https://valet.finance/posts/How_To_Use_Valet.md")
      addFlowChip(links.flow, getString(sources), R.drawable.border_green, _ => me browse "https://github.com/standardsats/wallet")
      addFlowChip(links.flow, getString(twitter), R.drawable.border_blue, _ => me browse "https://twitter.com/standard_sats")
      addFlowChip(links.flow, "&#9825; Rate us", R.drawable.border_green, _ => me bringRateDialog null)

    for (count <- LNParams.logBag.count if count > 0) {
      def exportLog: Unit = me share LNParams.logBag.recent.map(_.asString).mkString("\n\n")
      val errorCount = s"${me getString error_log} <font color=$cardZero>$count</font>"
      addFlowChip(links.flow, errorCount, R.drawable.border_yellow, _ => exportLog)
      addFlowChip(links.flow, me getString error_log_clear, R.drawable.border_yellow, _ => LNParams.logBag.clear())
    }

    settingsContainer.addView(settingsPageitle.view)
    settingsContainer.addView(storeLocalBackup.view)
    settingsContainer.addView(backupLocation.view)
    settingsContainer.addView(chainWallets.view)
    settingsContainer.addView(addHardware.view)
    settingsContainer.addView(electrum.view)
    settingsContainer.addView(setFiat.view)
    settingsContainer.addView(setBtc.view)

    settingsContainer.addView(useBiometric.view)
    settingsContainer.addView(enforceTor.view)
    settingsContainer.addView(viewCode.view)
    settingsContainer.addView(viewStat.view)
    settingsContainer.addView(links.view)
  }
}
