package immortan.utils

import java.text._

import fr.acinq.eclair._


object Denomination {
  val locale = new java.util.Locale("en", "US")
  val symbols = new DecimalFormatSymbols(locale)
  val formatFiat = new DecimalFormat("#,###,###.##")
  val formatFiatShort = new DecimalFormat("#,###,###")

  formatFiat setDecimalFormatSymbols symbols
  formatFiatShort setDecimalFormatSymbols symbols

  def btcBigDecimal2MSat(btc: BigDecimal): MilliSatoshi = (btc * BtcDenom.factor).toLong.msat

  def msat2BtcBigDecimal(msat: MilliSatoshi): BigDecimal = BigDecimal(msat.toLong) / BtcDenom.factor
}

trait Denomination {
  protected def parsed(msat: MilliSatoshi, mainColor: String, zeroColor: String): String
  def parsedWithSignTT(msat: MilliSatoshi, mainColor: String, zeroColor: String): String
  def parsedWithSign(msat: MilliSatoshi, mainColor: String, zeroColor: String): String
  def fromMsat(amount: MilliSatoshi): BigDecimal = BigDecimal(amount.toLong) / factor

  def directedWithSign(incoming: MilliSatoshi, outgoing: MilliSatoshi,
                       inColor: String, outColor: String, zeroColor: String,
                       isIncoming: Boolean): String = {

    if (isIncoming && incoming == 0L.msat) parsedWithSignTT(incoming, inColor, zeroColor)
    else if (isIncoming) "+&#160;" + parsedWithSignTT(incoming, inColor, zeroColor)
    else if (outgoing == 0L.msat) parsedWithSignTT(outgoing, outColor, zeroColor)
    else "-&#160;" + parsedWithSignTT(outgoing, outColor, zeroColor)
  }

  val fmt: DecimalFormat
  val factor: Long
  val sign: String
}

object SatDenom extends Denomination { me =>
  val fmt: DecimalFormat = new DecimalFormat("###,###,###")
  fmt.setDecimalFormatSymbols(Denomination.symbols)
  val factor = 1000L
  val sign = "sat"

  def parsedWithSignTT(msat: MilliSatoshi, mainColor: String, zeroColor: String): String =
    if (0L == msat.toLong) "<tt>0</tt>" else "<tt>" + parsed(msat, mainColor, zeroColor) + "</tt>\u00A0" + sign

  def parsedWithSign(msat: MilliSatoshi, mainColor: String, zeroColor: String): String =
    if (0L == msat.toLong) "0" else parsed(msat, mainColor, zeroColor) + "\u00A0" + sign

  protected def parsed(msat: MilliSatoshi, mainColor: String, zeroColor: String): String =
    s"<font color=$mainColor>" + fmt.format(me fromMsat msat) + "</font>"
}

object BtcDenom extends Denomination { me =>
  val fmt: DecimalFormat = new DecimalFormat("##0.00000000")
  fmt.setDecimalFormatSymbols(Denomination.symbols)
  val factor = 100000000000L
  val sign = "btc"

  def parsedWithSignTT(msat: MilliSatoshi, mainColor: String, zeroColor: String): String =
    if (0L == msat.toLong) "<tt>0</tt>" else "<tt>" + parsed(msat, mainColor, zeroColor) + "</tt>"

  def parsedWithSign(msat: MilliSatoshi, mainColor: String, zeroColor: String): String =
    if (0L == msat.toLong) "0" else parsed(msat, mainColor, zeroColor)

  protected def parsed(msat: MilliSatoshi, mainColor: String, zeroColor: String): String = {
    // Alpha channel does not work on Android when set as HTML attribute
    // hence zero color is supplied to match different backgrounds well

    val basicFormatted = fmt.format(me fromMsat msat)
    val (whole, decimal) = basicFormatted.splitAt(basicFormatted indexOf ".")
    val bld = new StringBuilder(decimal).insert(3, ",").insert(7, ",").insert(0, whole)
    val splitIndex = bld.indexWhere(char => char != '0' && char != '.' && char != ',')
    val finalSplitIndex = if (".00000000" == decimal) splitIndex - 1 else splitIndex
    val (finalWhole, finalDecimal) = bld.splitAt(finalSplitIndex)

    new StringBuilder("<font color=").append(zeroColor).append('>').append(finalWhole).append("</font>")
      .append("<font color=").append(mainColor).append('>').append(finalDecimal).append("</font>")
      .toString
  }
}