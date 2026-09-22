package trading.tacticaladvantage.utils

import fr.acinq.eclair._
import java.text._


object Denomination {
  val locale = java.util.Locale.US
  val symbols = new DecimalFormatSymbols(locale)
  val formatRoi = new DecimalFormat("#,##0.##%", symbols)
  val formatFiatShort = new DecimalFormat("#,###,###", symbols)
  def btcBigDecimal2MSat(btc: BigDecimal): MilliSatoshi = (btc * CoinDenom.factor).toLong.msat
  def msat2BtcBigDecimal(msat: MilliSatoshi): BigDecimal = BigDecimal(msat.toLong) / CoinDenom.factor
}

trait Denomination {
  def parsed(msat: MilliSatoshi, mainColor: String, zeroColor: String): String
  def parsedTT(msat: MilliSatoshi, mainColor: String, zeroColor: String): String
  def fromMsat(amount: MilliSatoshi): BigDecimal = BigDecimal(amount.toLong) / factor

  def directedTT(incoming: MilliSatoshi, outgoing: MilliSatoshi,
               inColor: String, outColor: String, zeroColor: String,
               isIncoming: Boolean): String = {

    if (isIncoming && incoming == 0L.msat) parsedTT(incoming, inColor, zeroColor)
    else if (isIncoming) "+&#160;" + parsedTT(incoming, inColor, zeroColor)
    else if (outgoing == 0L.msat) parsedTT(outgoing, outColor, zeroColor)
    else "-&#160;" + parsedTT(outgoing, outColor, zeroColor)
  }

  val fmt: DecimalFormat
  val factor: Long
}

object CoinDenom extends Denomination { me =>
  val fmt: DecimalFormat = new DecimalFormat("##0.00000000")
  fmt.setDecimalFormatSymbols(Denomination.symbols)
  val factor = 100000000000L

  def parsedTT(msat: MilliSatoshi, mainColor: String, zeroColor: String): String =
    if (0L == msat.toLong) "<tt>0</tt>" else "<tt>" + parsed(msat, mainColor, zeroColor) + "</tt>"

  def parsed(msat: MilliSatoshi, mainColor: String, zeroColor: String): String = {
    // Alpha channel does not work on Android when set as HTML attribute
    // hence zero color is supplied to match different backgrounds well

    val basicFormatted = fmt.format(me fromMsat msat)
    val (whole, decimal) = basicFormatted.splitAt(basicFormatted indexOf ".")
    val bld = new StringBuilder(decimal).insert(3, "'").insert(7, "'").insert(0, whole)
    val splitIndex = bld.indexWhere(char => char != '0' && char != '.' && char != ''')
    val finalSplitIndex = if (".00000000" == decimal) splitIndex - 1 else splitIndex
    val (finalWhole, finalDecimal) = bld.splitAt(finalSplitIndex)

    new StringBuilder("<font color=").append(zeroColor).append('>').append(finalWhole).append("</font>")
      .append("<font color=").append(mainColor).append('>').append(finalDecimal).append("</font>")
      .toString
  }
}

object TokenDenom extends Denomination {
  val fmt: DecimalFormat = new DecimalFormat("#,##0.00", Denomination.symbols)
  val factor = 100000000000L

  def parsedTT(msat: MilliSatoshi, mainColor: String, zeroColor: String): String =
    if (0L == msat.toLong) "<tt>0</tt>" else "<tt>" + parsed(msat, mainColor, zeroColor) + "</tt>"

  def parsed(msat: MilliSatoshi, mainColor: String, zeroColor: String): String = {
    val basicFormatted: String = fmt.format(fromMsat(amount = msat).bigDecimal)
    val (whole, dec) = basicFormatted.splitAt(basicFormatted indexOf ".")
    val color = if (0L == msat.toLong) zeroColor else mainColor
    s"<font color=$color>$whole<small>$dec</small></font>"
  }
}
