package immortan.utils

import java.text._

import fr.acinq.eclair._


object Denomination {
  val locale = new java.util.Locale("en", "US")
  val symbols = new DecimalFormatSymbols(locale)

  val formatFiatShort = new DecimalFormat("#,###,###")
  formatFiatShort setDecimalFormatSymbols symbols

  def btcBigDecimal2MSat(btc: BigDecimal): MilliSatoshi = (btc * BtcDenom.factor).toLong.msat
  def msat2BtcBigDecimal(msat: MilliSatoshi): BigDecimal = BigDecimal(msat.toLong) / BtcDenom.factor

  def fiat(incoming: String, outgoing: String, inColor: String, outColor: String, isIncoming: Boolean) = {
    val (color, amount) = if (isIncoming) (inColor, incoming) else (outColor, outgoing)
    val (whole, decimal) = amount.splitAt(amount indexOf ".")

    val (whole1, decimal1) = if (amount == decimal) (amount, new String) else (whole, decimal take 3)
    s"<font color=$color>${formatFiatShort format whole1.toDouble}<small>$decimal1</small></font>"
  }

  def fiatTT(incoming: String, outgoing: String, inColor: String, outColor: String, isIncoming: Boolean): String =
    "<tt>" + fiat(incoming, outgoing, inColor, outColor, isIncoming) + "</tt>"

  def fiatDirectedTT(incoming: String, outgoing: String, inColor: String, outColor: String, isIncoming: Boolean): String = {
    val base = fiatTT(incoming, outgoing, inColor, outColor, isIncoming)
    val direction = if (isIncoming) "+&#160;" else "-&#160;"
    s"$direction$base"
  }
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
  val sign: String
}

object BtcDenom extends Denomination { me =>
  val fmt: DecimalFormat = new DecimalFormat("##0.00000000")
  fmt.setDecimalFormatSymbols(Denomination.symbols)
  val factor = 100000000000L
  val sign = "btc"

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