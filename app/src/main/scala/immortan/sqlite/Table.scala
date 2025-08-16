package immortan.sqlite


trait Table {
  def createStatements: Seq[String]
  val Tuple2(id, fts) = Tuple2("_id", "fts4")
  val IDAUTOINC = s"$id INTEGER PRIMARY KEY AUTOINCREMENT"
  val UNIQUE = "UNIQUE"
}

object BtcWalletTable extends Table {
  val (table, info, xPub, data, lastBalance, label) = ("btcwallet", "info", "xpub", "data", "lastbalance", "label")

  val newSql = s"INSERT OR IGNORE INTO $table ($info, $xPub, $data, $lastBalance, $label) VALUES (?, ?, ?, ?, ?)"

  val updSql = s"UPDATE $table SET $data = ?, $lastBalance = ? WHERE $xPub = ?"

  val updLabelSql = s"UPDATE $table SET $label = ? WHERE $xPub = ?"

  val selectSql = s"SELECT * FROM $table ORDER BY $id ASC"

  val killSql = s"DELETE FROM $table WHERE $xPub = ?"

  def createStatements: Seq[String] =
    s"""CREATE TABLE IF NOT EXISTS $table(
      $IDAUTOINC, $info TEXT NOT NULL, $xPub TEXT NOT NULL $UNIQUE,
      $data BLOB NOT NULL, $lastBalance INTEGER NOT NULL, $label TEXT NOT NULL
    )""" :: Nil
}

object BtcTxTable extends Table {
  val (search, table, rawTx, txid, pub, depth, receivedSat, sentSat, feeSat, seenAt, updatedAt, description, balanceMsat, fiatRates, incoming, doubleSpent) =
    ("tsearch", "btc", "raw", "txid", "pub", "depth", "received", "sent", "fee", "seen", "updated", "desc", "balance", "fiatrates", "incoming", "doublespent")

  private val inserts = s"$rawTx, $txid, $pub, $depth, $receivedSat, $sentSat, $feeSat, $seenAt, $updatedAt, $description, $balanceMsat, $fiatRates, $incoming, $doubleSpent"
  val newSql = s"INSERT OR REPLACE INTO $table ($inserts) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

  val newVirtualSql = s"INSERT INTO $fts$table ($search, $txid) VALUES (?, ?)"

  // Selecting

  val selectRecentSql = s"SELECT * FROM $table ORDER BY $id DESC LIMIT ?"

  val searchSql = s"SELECT * FROM $table WHERE $txid IN (SELECT DISTINCT $txid FROM $fts$table WHERE $search MATCH ? LIMIT 10)"

  // Updating

  val updStatusSql = s"UPDATE $table SET $depth = ?, $doubleSpent = ?, $updatedAt = ? WHERE $txid = ?"

  val updateDescriptionSql = s"UPDATE $table SET $description = ? WHERE $txid = ?"

  def createStatements: Seq[String] = {
    val createTable = s"""CREATE TABLE IF NOT EXISTS $table(
      $IDAUTOINC, $rawTx TEXT NOT NULL, $txid TEXT NOT NULL $UNIQUE, $pub TEXT NOT NULL, $depth INTEGER NOT NULL, $receivedSat INTEGER NOT NULL,
      $sentSat INTEGER NOT NULL, $feeSat INTEGER NOT NULL, $seenAt INTEGER NOT NULL, $updatedAt INTEGER NOT NULL, $description TEXT NOT NULL,
      $balanceMsat INTEGER NOT NULL, $fiatRates TEXT NOT NULL, $incoming INTEGER NOT NULL, $doubleSpent INTEGER NOT NULL
    )"""

    val addSearchTable = s"CREATE VIRTUAL TABLE IF NOT EXISTS $fts$table USING $fts($search, $txid)"
    val addIndex1 = s"CREATE INDEX IF NOT EXISTS idx1$table ON $table ($pub)"
    createTable :: addIndex1 :: addSearchTable :: Nil
  }
}

object UsdtWalletTable extends Table {
  val (table, address, xPriv, lastBalance, lastNonce, lastTip, label) = ("usdtwallet", "address", "xpriv", "lastbalance", "lastnonce", "lasttip", "label")

  val newUpdSql = s"INSERT OR REPLACE INTO $table ($address, $xPriv, $lastBalance, $lastNonce, $lastTip, $label) VALUES (?, ?, ?, ?, ?, ?)"

  val selectSql = s"SELECT * FROM $table ORDER BY $id ASC"

  val killSql = s"DELETE FROM $table WHERE $xPriv = ?"

  def createStatements: Seq[String] =
    s"""CREATE TABLE IF NOT EXISTS $table(
      $IDAUTOINC, $address TEXT NOT NULL, $xPriv TEXT NOT NULL $UNIQUE,
      $lastBalance TEXT NOT NULL, $lastNonce TEXT NOT NULL,
      $lastTip INTEGER NOT NULL, $label TEXT NOT NULL
    )""" :: Nil
}

object UsdtTxTable extends Table {
  val (search, table, hash, network, block, receivedUsdt, sentUsdt, feeUsdt, seenAt, updatedAt, description, balanceUsdt, incoming, doubleSpent) =
    ("tsearch", "usdt", "hash", "network", "block", "received", "sent", "fee", "seen", "updated", "desc", "balance", "incoming", "doublespent")

  private val inserts = s"$hash, $network, $block, $receivedUsdt, $sentUsdt, $feeUsdt, $seenAt, $updatedAt, $description, $balanceUsdt, $incoming, $doubleSpent"
  val newSql = s"INSERT OR REPLACE INTO $table ($inserts) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

  val newVirtualSql = s"INSERT INTO $fts$table ($search, $hash) VALUES (?, ?)"

  // Selecting

  val selectRecentSql = s"SELECT * FROM $table ORDER BY $id DESC LIMIT ?"

  val searchSql = s"SELECT * FROM $table WHERE $hash IN (SELECT DISTINCT $hash FROM $fts$table WHERE $search MATCH ? LIMIT 10)"

  // Updating

  val updateDescriptionSql = s"UPDATE $table SET $description = ? WHERE $hash = ?"

  def createStatements: Seq[String] = {
    val createTable = s"""CREATE TABLE IF NOT EXISTS $table(
      $IDAUTOINC, $hash TEXT NOT NULL $UNIQUE, $network INTEGER NOT NULL, $block INTEGER NOT NULL, $receivedUsdt TEXT NOT NULL,
      $sentUsdt TEXT NOT NULL, $feeUsdt TEXT NOT NULL, $seenAt INTEGER NOT NULL, $updatedAt INTEGER NOT NULL, $description TEXT NOT NULL,
      $balanceUsdt TEXT NOT NULL, $incoming INTEGER NOT NULL, $doubleSpent INTEGER NOT NULL
    )"""

    val addSearchTable = s"CREATE VIRTUAL TABLE IF NOT EXISTS $fts$table USING $fts($search, $hash)"
    createTable :: addSearchTable :: Nil
  }
}

object DataTable extends Table {
  val (table, label, content) = ("data", "label", "content")

  val newSql = s"INSERT OR IGNORE INTO $table ($label, $content) VALUES (?, ?)"

  val updSql = s"UPDATE $table SET $content = ? WHERE $label = ?"

  val selectSql = s"SELECT * FROM $table WHERE $label = ?"

  val killSql = s"DELETE FROM $table WHERE $label = ?"

  def createStatements: Seq[String] = s"CREATE TABLE IF NOT EXISTS $table($IDAUTOINC, $label TEXT NOT NULL $UNIQUE, $content BLOB NOT NULL)" :: Nil
}

object ElectrumHeadersTable extends Table {
  val (table, height, blockHash, header) = ("headers", "height", "blockhash", "header")

  val addHeaderSql = s"INSERT OR IGNORE INTO $table ($height, $blockHash, $header) VALUES (?, ?, ?)"

  val selectByHeightSql = s"SELECT * FROM $table WHERE $height = ?"

  val selectByBlockHashSql = s"SELECT * FROM $table WHERE $blockHash = ?"

  val selectHeadersSql = s"SELECT * FROM $table WHERE $height >= ? ORDER BY $height LIMIT ?"

  val selectTipSql = s"SELECT * FROM $table INNER JOIN (SELECT MAX($height) AS maxHeight FROM $table) t1 ON $height = t1.maxHeight"

  def createStatements: Seq[String] = s"CREATE TABLE IF NOT EXISTS $table($IDAUTOINC, $height INTEGER NOT NULL $UNIQUE, $blockHash TEXT NOT NULL, $header BLOB NOT NULL)" :: Nil
}
