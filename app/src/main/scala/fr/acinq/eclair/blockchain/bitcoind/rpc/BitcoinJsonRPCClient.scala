package fr.acinq.eclair.blockchain.bitcoind.rpc

case class JsonRPCRequest(jsonrpc: String = "1.0", id: String = "scala-client", method: String, params: Seq[org.json4s.JsonAST.JValue] = Nil)
case class JsonRPCResponse(result: org.json4s.JsonAST.JValue, error: Option[Error], id: String)
case class Error(code: Int, message: String)