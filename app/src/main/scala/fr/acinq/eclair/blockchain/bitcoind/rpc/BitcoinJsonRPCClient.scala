package fr.acinq.eclair.blockchain.bitcoind.rpc

case class JsonRPCRequest(id: String, method: String, params: Seq[org.json4s.JsonAST.JValue], jsonrpc: String = "1.0")
case class JsonRPCResponse(result: org.json4s.JsonAST.JValue, error: Option[Error], id: String)
case class Error(code: Int, message: String)