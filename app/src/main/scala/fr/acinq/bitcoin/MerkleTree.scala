package fr.acinq.bitcoin

/**
  * see https://en.bitcoin.it/wiki/Protocol_specification#Merkle_Trees
  */
object MerkleTree {
  def computeRoot(tree: Seq[ByteVector32]): ByteVector32 = tree.length match {
    case 1 => tree(0)
    case n if n % 2 != 0 => computeRoot(tree :+ tree.last) // append last element again
    case _ => computeRoot(tree.grouped(2).map(a => Crypto.hash256(a(0) ++ a(1))).toSeq)
  }
}
