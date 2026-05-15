package bitassets.lite

final class BitassetsLite {
  @native def add(a: Int, b: Int): Int
}

object BitassetsLite {
  System.loadLibrary("bitassets_lite_client_lib")
  val native = new BitassetsLite
}
