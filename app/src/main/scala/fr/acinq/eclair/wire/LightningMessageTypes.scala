package fr.acinq.eclair.wire

import java.net.{Inet4Address, Inet6Address, InetAddress, InetSocketAddress}

sealed trait NodeAddress { def socketAddress: InetSocketAddress }

sealed trait OnionAddress extends NodeAddress

object NodeAddress {
  val onionSuffix = ".onion"
  val V2Len = 16
  val V3Len = 56

  def fromParts(host: String, port: Int, orElse: (String, Int) => NodeAddress = resolveIp): NodeAddress =
    if (host.endsWith(onionSuffix) && host.length == V2Len + onionSuffix.length) Tor2(host.dropRight(onionSuffix.length), port)
    else if (host.endsWith(onionSuffix) && host.length == V3Len + onionSuffix.length) Tor3(host.dropRight(onionSuffix.length), port)
    else orElse(host, port)

  def resolveIp(host: String, port: Int): NodeAddress =
    InetAddress getByName host match {
      case inetV4Address: Inet4Address => IPv4(inetV4Address, port)
      case inetV6Address: Inet6Address => IPv6(inetV6Address, port)
    }

  def unresolved(port: Int, host: Int*): NodeAddress =
    InetAddress getByAddress host.toArray.map(_.toByte) match {
      case inetV4Address: Inet4Address => IPv4(inetV4Address, port)
      case inetV6Address: Inet6Address => IPv6(inetV6Address, port)
    }
}

case class IPv4(ipv4: Inet4Address, port: Int) extends NodeAddress {
  override def socketAddress: InetSocketAddress = new InetSocketAddress(ipv4, port)
  override def toString: String = s"${ipv4.toString.tail}:$port"
}

case class IPv6(ipv6: Inet6Address, port: Int) extends NodeAddress {
  override def socketAddress: InetSocketAddress = new InetSocketAddress(ipv6, port)
  override def toString: String = s"${ipv6.toString.tail}:$port"
}

case class Tor2(tor2: String, port: Int) extends OnionAddress {
  override def socketAddress: InetSocketAddress = new InetSocketAddress(tor2 + NodeAddress.onionSuffix, port)
  override def toString: String = s"[ONION] $tor2:$port"
}

case class Tor3(tor3: String, port: Int) extends OnionAddress {
  override def socketAddress: InetSocketAddress = new InetSocketAddress(tor3 + NodeAddress.onionSuffix, port)
  override def toString: String = s"[ONION] $tor3:$port"
}

case class Domain(domain: String, port: Int) extends NodeAddress {
  override def socketAddress: InetSocketAddress = new InetSocketAddress(domain, port)
  override def toString: String = s"$domain:$port"
}