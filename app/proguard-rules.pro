-dontwarn org.apache.log4j.Level
-dontwarn org.apache.log4j.Logger
-dontwarn org.apache.log4j.Priority
-dontwarn org.apache.logging.log4j.Level
-dontwarn org.apache.logging.log4j.LogManager
-dontwarn org.apache.logging.log4j.Logger
-dontwarn org.apache.logging.log4j.message.MessageFactory
-dontwarn org.apache.logging.log4j.spi.ExtendedLogger
-dontwarn org.apache.logging.log4j.spi.ExtendedLoggerWrapper

-keepattributes SourceFile,LineNumberTable

-keep class akka.** { *; }
-keep class fr.acinq.secp256k1.** { *; }
-keep class trading.tacticaladvantage.R$* { <fields>; }
-keepclassmembers class * extends akka.actor.Actor { <init>(...); }
-keepclassmembers,allowobfuscation,allowshrinking class * { void *$_setter_$*(***); }

-keep class scala.concurrent.forkjoin.** { *; }
-keepclassmembers class * extends io.netty.channel.Channel { <init>(...); }
-keepclassmembers class scala.collection.concurrent.** { <fields>; }
-keepclassmembers class rx.internal.util.unsafe.** { <fields>; }
-keepclassmembers class io.netty.** { <fields>; }

-keep,allowobfuscation,allowshrinking class io.netty.handler.codec.MessageToMessageDecoder
-keep,allowobfuscation,allowshrinking class io.netty.handler.codec.MessageToMessageEncoder
-keep,allowobfuscation,allowshrinking class * extends io.netty.handler.codec.MessageToMessageDecoder
-keep,allowobfuscation,allowshrinking class * extends io.netty.handler.codec.MessageToMessageEncoder

-keepclassmembers class io.netty.buffer.AbstractByteBufAllocator {
    *** toLeakAwareBuffer(...);
}

-keepclassmembers class io.netty.buffer.AdvancedLeakAwareByteBuf {
    *** recordLeakNonRefCountingOperation(...);
    *** touch(...);
}

-keepclassmembers class io.netty.util.ReferenceCountUtil {
    *** touch(...);
}

-dontwarn io.netty.internal.tcnative.**
-dontwarn org.eclipse.jetty.alpn.**
-dontwarn org.eclipse.jetty.npn.**