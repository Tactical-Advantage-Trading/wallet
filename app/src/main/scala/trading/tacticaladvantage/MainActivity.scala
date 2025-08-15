package trading.tacticaladvantage

import android.app.Activity
import android.os.Bundle
import immortan.{ClearnetConnectionProvider, ConnectionProvider}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class MainActivity extends Activity {
  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)
    val cp: ConnectionProvider = new ClearnetConnectionProvider
    val biconomy = new Biconomy(cp, getFilesDir.getAbsolutePath)

    Future {
      println(biconomy.getSmartAccountAddress("0x1111111111111111111111111111111111111111111111111111111111111111"))
    }

  }
}