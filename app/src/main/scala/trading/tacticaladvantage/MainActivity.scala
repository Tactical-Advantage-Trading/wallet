package trading.tacticaladvantage

import android.app.Activity
import android.os.Bundle
import TaLink._
import immortan.{ClearnetConnectionProvider, ConnectionProvider}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

class MainActivity extends Activity {
    override def onCreate(savedInstanceState: Bundle): Unit = {
        super.onCreate(savedInstanceState)
        val cp: ConnectionProvider = new ClearnetConnectionProvider
        val biconomy = new Biconomy(cp)


//        biconomy.getSmartAccountAddress(Biconomy.AccountAddressRequest("0x1111111111111111111111111111111111111111111111111111111111111111"))

        //("0x1111111111111111111111111111111111111111111111111111111111111111")
        //biconomy.startNodeWithArguments(Array("12"))
    }
}