package trading.tacticaladvantage

import android.app.Activity
import android.os.Bundle
import TaLink._
import immortan.{ClearnetConnectionProvider, ConnectionProvider}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

class MainActivity extends Activity {
    override def onCreate(savedInstanceState: Bundle): Unit = {
        super.onCreate(savedInstanceState)
        val cp: ConnectionProvider = new ClearnetConnectionProvider
        val biconomy = new Biconomy(cp, getFilesDir.getAbsolutePath)

        Future {
            println(biconomy.getSmartAccountAddress(Biconomy.AccountAddressRequest("0x1111111111111111111111111111111111111111111111111111111111111111")))
        }


        //("0x1111111111111111111111111111111111111111111111111111111111111111")
        //biconomy.startNodeWithArguments(Array("12"))
    }
}