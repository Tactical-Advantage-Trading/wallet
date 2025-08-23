package trading.tacticaladvantage

import android.os.Bundle

class TaActivity extends BaseActivity {
  override def START(state: Bundle): Unit =
    setContentView(R.layout.activity_ta)
}
