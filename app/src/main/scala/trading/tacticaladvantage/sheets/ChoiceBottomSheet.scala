package trading.tacticaladvantage.sheets

import android.os.Bundle
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget.ListView
import trading.tacticaladvantage.utils.OnListItemClickListener
import com.google.android.material.bottomsheet.BottomSheetDialogFragment
import trading.tacticaladvantage.{ChoiceReceiver, R}


class ChoiceBottomSheet(list: ListView, tag: AnyRef, host: ChoiceReceiver) extends BottomSheetDialogFragment {
  override def onCreateView(inflater: LayoutInflater, container: ViewGroup, state: Bundle): View = list

  override def onViewCreated(view: View, state: Bundle): Unit = {
    view.setBackgroundResource(R.color.chip_default_text_color)

    list setOnItemClickListener new OnListItemClickListener {
      def onItemClicked(itemPosition: Int): Unit = {
        host.onChoiceMade(tag, itemPosition)
        dismiss
      }
    }
  }
}
