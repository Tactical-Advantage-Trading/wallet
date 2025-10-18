package trading.tacticaladvantage.sheets

import android.os.Bundle
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget.ListView
import com.google.android.material.bottomsheet.BottomSheetDialogFragment
import trading.tacticaladvantage.R
import trading.tacticaladvantage.utils.OnListItemClickListener

class ChoiceBottomSheet(list: ListView, onChoice: Int => Unit) extends BottomSheetDialogFragment {
  override def onCreateView(inflater: LayoutInflater, container: ViewGroup, state: Bundle): View = list

  override def onViewCreated(view: View, state: Bundle): Unit = {
    view.setBackgroundResource(R.color.chip_default_text_color)
    list setOnItemClickListener new OnListItemClickListener {
      def onItemClicked(itemPosition: Int): Unit = {
        onChoice(itemPosition)
        dismiss
      }
    }
  }
}
