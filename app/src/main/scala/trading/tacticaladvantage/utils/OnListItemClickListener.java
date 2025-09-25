package trading.tacticaladvantage.utils;

import android.view.View;
import android.widget.AdapterView;

public abstract class OnListItemClickListener implements AdapterView.OnItemClickListener {
    @Override public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        onItemClicked(position);
    }

    public abstract void onItemClicked(int position);
}
