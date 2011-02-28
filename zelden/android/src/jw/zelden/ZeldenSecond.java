package jw.zelden;

import android.app.Activity;
import android.os.Bundle;
import android.widget.TextView;

public class ZeldenSecond extends Activity
{
    
    @Override
    protected void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        TextView text = new TextView(this);
        text.setText("Second sub-view here. The end of the line, as far as we go.");
        setContentView(text);
    }
    
}
