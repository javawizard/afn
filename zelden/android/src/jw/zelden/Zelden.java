package jw.zelden;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.TextView;

public class Zelden extends Activity
{
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        TextView text = new TextView(this);
        text.setText("Welcome to the toplevel view. This is Zelden, the future "
            + "distributed IRC client, written by Alexander Boyd. It's pretty "
            + "much just an Android dev sandbox until Alex figures out how "
            + "to write an Android app properly.");
        LinearLayout layout = new LinearLayout(this);
        layout.setOrientation(layout.VERTICAL);
        Button button = new Button(this);
        button.setText("Go to first sub-view");
        button.setOnClickListener(new OnClickListener()
        {
            
            @Override
            public void onClick(View arg0)
            {
                Zelden.this.startActivity(new Intent("jw.zelden.intent.ZELDENFIRST"));
            }
        });
        layout.addView(text);
        layout.addView(button);
        setContentView(layout);
    }
}