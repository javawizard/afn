package jw.zelden;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.TextView;

public class ZeldenFirst extends Activity
{
    
    @Override
    protected void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        TextView text = new TextView(this);
        text.setText("This is the first sub-view.");
        LinearLayout layout = new LinearLayout(this);
        layout.setOrientation(layout.VERTICAL);
        Button button = new Button(this);
        button.setText("Go to second sub-view");
        button.setOnClickListener(new OnClickListener()
        {
            
            @Override
            public void onClick(View arg0)
            {
                ZeldenFirst.this.startActivity(new Intent("jw.zelden.intent.ZELDENSECOND"));
            }
        });
        layout.addView(text);
        layout.addView(button);
        setContentView(layout);
    }
}
