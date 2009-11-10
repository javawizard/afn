package jw.docreader;

import java.awt.Button;
import java.awt.Panel;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;

public class ButtonPanel extends Panel
{
    public ButtonPanel(String[] names, String[] titles, boolean[] enabled,
            final ButtonListener listener)
    {
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        for (int i = 0; i < names.length; i++)
        {
            Button b = new Button(titles[i]);
            b.setEnabled(enabled[i]);
            final String fName = names[i];
            super.add(b);
            b.addActionListener(new ActionListener()
            {
                
                @Override
                public void actionPerformed(ActionEvent e)
                {
                    listener.action(fName);
                }
            });
        }
    }
}
