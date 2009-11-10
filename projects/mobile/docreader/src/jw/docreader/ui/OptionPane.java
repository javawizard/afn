package jw.docreader.ui;

import java.awt.BorderLayout;
import java.awt.Button;
import java.awt.Dialog;
import java.awt.Frame;
import java.awt.TextArea;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

public class OptionPane
{
    public static void showMessageDialog(Frame parent, String message)
    {
        final Dialog dialog = new Dialog(parent, "DocReader Message", true);
        dialog.setSize(240, 320);
        dialog.addWindowListener(new WindowAdapter()
        {
            
            @Override
            public void windowClosing(WindowEvent e)
            {
                dialog.dispose();
            }
        });
        dialog.setLayout(new BorderLayout());
        TextArea area = new TextArea(message, 5, 20, TextArea.SCROLLBARS_VERTICAL_ONLY);
        area.setEditable(false);
        dialog.add(area);
        Button button = new Button("Close");
        dialog.add(button, BorderLayout.SOUTH);
        button.addActionListener(new ActionListener()
        {
            
            @Override
            public void actionPerformed(ActionEvent e)
            {
                dialog.dispose();
            }
        });
        dialog.show();
    }
}
