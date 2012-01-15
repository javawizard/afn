package jw.docreader.ui;

import java.awt.BorderLayout;
import java.awt.Button;
import java.awt.Dialog;
import java.awt.Frame;
import java.awt.Panel;
import java.awt.TextArea;
import java.awt.TextField;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

public class OptionPane
{
    private static class StringContainer
    {
        public String value;
    }
    
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
    
    public static String showInputDialog(Frame parent, String message, String current)
    {
        final StringContainer result = new StringContainer();
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
        Panel p2 = new Panel();
        p2.setLayout(new BorderLayout());
        final TextField box = new TextField();
        p2.add(box, BorderLayout.NORTH);
        Button okButton = new Button("ok");
        p2.add(okButton);
        Button cancelButton = new Button("cancel");
        p2.add(cancelButton, BorderLayout.SOUTH);
        dialog.add(p2, BorderLayout.SOUTH);
        okButton.addActionListener(new ActionListener()
        {
            
            @Override
            public void actionPerformed(ActionEvent e)
            {
                result.value = box.getText();
                dialog.dispose();
            }
        });
        cancelButton.addActionListener(new ActionListener()
        {
            
            @Override
            public void actionPerformed(ActionEvent e)
            {
                dialog.dispose();
            }
        });
        dialog.show();
        return result.value;
    }
    
}
