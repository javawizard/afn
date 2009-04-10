package org.opengroove.sixjet.common.ui;

import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JSeparator;

public class HeaderComponent extends JComponent
{
    private JLabel label;
    private JSeparator separator;
    
    public HeaderComponent()
    {
        label = new JLabel("Header Text");
        separator = new JSeparator();
        setBorder(BorderFactory.createEmptyBorder(3, 6, 3, 6));
        setLayout(new BorderLayout());
        label.setMaximumSize(new Dimension(10000, 10000));
        label.setHorizontalAlignment(label.CENTER);
        add(label, BorderLayout.CENTER);
    }
    
    public String getLabel()
    {
        return label.getText();
    }
    
    public void setLabel(String text)
    {
        label.setText(text);
    }
}
