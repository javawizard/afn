package com.googlecode.jwutils.jdtype;

import javax.swing.JFrame;
import javax.swing.JLabel;

public class JDType
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        JFrame frame = new JFrame("JDType");
        frame.setSize(600, 470);
        frame.setLocationRelativeTo(null);
        JLabel label =
            new JLabel("<html><div style='align: center; font-size: 24px'>"
                + "Welcome to JDType.</div><br/>More to come soon!"
                + " -- javawizard2539");
        label.setHorizontalAlignment(JLabel.CENTER);
        frame.getContentPane().add(label);
        frame.show();
    }
    
}
