package com.googlecode.jwutils.jdtype;

import java.awt.Font;

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
            new JLabel(
                "<html><table border='0' cellspacing='0' cellpadding='3'>"
                    + "<tr><td style='font-size: 24px' align='center'><b>"
                    + "Welcome to JMLogo.</b></td></tr>"
                    + "<tr><td align='center'>More to come soon!"
                    + " -- javawizard2539</td></tr><br/>&nbsp;<br/>&nbsp;<br/>&nbsp;<br/>&nbsp;");
        label.setFont(Font.decode(null));
        label.setHorizontalAlignment(JLabel.CENTER);
        frame.getContentPane().add(label);
        frame.show();
    }
    
}
