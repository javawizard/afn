package com.googlecode.jwutils.timer;

import info.clearthought.layout.TableLayout;

import java.awt.Dimension;
import javax.swing.JButton;

import javax.swing.WindowConstants;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JToggleButton;

/**
 * This code was edited or generated using CloudGarden's Jigloo SWT/Swing GUI
 * Builder, which is free for non-commercial use. If Jigloo is being used
 * commercially (ie, by a corporation, company or business for any purpose
 * whatever) then you should purchase a license for each developer using Jigloo.
 * Please visit www.cloudgarden.com for details. Use of Jigloo implies
 * acceptance of these licensing terms. A COMMERCIAL LICENSE HAS NOT BEEN
 * PURCHASED FOR THIS MACHINE, SO JIGLOO OR THIS CODE CANNOT BE USED LEGALLY FOR
 * ANY CORPORATE OR COMMERCIAL PURPOSE.
 */
public class TimerComponent extends javax.swing.JPanel
{
    private JToggleButton mainButton;
    private JButton cancelButton;
    private JLabel nameLabel;
    
    /**
     * Auto-generated main method to display this JPanel inside a new JFrame.
     */
    public static void main(String[] args)
    {
        JFrame frame = new JFrame();
        frame.getContentPane().add(new TimerComponent());
        frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
    }
    
    public TimerComponent()
    {
        super();
        initGUI();
    }
    
    private void initGUI()
    {
        try
        {
            TableLayout thisLayout =
                new TableLayout(new double[][] {
                    { TableLayout.FILL, TableLayout.PREFERRED, 3.0,
                        TableLayout.PREFERRED, TableLayout.FILL },
                    { TableLayout.FILL, TableLayout.PREFERRED, 3.0,
                        TableLayout.PREFERRED, TableLayout.FILL } });
            thisLayout.setHGap(5);
            thisLayout.setVGap(5);
            this.setLayout(thisLayout);
            setPreferredSize(new Dimension(400, 300));
            {
                mainButton = new JToggleButton();
                this.add(mainButton, "1, 3");
                mainButton.setText("Loading...");
                mainButton
                    .setToolTipText("Click this button to pause or unpause this timer.");
                mainButton.setSelected(true);
            }
            {
                cancelButton = new JButton();
                this.add(getCancelButton(), "3, 3");
                cancelButton.setText("X");
                cancelButton.setMargin(new java.awt.Insets(0, 0, 0, 0));
                cancelButton
                    .setToolTipText("Click this button to cancel the timer. You will not be asked to confirm this.");
            }
            {
                nameLabel = new JLabel();
                this.add(getNameLabel(), "1,1,3,1");
                nameLabel.setText("Loading...");
            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }
    
    public JButton getCancelButton()
    {
        return cancelButton;
    }
    
    public JLabel getNameLabel()
    {
        return nameLabel;
    }
    
    public JToggleButton getMainButton()
    {
        return mainButton;
    }
    
}
