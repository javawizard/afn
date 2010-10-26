package afn.traytimer.ui;

import info.clearthought.layout.TableLayout;

import java.awt.Dimension;

import javax.swing.WindowConstants;
import javax.swing.plaf.basic.BasicArrowButton;
import javax.swing.JFrame;
import javax.swing.JTextField;

/**
 * This code was edited or generated using CloudGarden's Jigloo SWT/Swing GUI Builder,
 * which is free for non-commercial use. If Jigloo is being used commercially (ie, by a
 * corporation, company or business for any purpose whatever) then you should purchase a
 * license for each developer using Jigloo. Please visit www.cloudgarden.com for details.
 * Use of Jigloo implies acceptance of these licensing terms. A COMMERCIAL LICENSE HAS NOT
 * BEEN PURCHASED FOR THIS MACHINE, SO JIGLOO OR THIS CODE CANNOT BE USED LEGALLY FOR ANY
 * CORPORATE OR COMMERCIAL PURPOSE.
 */
public class MultiSpinner extends javax.swing.JPanel
{
    private JTextField value;
    private BasicArrowButton upButton;
    private BasicArrowButton fastDownButton;
    private BasicArrowButton fastUpButton;
    private BasicArrowButton downButton;
    
    /**
     * Auto-generated main method to display this JPanel inside a new JFrame.
     */
    public static void main(String[] args)
    {
        JFrame frame = new JFrame();
        frame.getContentPane().add(new MultiSpinner());
        frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
    }
    
    public MultiSpinner()
    {
        super();
        initGUI();
    }
    
    private void initGUI()
    {
        try
        {
            TableLayout thisLayout = new TableLayout(new double[][] {{TableLayout.PREFERRED, TableLayout.PREFERRED, TableLayout.PREFERRED}, {12.0, 12.0}});
            this.setLayout(thisLayout);
            {
                value = new JTextField();
                this.add(getValue(), "0,0,0,1");
                value.setText("00");
                value.setEditable(false);
                value.setBackground(new java.awt.Color(255,255,255));
                value.setColumns(2);
            }
            {
                upButton = new BasicArrowButton(1);
                this.add(getUpButton(), "1, 0");
            }
            {
                downButton = new BasicArrowButton(5);
                this.add(getDownButton(), "1, 1");
                downButton.setDirection(5);
            }
            {
                fastUpButton = new BasicArrowButton(1);
                this.add(getFastUpButton(), "2, 0");
            }
            {
                fastDownButton = new BasicArrowButton(5);
                this.add(getFastDownButton(), "2, 1");
                fastDownButton.setDirection(5);
            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }
    
    public JTextField getValue()
    {
        return value;
    }
    
    public BasicArrowButton getUpButton()
    {
        return upButton;
    }
    
    public BasicArrowButton getDownButton()
    {
        return downButton;
    }
    
    public BasicArrowButton getFastUpButton()
    {
        return fastUpButton;
    }
    
    public BasicArrowButton getFastDownButton()
    {
        return fastDownButton;
    }
    
}
