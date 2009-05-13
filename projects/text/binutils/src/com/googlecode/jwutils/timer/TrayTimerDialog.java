package com.googlecode.jwutils.timer;

import info.clearthought.layout.TableLayout;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;
import javax.swing.BorderFactory;

import javax.swing.BoxLayout;
import javax.swing.JButton;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.border.LineBorder;

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
public class TrayTimerDialog extends javax.swing.JDialog
{
    private JPanel jPanel1;
    private JScrollPane jScrollPane1;
    private JLabel jLabel5;
    private JPanel jPanel3;
    private JTextField downName;
    private JTextField upName;
    private JButton downGo;
    private JTextField downSeconds;
    private JLabel jLabel6;
    private JTextField downMinutes;
    private JLabel jLabel4;
    private JTextField downHours;
    
    public JTextField getDownSeconds()
    {
        return downSeconds;
    }
    
    public JTextField getDownMinutes()
    {
        return downMinutes;
    }
    
    public JTextField getDownHours()
    {
        return downHours;
    }
    
    public void setDownSeconds(JTextField downSeconds)
    {
        this.downSeconds = downSeconds;
    }
    
    public void setDownMinutes(JTextField downMinutes)
    {
        this.downMinutes = downMinutes;
    }
    
    public void setDownHours(JTextField downHours)
    {
        this.downHours = downHours;
    }
    
    private JButton upGo;
    private JTextField upSeconds;
    private JLabel jLabel3;
    private JTextField upMinutes;
    private JLabel jLabel2;
    private JTextField upHours;
    private JLabel jLabel1;
    private JPanel currentTimerPanel;
    private JPanel jPanel2;
    
    /**
     * Auto-generated main method to display this JDialog
     */
    public static void main(String[] args)
    {
        SwingUtilities.invokeLater(new Runnable()
        {
            public void run()
            {
                JFrame frame = new JFrame();
                TrayTimerDialog inst = new TrayTimerDialog(frame);
                inst.setVisible(true);
            }
        });
    }
    
    public TrayTimerDialog(JFrame frame)
    {
        super(frame);
        initGUI();
    }
    
    private void initGUI()
    {
        TableLayout thisLayout =
            new TableLayout(new double[][] {
                { 3.0, TableLayout.FILL, 3.0, TableLayout.FILL, 3.0 },
                { 3.0, TableLayout.FILL, 3.0 } });
        thisLayout.setHGap(5);
        thisLayout.setVGap(5);
        JPanel p = new JPanel();
        p.setLayout(thisLayout);
        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(p);
        p.setBorder(new LineBorder(Color.GRAY));
        {
            jPanel1 = new JPanel();
            TableLayout jPanel1Layout =
                new TableLayout(new double[][] {
                    { TableLayout.FILL, TableLayout.PREFERRED, TableLayout.PREFERRED,
                        TableLayout.PREFERRED, 3.0, TableLayout.PREFERRED, 3.0,
                        TableLayout.PREFERRED, TableLayout.FILL },
                    { TableLayout.FILL, TableLayout.PREFERRED, 6.0,
                        TableLayout.PREFERRED, 6.0, TableLayout.PREFERRED,
                        TableLayout.FILL, TableLayout.PREFERRED, 6.0,
                        TableLayout.PREFERRED, 6.0, TableLayout.PREFERRED,
                        TableLayout.FILL } });
            jPanel1Layout.setHGap(5);
            jPanel1Layout.setVGap(5);
            jPanel1.setLayout(jPanel1Layout);
            p.add(jPanel1, "3, 1");
            {
                jLabel1 = new JLabel();
                jPanel1.add(jLabel1, "1,1,8,1");
                jLabel1.setText("Create counting up");
            }
            {
                upHours = new JTextField();
                jPanel1.add(upHours, "1, 5");
                upHours.setText("0");
                upHours.setColumns(2);
            }
            {
                jLabel2 = new JLabel();
                jPanel1.add(jLabel2, "2, 5");
                jLabel2.setText(":");
            }
            {
                upMinutes = new JTextField();
                jPanel1.add(upMinutes, "3, 5");
                upMinutes.setText("0");
                upMinutes.setColumns(2);
            }
            {
                jLabel3 = new JLabel();
                jPanel1.add(jLabel3, "4, 5");
                jLabel3.setText(":");
            }
            {
                upSeconds = new JTextField();
                jPanel1.add(upSeconds, "5, 5");
                upSeconds.setText("0");
                upSeconds.setColumns(2);
            }
            {
                upGo = new JButton();
                jPanel1.add(upGo, "7, 5");
                upGo.setText("Go");
                upGo.setMargin(new java.awt.Insets(0, 0, 0, 0));
            }
            {
                jLabel4 = new JLabel();
                jPanel1.add(jLabel4, "1, 7, 8, 7");
                jLabel4.setText("Create counting down");
            }
            {
                downHours = new JTextField();
                jPanel1.add(downHours, "1, 11");
                downHours.setText("0");
                downHours.setColumns(2);
            }
            {
                jLabel5 = new JLabel();
                jPanel1.add(jLabel5, "2, 11");
                jLabel5.setText(":");
            }
            {
                downMinutes = new JTextField();
                jPanel1.add(downMinutes, "3, 11");
                downMinutes.setText("0");
                downMinutes.setColumns(2);
            }
            {
                jLabel6 = new JLabel();
                jPanel1.add(jLabel6, "4, 11");
                jLabel6.setText(":");
            }
            {
                downSeconds = new JTextField();
                jPanel1.add(downSeconds, "5, 11");
                downSeconds.setText("0");
                downSeconds.setColumns(2);
            }
            {
                downGo = new JButton();
                jPanel1.add(downGo, "7, 11");
                downGo.setText("Go");
                downGo.setMargin(new java.awt.Insets(0, 0, 0, 0));
            }
            {
                upName = new JTextField();
                jPanel1.add(getUpName(), "1,3,7,3");
                upName.setToolTipText("Type a name for the timer");
            }
            {
                downName = new JTextField();
                jPanel1.add(getDownName(), "1,9,7,9");
                downName.setToolTipText("Type a name for the timer");
            }
        }
        {
            jPanel2 = new JPanel();
            BorderLayout jPanel2Layout = new BorderLayout();
            jPanel2.setLayout(jPanel2Layout);
            p.add(jPanel2, "1, 1");
            {
                jScrollPane1 = new JScrollPane();
                jPanel2.add(jScrollPane1, BorderLayout.CENTER);
                {
                    jPanel3 = new JPanel();
                    BorderLayout jPanel3Layout = new BorderLayout();
                    jPanel3.setLayout(jPanel3Layout);
                    jScrollPane1.setViewportView(jPanel3);
                    {
                        currentTimerPanel = new JPanel();
                        jPanel3.add(currentTimerPanel, BorderLayout.NORTH);
                        BoxLayout currentTimerPanelLayout =
                            new BoxLayout(currentTimerPanel,
                                javax.swing.BoxLayout.Y_AXIS);
                        currentTimerPanel.setLayout(currentTimerPanelLayout);
                        currentTimerPanel.setEnabled(false);
                        currentTimerPanel.setBorder(BorderFactory.createEmptyBorder(4,
                            4, 4, 4));
                    }
                }
            }
        }
        setSize(600, 280);
    }
    
    public JPanel getCurrentTimerPanel()
    {
        return currentTimerPanel;
    }
    
    public JTextField getUpHours()
    {
        return upHours;
    }
    
    public JTextField getUpMinutes()
    {
        return upMinutes;
    }
    
    public JTextField getUpSeconds()
    {
        return upSeconds;
    }
    
    public JButton getUpGo()
    {
        return upGo;
    }
    
    public JButton getDownGo()
    {
        return downGo;
    }
    
    public JTextField getUpName()
    {
        return upName;
    }
    
    public JTextField getDownName()
    {
        return downName;
    }
    
    public JPanel getCurrentSurround()
    {
        return jPanel3;
    }
    
}
