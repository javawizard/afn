package jw.flashcards;

import info.clearthought.layout.TableLayout;
import java.awt.BorderLayout;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import javax.swing.WindowConstants;
import javax.swing.SwingUtilities;

import jw.flashcards.Flashcards.Operation;

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
public class ChoiceFrame extends javax.swing.JFrame
{
    private JPanel jPanel1;
    private JTextField firstEnd;
    private JLabel jLabel3;
    private JLabel jLabel4;
    private JLabel jLabel5;
    private JButton goButton;
    private JComboBox sideField;
    private JLabel jLabel6;
    private JComboBox operationField;
    private JTextField secondEnd;
    private JTextField secondStart;
    private JLabel jLabel2;
    private JTextField firstStart;
    private JLabel jLabel1;
    
    /**
     * Auto-generated main method to display this JFrame
     */
    public static void main(String[] args)
    {
        SwingUtilities.invokeLater(new Runnable()
        {
            public void run()
            {
                ChoiceFrame inst = new ChoiceFrame();
                inst.setLocationRelativeTo(null);
                inst.setVisible(true);
            }
        });
    }
    
    public ChoiceFrame()
    {
        super();
        initGUI();
    }
    
    private void initGUI()
    {
        try
        {
            setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
            {
                jPanel1 = new JPanel();
                TableLayout jPanel1Layout = new TableLayout(new double[][]
                {
                        {
                                3.0, TableLayout.FILL, TableLayout.PREFERRED,
                                TableLayout.PREFERRED, TableLayout.PREFERRED,
                                3.0
                        },
                        {
                                3.0, TableLayout.PREFERRED,
                                TableLayout.PREFERRED, TableLayout.PREFERRED,
                                TableLayout.PREFERRED, TableLayout.PREFERRED,
                                3.0
                        }
                });
                jPanel1Layout.setHGap(5);
                jPanel1Layout.setVGap(5);
                jPanel1.setLayout(jPanel1Layout);
                getContentPane().add(jPanel1, BorderLayout.CENTER);
                {
                    jLabel1 = new JLabel();
                    jPanel1.add(jLabel1, "1, 1");
                    jLabel1.setText("First: ");
                }
                {
                    firstStart = new JTextField();
                    jPanel1.add(getFirstStart(), "2, 1");
                    firstStart.setColumns(3);
                }
                {
                    jLabel2 = new JLabel();
                    jPanel1.add(jLabel2, "3, 1");
                    jLabel2.setText(" to ");
                }
                {
                    firstEnd = new JTextField();
                    jPanel1.add(getFirstEnd(), "4, 1");
                    firstEnd.setColumns(3);
                }
                {
                    jLabel3 = new JLabel();
                    jPanel1.add(jLabel3, "1, 2");
                    jLabel3.setText("Second:");
                }
                {
                    secondStart = new JTextField();
                    jPanel1.add(getSecondStart(), "2, 2");
                    secondStart.setColumns(3);
                }
                {
                    jLabel4 = new JLabel();
                    jPanel1.add(jLabel4, "3, 2");
                    jLabel4.setText(" to ");
                }
                {
                    secondEnd = new JTextField();
                    jPanel1.add(getSecondEnd(), "4, 2");
                    secondEnd.setColumns(3);
                }
                {
                    jLabel5 = new JLabel();
                    jPanel1.add(jLabel5, "1, 3");
                    jLabel5.setText("Operation:");
                }
                {
                    String[] operationNames = new String[Operation.values().length];
                    for (int i = 0; i < Operation.values().length; i++)
                    {
                        operationNames[i] = Operation.values()[i].name();
                    }
                    ComboBoxModel operationFieldModel = new DefaultComboBoxModel(
                            operationNames);
                    operationField = new JComboBox();
                    jPanel1.add(operationField, "2,3,4,3");
                    operationField.setModel(operationFieldModel);
                    if (operationField.getItemCount() > 2)
                        operationField.setSelectedIndex(2);
                }
                {
                    jLabel6 = new JLabel();
                    jPanel1.add(jLabel6, "1, 4");
                    jLabel6.setText("Side:");
                }
                {
                    ComboBoxModel sideFieldModel = new DefaultComboBoxModel(
                            new String[]
                            {
                                    "Front", "Back"
                            });
                    sideField = new JComboBox();
                    jPanel1.add(getSideField(), "2,4,4,4");
                    sideField.setModel(sideFieldModel);
                }
                {
                    goButton = new JButton();
                    jPanel1.add(getGoButton(), "4, 5");
                    goButton.setText("Go");
                    goButton.setMargin(new java.awt.Insets(0, 0, 0, 0));
                }
            }
            pack();
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }
    
    public JTextField getFirstStart()
    {
        return firstStart;
    }
    
    public JTextField getFirstEnd()
    {
        return firstEnd;
    }
    
    public JTextField getSecondStart()
    {
        return secondStart;
    }
    
    public JTextField getSecondEnd()
    {
        return secondEnd;
    }
    
    public JComboBox getSideField()
    {
        return sideField;
    }
    
    public JButton getGoButton()
    {
        return goButton;
    }
    
    public JComboBox getOperationField()
    {
        return operationField;
    }
    
}
