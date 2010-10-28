package afn.traytimer.ui;

import info.clearthought.layout.TableLayout;
import java.awt.BorderLayout;

import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;

import javax.swing.WindowConstants;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

import afn.traytimer.TrayTimer;

/**
 * This code was edited or generated using CloudGarden's Jigloo SWT/Swing GUI Builder,
 * which is free for non-commercial use. If Jigloo is being used commercially (ie, by a
 * corporation, company or business for any purpose whatever) then you should purchase a
 * license for each developer using Jigloo. Please visit www.cloudgarden.com for details.
 * Use of Jigloo implies acceptance of these licensing terms. A COMMERCIAL LICENSE HAS NOT
 * BEEN PURCHASED FOR THIS MACHINE, SO JIGLOO OR THIS CODE CANNOT BE USED LEGALLY FOR ANY
 * CORPORATE OR COMMERCIAL PURPOSE.
 */
public class TimerPanel extends javax.swing.JPanel
{
    private JPanel panel;
    private JLabel announceIntervalSuffix;
    private JTextField announceIntervalField;
    private JCheckBox announceIntervalBox;
    private JPanel announceIntervalPanel;
    private JCheckBox announceOnStateChangeBox;
    private JButton stoppedButton;
    private JButton countingDownButton;
    private JButton countingUpButton;
    private MultiSpinner seconds;
    private JPanel timePanel;
    private MultiSpinner minutes;
    private MultiSpinner hours;
    private JButton setNameButton;
    private JButton deleteButton;
    private JPanel rightButtonInset;
    private JPanel rightButtonPanel;
    private JLabel nameLabel;
    private JLabel timerLabel;
    private JButton announceButton;
    private JPanel leftButtonInset;
    private JPanel leftButtonPanel;
    public int number;
    
    /**
     * Auto-generated main method to display this JPanel inside a new JFrame.
     */
    public static void main(String[] args)
    {
        JFrame frame = new JFrame();
        frame.getContentPane().add(new TimerPanel());
        frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
    }
    
    public TimerPanel()
    {
        super();
        initGUI();
    }
    
    private void initGUI()
    {
        try
        {
            BorderLayout thisLayout = new BorderLayout();
            this.setLayout(thisLayout);
            this.setPreferredSize(new java.awt.Dimension(430, 260));
            this.setOpaque(false);
            {
                panel = new JPanel();
                TableLayout panelLayout =
                        new TableLayout(new double[][] {
                                { 6.0, TableLayout.PREFERRED, TableLayout.PREFERRED, 6.0,
                                        TableLayout.FILL, TableLayout.PREFERRED },
                                { TableLayout.FILL, TableLayout.PREFERRED, 6.0,
                                        TableLayout.PREFERRED, TableLayout.FILL,
                                        TableLayout.PREFERRED, TableLayout.FILL,
                                        TableLayout.PREFERRED, TableLayout.PREFERRED,
                                        TableLayout.PREFERRED, TableLayout.FILL } });
                panel.setLayout(panelLayout);
                this.add(panel, BorderLayout.CENTER);
                panel.setOpaque(false);
                {
                    leftButtonPanel = new JPanel();
                    BorderLayout leftButtonPanelLayout = new BorderLayout();
                    panel.add(leftButtonPanel, "0, 0, 1, 4");
                    leftButtonPanel.setLayout(leftButtonPanelLayout);
                    leftButtonPanel.setOpaque(false);
                    {
                        leftButtonInset = new JPanel();
                        GridLayout leftButtonInsetLayout = new GridLayout(0, 1);
                        leftButtonInsetLayout.setColumns(1);
                        leftButtonInsetLayout.setRows(0);
                        leftButtonPanel.add(leftButtonInset, BorderLayout.NORTH);
                        leftButtonInset.setLayout(leftButtonInsetLayout);
                        {
                            announceButton = new JButton();
                            leftButtonInset.add(announceButton);
                            announceButton.setText("Announce");
                            announceButton.addActionListener(new ActionListener()
                            {
                                public void actionPerformed(ActionEvent evt)
                                {
                                    TrayTimer.onAnnounce(number);
                                }
                            });
                        }
                    }
                }
                {
                    timerLabel = new JLabel();
                    panel.add(getTimerLabel(), "2,1,4,1");
                    timerLabel.setText("Timer 1");
                    timerLabel.setHorizontalAlignment(SwingConstants.CENTER);
                    timerLabel.setFont(new java.awt.Font("Dialog", 1, 22));
                }
                {
                    nameLabel = new JLabel();
                    panel.add(getNameLabel(), "2,3,4,3");
                    nameLabel.setText("Example Timer");
                    nameLabel.setHorizontalAlignment(SwingConstants.CENTER);
                    nameLabel.setFont(new java.awt.Font("Dialog", 0, 16));
                }
                {
                    rightButtonPanel = new JPanel();
                    BorderLayout rightButtonPanelLayout = new BorderLayout();
                    panel.add(rightButtonPanel, "5, 0, 5, 4");
                    rightButtonPanel.setLayout(rightButtonPanelLayout);
                    rightButtonPanel.setOpaque(false);
                    {
                        rightButtonInset = new JPanel();
                        GridLayout rightButtonInsetLayout = new GridLayout(0, 1);
                        rightButtonInsetLayout.setColumns(1);
                        rightButtonInsetLayout.setRows(0);
                        rightButtonPanel.add(rightButtonInset, BorderLayout.NORTH);
                        rightButtonInset.setLayout(rightButtonInsetLayout);
                        {
                            deleteButton = new JButton();
                            rightButtonInset.add(deleteButton);
                            deleteButton.setText("Delete");
                            deleteButton.addActionListener(new ActionListener()
                            {
                                public void actionPerformed(ActionEvent evt)
                                {
                                    TrayTimer.onDelete(number);
                                }
                            });
                        }
                        {
                            setNameButton = new JButton();
                            rightButtonInset.add(getSetNameButton());
                            setNameButton.setText("Set Name");
                        }
                    }
                }
                {
                    timePanel = new JPanel();
                    GridLayout timePanelLayout = new GridLayout(1, 1);
                    timePanel.setLayout(timePanelLayout);
                    panel.add(timePanel, "2,5,4,5,c,c");
                    {
                        hours = new MultiSpinner();
                        timePanel.add(getHours());
                    }
                    {
                        minutes = new MultiSpinner();
                        timePanel.add(getMinutes());
                    }
                    {
                        seconds = new MultiSpinner();
                        timePanel.add(getSeconds());
                    }
                }
                {
                    countingUpButton = new JButton();
                    panel.add(getCountingUpButton(), "1, 7, 2, 7");
                    countingUpButton.setText("Counting Up");
                    countingUpButton.setMargin(new java.awt.Insets(1, 1, 1, 1));
                    countingUpButton.addActionListener(new ActionListener()
                    {
                        public void actionPerformed(ActionEvent evt)
                        {
                            TrayTimer.onStateButton(number, 1);
                        }
                    });
                }
                {
                    countingDownButton = new JButton();
                    panel.add(countingDownButton, "1,9,2,9");
                    countingDownButton.setText("Counting Down");
                    countingDownButton.setMargin(new java.awt.Insets(1, 1, 1, 1));
                    countingDownButton.setSelected(true);
                    countingDownButton.addActionListener(new ActionListener()
                    {
                        public void actionPerformed(ActionEvent evt)
                        {
                            TrayTimer.onStateButton(number, 2);
                        }
                    });
                }
                {
                    stoppedButton = new JButton();
                    panel.add(stoppedButton, "1,8,2,8");
                    stoppedButton.setText("Stopped");
                    stoppedButton.setMargin(new java.awt.Insets(1, 1, 1, 1));
                    stoppedButton.setBackground(new java.awt.Color(192, 205, 255));
                    stoppedButton.addActionListener(new ActionListener()
                    {
                        public void actionPerformed(ActionEvent evt)
                        {
                            TrayTimer.onStateButton(number, 3);
                        }
                    });
                }
                {
                    announceOnStateChangeBox = new JCheckBox();
                    panel.add(getAnnounceOnStateChangeBox(), "4, 7, 5, 7");
                    announceOnStateChangeBox.setText("Announce on state change");
                    announceOnStateChangeBox.setOpaque(false);
                }
                {
                    announceIntervalPanel = new JPanel();
                    BoxLayout announceIntervalPanelLayout =
                            new BoxLayout(announceIntervalPanel,
                                    javax.swing.BoxLayout.X_AXIS);
                    panel.add(announceIntervalPanel, "4,8,5,8,l,f");
                    announceIntervalPanel.setLayout(announceIntervalPanelLayout);
                    announceIntervalPanel.setOpaque(false);
                    {
                        announceIntervalBox = new JCheckBox();
                        announceIntervalPanel.add(getAnnounceIntervalBox());
                        announceIntervalBox.setText("Announce every ");
                        announceIntervalBox.setOpaque(false);
                    }
                    {
                        announceIntervalField = new JTextField();
                        announceIntervalPanel.add(getAnnounceIntervalField());
                        announceIntervalField.setText("5");
                        announceIntervalField.setColumns(2);
                    }
                    {
                        announceIntervalSuffix = new JLabel();
                        announceIntervalPanel.add(announceIntervalSuffix);
                        announceIntervalSuffix.setText(" minutes");
                    }
                }
            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }
    
    public JButton getAnnounceButton()
    {
        return announceButton;
    }
    
    public JLabel getTimerLabel()
    {
        return timerLabel;
    }
    
    public JLabel getNameLabel()
    {
        return nameLabel;
    }
    
    public JButton getDeleteButton()
    {
        return deleteButton;
    }
    
    public JButton getSetNameButton()
    {
        return setNameButton;
    }
    
    public MultiSpinner getHours()
    {
        return hours;
    }
    
    public MultiSpinner getMinutes()
    {
        return minutes;
    }
    
    public MultiSpinner getSeconds()
    {
        return seconds;
    }
    
    public JButton getCountingUpButton()
    {
        return countingUpButton;
    }
    
    public JButton getCountingDownButton()
    {
        return countingDownButton;
    }
    
    public JButton getStoppedButton()
    {
        return stoppedButton;
    }
    
    public JCheckBox getAnnounceOnStateChangeBox()
    {
        return announceOnStateChangeBox;
    }
    
    public JCheckBox getAnnounceIntervalBox()
    {
        return announceIntervalBox;
    }
    
    public JTextField getAnnounceIntervalField()
    {
        return announceIntervalField;
    }
    
}
