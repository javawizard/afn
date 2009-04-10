package org.opengroove.sixjet.controller.ui.frames;

import java.awt.BorderLayout;
import javax.swing.BoxLayout;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ListModel;
import javax.swing.SwingConstants;

import javax.swing.WindowConstants;
import org.opengroove.sixjet.common.ui.HeaderComponent;
import javax.swing.SwingUtilities;

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
public class MainFrame extends javax.swing.JFrame
{
    private JPanel outerPanel;
    private JSplitPane topContentLeftSplit;
    private JScrollPane jScrollPane2;
    private JList scheduleList;
    private HeaderComponent scheduleHeader;
    private JPanel schedulePanel;
    private JButton chatSendButton;
    private JSplitPane scheduleChatSplit;
    private JPanel chatSouth;
    private JTextField chatTextField;
    private JScrollPane jScrollPane1;
    private JTextArea chatTextArea;
    private HeaderComponent chatPanelHeader;
    private JPanel chatPanel;
    private JSplitPane playlistSplit;
    private JSplitPane topContentRightSplit;
    private JSplitPane musicPlaybackSplit;
    private JSplitPane mainSplit;
    
    /**
     * Auto-generated main method to display this JFrame
     */
    public static void main(String[] args)
    {
        SwingUtilities.invokeLater(new Runnable()
        {
            public void run()
            {
                MainFrame inst = new MainFrame();
                inst.setLocationRelativeTo(null);
                inst.setVisible(true);
            }
        });
    }
    
    public MainFrame()
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
                outerPanel = new JPanel();
                BorderLayout outerPanelLayout = new BorderLayout();
                getContentPane().add(outerPanel, BorderLayout.CENTER);
                outerPanel.setLayout(outerPanelLayout);
                {
                    mainSplit = new JSplitPane();
                    outerPanel.add(mainSplit, BorderLayout.CENTER);
                    mainSplit.setOrientation(JSplitPane.VERTICAL_SPLIT);
                    mainSplit.setDividerLocation(420);
                    mainSplit.setResizeWeight(1.0);
                    mainSplit.setContinuousLayout(true);
                    {
                        musicPlaybackSplit = new JSplitPane();
                        mainSplit.add(musicPlaybackSplit, JSplitPane.BOTTOM);
                        musicPlaybackSplit.setContinuousLayout(true);
                        musicPlaybackSplit.setDividerLocation(336);
                        musicPlaybackSplit.setResizeWeight(0.5);
                    }
                    {
                        topContentLeftSplit = new JSplitPane();
                        mainSplit.add(topContentLeftSplit, JSplitPane.TOP);
                        topContentLeftSplit.setContinuousLayout(true);
                        topContentLeftSplit.setDividerLocation(125);
                        {
                            topContentRightSplit = new JSplitPane();
                            topContentLeftSplit.add(topContentRightSplit,
                                JSplitPane.RIGHT);
                            topContentRightSplit.setResizeWeight(1.0);
                            topContentRightSplit.setDividerLocation(399);
                            topContentRightSplit.setContinuousLayout(true);
                            {
                                scheduleChatSplit = new JSplitPane();
                                topContentRightSplit.add(scheduleChatSplit,
                                    JSplitPane.RIGHT);
                                scheduleChatSplit
                                    .setOrientation(JSplitPane.VERTICAL_SPLIT);
                                scheduleChatSplit.setDividerLocation(215);
                                scheduleChatSplit.setContinuousLayout(true);
                                scheduleChatSplit.setResizeWeight(0.5);
                                {
                                    chatPanel = new JPanel();
                                    BorderLayout chatPanelLayout = new BorderLayout();
                                    scheduleChatSplit.add(chatPanel, JSplitPane.BOTTOM);
                                    chatPanel.setLayout(chatPanelLayout);
                                    {
                                        chatPanelHeader = new HeaderComponent();
                                        chatPanel.add(chatPanelHeader,
                                            BorderLayout.NORTH);
                                        chatPanelHeader.setLabel("Chat");
                                    }
                                    {
                                        jScrollPane1 = new JScrollPane();
                                        chatPanel
                                            .add(jScrollPane1, BorderLayout.CENTER);
                                        jScrollPane1
                                            .setPreferredSize(new java.awt.Dimension(
                                                124, 162));
                                        {
                                            chatTextArea = new JTextArea();
                                            jScrollPane1
                                                .setViewportView(getChatTextArea());
                                            chatTextArea.setEditable(false);
                                            chatTextArea.setOpaque(false);
                                        }
                                    }
                                    {
                                        chatSouth = new JPanel();
                                        BorderLayout chatSouthLayout =
                                            new BorderLayout();
                                        chatPanel.add(chatSouth, BorderLayout.SOUTH);
                                        chatSouth.setLayout(chatSouthLayout);
                                        {
                                            chatTextField = new JTextField();
                                            chatSouth.add(getChatTextField(),
                                                BorderLayout.NORTH);
                                        }
                                        {
                                            chatSendButton = new JButton();
                                            chatSouth.add(chatSendButton,
                                                BorderLayout.SOUTH);
                                            chatSendButton.setText("Send");
                                        }
                                    }
                                }
                                {
                                    schedulePanel = new JPanel();
                                    BorderLayout schedulePanelLayout =
                                        new BorderLayout();
                                    scheduleChatSplit
                                        .add(schedulePanel, JSplitPane.TOP);
                                    schedulePanel.setLayout(schedulePanelLayout);
                                    {
                                        scheduleHeader = new HeaderComponent();
                                        schedulePanel.add(scheduleHeader, BorderLayout.NORTH);
                                        scheduleHeader.setLabel("Schedule");
                                    }
                                    {
                                        jScrollPane2 = new JScrollPane();
                                        schedulePanel.add(jScrollPane2, BorderLayout.CENTER);
                                        jScrollPane2.setPreferredSize(new java.awt.Dimension(124, 187));
                                        {
                                            ListModel scheduleListModel = 
                                                new DefaultComboBoxModel(
                                                    new String[] { "Item One", "Item Two" });
                                            scheduleList = new JList();
                                            jScrollPane2.setViewportView(getScheduleList());
                                            scheduleList.setModel(scheduleListModel);
                                        }
                                    }
                                }
                            }
                        }
                        {
                            playlistSplit = new JSplitPane();
                            topContentLeftSplit.add(playlistSplit, JSplitPane.LEFT);
                            playlistSplit.setOrientation(JSplitPane.VERTICAL_SPLIT);
                            playlistSplit.setContinuousLayout(true);
                            playlistSplit.setDividerLocation(215);
                            playlistSplit.setResizeWeight(0.5);
                        }
                    }
                }
            }
            pack();
            this.setSize(690, 710);
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }
    
    public JTextArea getChatTextArea()
    {
        return chatTextArea;
    }
    
    public JTextField getChatTextField()
    {
        return chatTextField;
    }
    
    public JList getScheduleList() {
        return scheduleList;
    }

}
