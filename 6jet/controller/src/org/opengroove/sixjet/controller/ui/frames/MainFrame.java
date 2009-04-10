package org.opengroove.sixjet.controller.ui.frames;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import javax.swing.BoxLayout;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
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
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

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
    private JLabel timeMsLabel;
    private JTextField manualControlTimeField;
    private JLabel manualControlTimeLabel;
    private JPanel manualControlTimePanel;
    private JCheckBox manualControlFixedCheckbox;
    private JCheckBox manualControlToggleCheckbox;
    private JPanel centerPanelSouth;
    private JButton musicDownloadButton;
    private JButton musicDeleteButton;
    private JButton musicPlayButton;
    private JButton musicUploadButton;
    private JPanel musicSouth;
    private JButton editPlaylistDownButton;
    private JButton editPlaylistUpButton;
    private JPanel editPlaylistMovePanel;
    private JLabel editPlaylistMoveLabel;
    private JPanel editPlaylistSouthTop;
    private JPanel editPlaylistAddPanel;
    private JLabel editPlaylistAddLabel;
    private JPanel editPlaylistSouthMid;
    private HeaderComponent centerPanelHeader;
    private JPanel centerPanelNorth;
    private JPanel centerPanel;
    private JScrollPane jScrollPane4;
    private JList editPlaylistList;
    private JButton playlistDeleteItemButton;
    private JButton playlistAddDelayButton;
    private JButton playlistAddMusicButton;
    private JScrollPane jScrollPane3;
    private JList playlistList;
    private JButton playlistPlayButton;
    private JButton playlistDeleteButton;
    private JButton playlistAddButton;
    private JPanel editPlaylistSouth;
    private JPanel playlistsSouth;
    private HeaderComponent nowPlayingHeader;
    private HeaderComponent musicHeader;
    private HeaderComponent editPlaylistHeader;
    private HeaderComponent playlistsHeader;
    private JScrollPane jScrollPane2;
    private JPanel nowPlayingPanel;
    private JPanel musicPanel;
    private JPanel editPlaylistPanel;
    private JPanel playlistsPanel;
    private JButton scheduleDeleteButton;
    private JButton scheduleAddButton;
    private JPanel scheduleSouth;
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
                        {
                            musicPanel = new JPanel();
                            BorderLayout musicPanelLayout = new BorderLayout();
                            musicPlaybackSplit.add(musicPanel, JSplitPane.LEFT);
                            musicPanel.setLayout(musicPanelLayout);
                            {
                                musicHeader = new HeaderComponent();
                                musicPanel.add(musicHeader, BorderLayout.NORTH);
                                musicHeader.setLabel("Music");
                            }
                            {
                                musicSouth = new JPanel();
                                GridLayout musicSouthLayout = new GridLayout(1, 1);
                                musicPanel.add(musicSouth, BorderLayout.SOUTH);
                                musicSouth.setLayout(musicSouthLayout);
                                {
                                    musicUploadButton = new JButton();
                                    musicSouth.add(getMusicUploadButton());
                                    musicUploadButton.setText("Upload");
                                    musicUploadButton.setMargin(new java.awt.Insets(0,
                                        0, 0, 0));
                                }
                                {
                                    musicPlayButton = new JButton();
                                    musicSouth.add(getMusicPlayButton());
                                    musicPlayButton.setText("Play");
                                    musicPlayButton.setMargin(new java.awt.Insets(0, 0,
                                        0, 0));
                                }
                                {
                                    musicDeleteButton = new JButton();
                                    musicSouth.add(getMusicDeleteButton());
                                    musicDeleteButton.setText("Delete");
                                    musicDeleteButton.setMargin(new java.awt.Insets(0,
                                        0, 0, 0));
                                }
                                {
                                    musicDownloadButton = new JButton();
                                    musicSouth.add(getMusicDownloadButton());
                                    musicDownloadButton.setText("Download");
                                    musicDownloadButton.setMargin(new java.awt.Insets(
                                        0, 0, 0, 0));
                                }
                            }
                        }
                        {
                            nowPlayingPanel = new JPanel();
                            BorderLayout nowPlayingPanelLayout = new BorderLayout();
                            musicPlaybackSplit.add(nowPlayingPanel, JSplitPane.RIGHT);
                            nowPlayingPanel.setLayout(nowPlayingPanelLayout);
                            {
                                nowPlayingHeader = new HeaderComponent();
                                nowPlayingPanel.add(nowPlayingHeader,
                                    BorderLayout.NORTH);
                                nowPlayingHeader.setLabel("Now Playing");
                            }
                        }
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
                                        jScrollPane1
                                            .setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
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
                                            chatSendButton
                                                .setMargin(new java.awt.Insets(0, 14,
                                                    0, 14));
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
                                        schedulePanel.add(scheduleHeader,
                                            BorderLayout.NORTH);
                                        scheduleHeader.setLabel("Schedule");
                                    }
                                    {
                                        jScrollPane2 = new JScrollPane();
                                        schedulePanel.add(jScrollPane2,
                                            BorderLayout.CENTER);
                                        jScrollPane2
                                            .setPreferredSize(new java.awt.Dimension(
                                                124, 187));
                                        {
                                            ListModel scheduleListModel =
                                                new DefaultComboBoxModel(new String[] {
                                                    "Item One", "Item Two" });
                                            scheduleList = new JList();
                                            jScrollPane2
                                                .setViewportView(getScheduleList());
                                            scheduleList.setModel(scheduleListModel);
                                        }
                                    }
                                    {
                                        scheduleSouth = new JPanel();
                                        BorderLayout scheduleSouthLayout =
                                            new BorderLayout();
                                        schedulePanel.add(scheduleSouth,
                                            BorderLayout.SOUTH);
                                        scheduleSouth.setLayout(scheduleSouthLayout);
                                        {
                                            scheduleAddButton = new JButton();
                                            scheduleSouth.add(getScheduleAddButton(),
                                                BorderLayout.NORTH);
                                            scheduleAddButton.setText("Add");
                                            scheduleAddButton
                                                .setMargin(new java.awt.Insets(0, 14,
                                                    0, 14));
                                        }
                                        {
                                            scheduleDeleteButton = new JButton();
                                            scheduleSouth.add(
                                                getScheduleDeleteButton(),
                                                BorderLayout.SOUTH);
                                            scheduleDeleteButton.setText("Delete");
                                            scheduleDeleteButton
                                                .setMargin(new java.awt.Insets(0, 14,
                                                    0, 14));
                                        }
                                    }
                                }
                            }
                            {
                                centerPanel = new JPanel();
                                BorderLayout centerPanelLayout = new BorderLayout();
                                topContentRightSplit.add(centerPanel, JSplitPane.LEFT);
                                centerPanel.setLayout(centerPanelLayout);
                                {
                                    centerPanelNorth = new JPanel();
                                    BorderLayout centerPanelNorthLayout =
                                        new BorderLayout();
                                    centerPanel.add(centerPanelNorth,
                                        BorderLayout.NORTH);
                                    centerPanelNorth.setLayout(centerPanelNorthLayout);
                                    {
                                        centerPanelHeader = new HeaderComponent();
                                        centerPanelNorth.add(centerPanelHeader,
                                            BorderLayout.NORTH);
                                        centerPanelHeader
                                            .setLabel("<html><big>6jet Controller</big></html>");
                                    }
                                }
                                {
                                    centerPanelSouth = new JPanel();
                                    FlowLayout centerPanelSouthLayout =
                                        new FlowLayout();
                                    centerPanelSouthLayout
                                        .setAlignment(FlowLayout.LEFT);
                                    centerPanelSouthLayout.setHgap(3);
                                    centerPanelSouthLayout.setVgap(3);
                                    centerPanel.add(centerPanelSouth,
                                        BorderLayout.SOUTH);
                                    centerPanelSouth.setLayout(centerPanelSouthLayout);
                                    {
                                        manualControlToggleCheckbox = new JCheckBox();
                                        centerPanelSouth
                                            .add(getManualControlToggleCheckbox());
                                        manualControlToggleCheckbox.setText("Toggle");
                                    }
                                    {
                                        manualControlFixedCheckbox = new JCheckBox();
                                        centerPanelSouth
                                            .add(getManualControlFixedCheckbox());
                                        manualControlFixedCheckbox
                                            .setText("Fixed time");
                                    }
                                    {
                                        manualControlTimePanel = new JPanel();
                                        BorderLayout manualControlTimePanelLayout =
                                            new BorderLayout();
                                        centerPanelSouth.add(manualControlTimePanel);
                                        manualControlTimePanel
                                            .setLayout(manualControlTimePanelLayout);
                                        {
                                            manualControlTimeLabel = new JLabel();
                                            manualControlTimePanel.add(
                                                manualControlTimeLabel,
                                                BorderLayout.WEST);
                                            manualControlTimeLabel.setText("Time: ");
                                            manualControlTimeLabel
                                                .setPreferredSize(new java.awt.Dimension(
                                                    34, 20));
                                        }
                                        {
                                            manualControlTimeField = new JTextField();
                                            manualControlTimeField.getDocument()
                                                .addDocumentListener(
                                                    new DocumentListener()
                                                    {
                                                        
                                                        public void changedUpdate(
                                                            DocumentEvent e)
                                                        {
                                                            onTimeFieldChanged();
                                                        }
                                                        
                                                        public void insertUpdate(
                                                            DocumentEvent e)
                                                        {
                                                            changedUpdate(e);
                                                        }
                                                        
                                                        public void removeUpdate(
                                                            DocumentEvent e)
                                                        {
                                                            changedUpdate(e);
                                                        }
                                                    });
                                            manualControlTimePanel.add(
                                                manualControlTimeField,
                                                BorderLayout.CENTER);
                                            manualControlTimeField.setText("250");
                                            manualControlTimeField
                                                .setPreferredSize(new java.awt.Dimension(
                                                    40, 20));
                                        }
                                        {
                                            timeMsLabel = new JLabel();
                                            manualControlTimePanel.add(timeMsLabel,
                                                BorderLayout.EAST);
                                            timeMsLabel.setText("ms");
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
                            {
                                playlistsPanel = new JPanel();
                                BorderLayout playlistsPanelLayout = new BorderLayout();
                                playlistSplit.add(playlistsPanel, JSplitPane.TOP);
                                playlistsPanel.setLayout(playlistsPanelLayout);
                                {
                                    playlistsHeader = new HeaderComponent();
                                    playlistsPanel.add(playlistsHeader,
                                        BorderLayout.NORTH);
                                    playlistsHeader.setLabel("Playlists");
                                }
                                {
                                    playlistsSouth = new JPanel();
                                    BorderLayout playlistsSouthLayout =
                                        new BorderLayout();
                                    playlistsPanel.add(playlistsSouth,
                                        BorderLayout.SOUTH);
                                    playlistsSouth.setLayout(playlistsSouthLayout);
                                    {
                                        playlistAddButton = new JButton();
                                        playlistsSouth.add(playlistAddButton,
                                            BorderLayout.NORTH);
                                        playlistAddButton.setText("Add");
                                        playlistAddButton
                                            .setMargin(new java.awt.Insets(0, 14, 0, 14));
                                    }
                                    {
                                        playlistDeleteButton = new JButton();
                                        playlistsSouth.add(playlistDeleteButton,
                                            BorderLayout.CENTER);
                                        playlistDeleteButton.setText("Delete");
                                        playlistDeleteButton
                                            .setMargin(new java.awt.Insets(0, 14, 0, 14));
                                    }
                                    {
                                        playlistPlayButton = new JButton();
                                        playlistsSouth.add(playlistPlayButton,
                                            BorderLayout.SOUTH);
                                        playlistPlayButton.setText("Play");
                                        playlistPlayButton
                                            .setMargin(new java.awt.Insets(0, 14, 0, 14));
                                    }
                                }
                                {
                                    jScrollPane3 = new JScrollPane();
                                    playlistsPanel.add(jScrollPane3,
                                        BorderLayout.CENTER);
                                    jScrollPane3
                                        .setPreferredSize(new java.awt.Dimension(122,
                                            109));
                                    {
                                        ListModel playlistListModel =
                                            new DefaultComboBoxModel(new String[] {
                                                "Item One", "Item Two" });
                                        playlistList = new JList();
                                        jScrollPane3.setViewportView(getPlaylistList());
                                        playlistList.setModel(playlistListModel);
                                    }
                                }
                            }
                            {
                                editPlaylistPanel = new JPanel();
                                BorderLayout editPlaylistPanelLayout =
                                    new BorderLayout();
                                playlistSplit.add(editPlaylistPanel, JSplitPane.BOTTOM);
                                editPlaylistPanel.setLayout(editPlaylistPanelLayout);
                                {
                                    editPlaylistHeader = new HeaderComponent();
                                    editPlaylistPanel.add(editPlaylistHeader,
                                        BorderLayout.NORTH);
                                    editPlaylistHeader.setLabel("Edit Playlist");
                                }
                                {
                                    editPlaylistSouth = new JPanel();
                                    BorderLayout editPlaylistSouthLayout =
                                        new BorderLayout();
                                    editPlaylistPanel.add(editPlaylistSouth,
                                        BorderLayout.SOUTH);
                                    editPlaylistSouth
                                        .setLayout(editPlaylistSouthLayout);
                                    {
                                        playlistDeleteItemButton = new JButton();
                                        editPlaylistSouth.add(
                                            getPlaylistDeleteItemButton(),
                                            BorderLayout.SOUTH);
                                        playlistDeleteItemButton.setText("Delete Item");
                                        playlistDeleteItemButton
                                            .setMargin(new java.awt.Insets(0, 14, 0, 14));
                                    }
                                    {
                                        editPlaylistSouthMid = new JPanel();
                                        BorderLayout editPlaylistSouthMidLayout =
                                            new BorderLayout();
                                        editPlaylistSouth.add(editPlaylistSouthMid,
                                            BorderLayout.CENTER);
                                        editPlaylistSouthMid
                                            .setLayout(editPlaylistSouthMidLayout);
                                        {
                                            editPlaylistAddLabel = new JLabel();
                                            editPlaylistSouthMid
                                                .add(editPlaylistAddLabel,
                                                    BorderLayout.WEST);
                                            editPlaylistAddLabel.setText("Add:");
                                            editPlaylistAddLabel
                                                .setPreferredSize(new java.awt.Dimension(
                                                    38, 22));
                                        }
                                        {
                                            editPlaylistAddPanel = new JPanel();
                                            GridLayout editPlaylistAddPanelLayout =
                                                new GridLayout(1, 2);
                                            editPlaylistAddPanelLayout.setColumns(1);
                                            editPlaylistAddPanelLayout.setHgap(0);
                                            editPlaylistAddPanelLayout.setVgap(0);
                                            editPlaylistSouthMid.add(
                                                editPlaylistAddPanel,
                                                BorderLayout.CENTER);
                                            editPlaylistAddPanel
                                                .setLayout(editPlaylistAddPanelLayout);
                                            {
                                                playlistAddDelayButton = new JButton();
                                                editPlaylistAddPanel
                                                    .add(playlistAddDelayButton);
                                                playlistAddDelayButton.setText("Delay");
                                                playlistAddDelayButton
                                                    .setMargin(new java.awt.Insets(0,
                                                        0, 0, 0));
                                            }
                                            {
                                                playlistAddMusicButton = new JButton();
                                                editPlaylistAddPanel
                                                    .add(playlistAddMusicButton);
                                                playlistAddMusicButton.setText("Music");
                                                playlistAddMusicButton
                                                    .setMargin(new java.awt.Insets(0,
                                                        0, 0, 0));
                                            }
                                        }
                                    }
                                    {
                                        editPlaylistSouthTop = new JPanel();
                                        BorderLayout editPlaylistSouthTopLayout =
                                            new BorderLayout();
                                        editPlaylistSouth.add(editPlaylistSouthTop,
                                            BorderLayout.NORTH);
                                        editPlaylistSouthTop
                                            .setLayout(editPlaylistSouthTopLayout);
                                        {
                                            editPlaylistMoveLabel = new JLabel();
                                            editPlaylistSouthTop.add(
                                                editPlaylistMoveLabel,
                                                BorderLayout.WEST);
                                            editPlaylistMoveLabel.setText("Move:");
                                            editPlaylistMoveLabel
                                                .setPreferredSize(new java.awt.Dimension(
                                                    38, 22));
                                        }
                                        {
                                            editPlaylistMovePanel = new JPanel();
                                            GridLayout editPlaylistMovePanelLayout =
                                                new GridLayout(1, 1);
                                            editPlaylistSouthTop.add(
                                                editPlaylistMovePanel,
                                                BorderLayout.CENTER);
                                            editPlaylistMovePanel
                                                .setLayout(editPlaylistMovePanelLayout);
                                            {
                                                editPlaylistUpButton = new JButton();
                                                editPlaylistMovePanel
                                                    .add(editPlaylistUpButton);
                                                editPlaylistUpButton
                                                    .setText("<html>&uarr;");
                                                editPlaylistUpButton
                                                    .setMargin(new java.awt.Insets(0,
                                                        0, 0, 0));
                                            }
                                            {
                                                editPlaylistDownButton = new JButton();
                                                editPlaylistMovePanel
                                                    .add(getEditPlaylistDownButton());
                                                editPlaylistDownButton
                                                    .setText("<html>&darr;");
                                                editPlaylistDownButton
                                                    .setMargin(new java.awt.Insets(0,
                                                        0, 0, 0));
                                            }
                                        }
                                    }
                                }
                                {
                                    jScrollPane4 = new JScrollPane();
                                    editPlaylistPanel.add(jScrollPane4,
                                        BorderLayout.CENTER);
                                    jScrollPane4
                                        .setPreferredSize(new java.awt.Dimension(122,
                                            98));
                                    {
                                        ListModel editPlaylistListModel =
                                            new DefaultComboBoxModel(new String[] {
                                                "Item One", "Item Two" });
                                        editPlaylistList = new JList();
                                        jScrollPane4
                                            .setViewportView(getEditPlaylistList());
                                        editPlaylistList
                                            .setModel(editPlaylistListModel);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            pack();
            this.setSize(690, 690);
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }
    
    protected void onTimeFieldChanged()
    {
        final String text = manualControlTimeField.getText();
        if ((!text.replaceAll("[^0-9]", "").equals(text)) || text.length() == 0)
        {
            /*
             * There were some non-number characters in the text. We'll update
             * the text field with numbers removed.
             */
            SwingUtilities.invokeLater(new Runnable()
            {
                
                public void run()
                {
                    manualControlTimeField.setText(text.replaceAll("[^0-9]", ""));
                    if (text.length() == 0)
                        manualControlTimeField.setText("0");
                }
            });
            
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
    
    public JList getScheduleList()
    {
        return scheduleList;
    }
    
    public JButton getScheduleAddButton()
    {
        return scheduleAddButton;
    }
    
    public JButton getScheduleDeleteButton()
    {
        return scheduleDeleteButton;
    }
    
    public JList getPlaylistList()
    {
        return playlistList;
    }
    
    public JButton getPlaylistAddMusicButton()
    {
        return playlistAddMusicButton;
    }
    
    public JButton getPlaylistAddDelayButton()
    {
        return playlistAddDelayButton;
    }
    
    public JButton getPlaylistDeleteItemButton()
    {
        return playlistDeleteItemButton;
    }
    
    public JList getEditPlaylistList()
    {
        return editPlaylistList;
    }
    
    public JButton getEditPlaylistDownButton()
    {
        return editPlaylistDownButton;
    }
    
    public JButton getMusicUploadButton()
    {
        return musicUploadButton;
    }
    
    public JButton getMusicPlayButton()
    {
        return musicPlayButton;
    }
    
    public JButton getMusicDeleteButton()
    {
        return musicDeleteButton;
    }
    
    public JButton getMusicDownloadButton()
    {
        return musicDownloadButton;
    }
    
    public JCheckBox getManualControlToggleCheckbox()
    {
        return manualControlToggleCheckbox;
    }
    
    public JCheckBox getManualControlFixedCheckbox()
    {
        return manualControlFixedCheckbox;
    }
    
}
