package org.opengroove.sixjet.controller.ui.frames;

import info.clearthought.layout.TableLayout;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BoxLayout;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.ToolTipManager;

import javax.swing.WindowConstants;
import org.opengroove.sixjet.common.ui.HeaderComponent;
import org.opengroove.sixjet.controller.SixjetController;

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
    private JButton playlistRenameButton;
    private JButton aboutButton;
    private JScrollPane jScrollPane6;
    private JPanel jetDisplayPanel;
    private JButton nowPlayingNextButton;
    private JButton nowPlayingCenterButton;
    private JButton nowPlayingStopButton;
    private JButton nowPlayingPauseButton;
    private JButton nowPlayingRestartButton;
    private JButton nowPlayingPreviousButton;
    private JPanel nowPlayingSouth;
    private JScrollPane jScrollPane5;
    private JList musicList;
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
        ToolTipManager.sharedInstance().setDismissDelay(15 * 1000);
        initGUI();
    }
    
    private void initGUI()
    {
        try
        {
            setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
            this.setTitle("6jet Controller");
            {
                outerPanel = new JPanel();
                BorderLayout outerPanelLayout = new BorderLayout();
                getContentPane().add(outerPanel, BorderLayout.CENTER);
                outerPanel.setLayout(outerPanelLayout);
                {
                    mainSplit = new JSplitPane();
                    outerPanel.add(mainSplit, BorderLayout.CENTER);
                    mainSplit.setOrientation(JSplitPane.VERTICAL_SPLIT);
                    mainSplit.setDividerLocation(500);
                    mainSplit.setResizeWeight(1.0);
                    mainSplit.setContinuousLayout(true);
                    mainSplit.setOneTouchExpandable(true);
                    {
                        musicPlaybackSplit = new JSplitPane();
                        mainSplit.add(musicPlaybackSplit, JSplitPane.BOTTOM);
                        musicPlaybackSplit.setContinuousLayout(true);
                        musicPlaybackSplit.setDividerLocation(356);
                        musicPlaybackSplit.setResizeWeight(0.5);
                        musicPlaybackSplit.setOneTouchExpandable(true);
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
                                    musicUploadButton
                                        .setToolTipText("<html>Allows you to upload a new music file. You will be asked for the file that you want to upload. <br/>You can also upload a file directly from within 6jet Music.");
                                    musicUploadButton
                                        .addActionListener(new ActionListener()
                                        {
                                            public void actionPerformed(ActionEvent evt)
                                            {
                                                musicUploadButtonActionPerformed(evt);
                                            }
                                        });
                                }
                                {
                                    musicPlayButton = new JButton();
                                    musicSouth.add(getMusicPlayButton());
                                    musicPlayButton.setText("Play");
                                    musicPlayButton.setMargin(new java.awt.Insets(0, 0,
                                        0, 0));
                                    musicPlayButton
                                        .setToolTipText("Plays the selected music, stopping any playlist or music that might be currently playing.");
                                    musicPlayButton
                                        .addActionListener(new ActionListener()
                                        {
                                            public void actionPerformed(ActionEvent evt)
                                            {
                                                musicPlayButtonActionPerformed(evt);
                                            }
                                        });
                                }
                                {
                                    musicDeleteButton = new JButton();
                                    musicSouth.add(getMusicDeleteButton());
                                    musicDeleteButton.setText("Delete");
                                    musicDeleteButton.setMargin(new java.awt.Insets(0,
                                        0, 0, 0));
                                    musicDeleteButton
                                        .setToolTipText("Deletes the selected music. You will be asked to confirm that you really want to delete the selected music.");
                                    musicDeleteButton
                                        .addActionListener(new ActionListener()
                                        {
                                            public void actionPerformed(ActionEvent evt)
                                            {
                                                musicDeleteButtonActionPerformed(evt);
                                            }
                                        });
                                }
                                {
                                    musicDownloadButton = new JButton();
                                    musicSouth.add(getMusicDownloadButton());
                                    musicDownloadButton.setText("Download");
                                    musicDownloadButton.setMargin(new java.awt.Insets(
                                        0, 0, 0, 0));
                                    musicDownloadButton
                                        .setToolTipText("<html>Downloads the selected music. This will open a web browser window, which will ask you to download the file. <br/>This file can then be edited using 6jet Music.");
                                    musicDownloadButton
                                        .addActionListener(new ActionListener()
                                        {
                                            public void actionPerformed(ActionEvent evt)
                                            {
                                                musicDownloadButtonActionPerformed(evt);
                                            }
                                        });
                                }
                            }
                            {
                                jScrollPane5 = new JScrollPane();
                                musicPanel.add(jScrollPane5, BorderLayout.CENTER);
                                jScrollPane5.setPreferredSize(new java.awt.Dimension(
                                    335, 172));
                                {
                                    ListModel musicListModel =
                                        new DefaultComboBoxModel(new String[] {
                                             });
                                    musicList = new JList();
                                    jScrollPane5.setViewportView(getMusicList());
                                    musicList.setModel(musicListModel);
                                    musicList
                                        .setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
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
                            {
                                nowPlayingSouth = new JPanel();
                                GridLayout nowPlayingSouthLayout = new GridLayout(1, 1);
                                nowPlayingPanel
                                    .add(nowPlayingSouth, BorderLayout.SOUTH);
                                nowPlayingSouth.setLayout(nowPlayingSouthLayout);
                                {
                                    nowPlayingPreviousButton = new JButton();
                                    nowPlayingSouth.add(getNowPlayingPreviousButton());
                                    nowPlayingPreviousButton.setText("<<");
                                    nowPlayingPreviousButton
                                        .setMargin(new java.awt.Insets(0, 0, 0, 0));
                                    nowPlayingPreviousButton
                                        .setToolTipText("Skips to the previous song in the playlist, if a playlist is currently playing.");
                                    nowPlayingPreviousButton
                                        .addActionListener(new ActionListener()
                                        {
                                            public void actionPerformed(ActionEvent evt)
                                            {
                                                nowPlayingPreviousButtonActionPerformed(evt);
                                            }
                                        });
                                }
                                {
                                    nowPlayingRestartButton = new JButton();
                                    nowPlayingSouth.add(getNowPlayingRestartButton());
                                    nowPlayingRestartButton.setText("|<");
                                    nowPlayingRestartButton
                                        .setMargin(new java.awt.Insets(0, 0, 0, 0));
                                    nowPlayingRestartButton
                                        .setToolTipText("Starts the current song over from the beginning.");
                                    nowPlayingRestartButton
                                        .addActionListener(new ActionListener()
                                        {
                                            public void actionPerformed(ActionEvent evt)
                                            {
                                                nowPlayingRestartButtonActionPerformed(evt);
                                            }
                                        });
                                }
                                {
                                    nowPlayingPauseButton = new JButton();
                                    nowPlayingSouth.add(getNowPlayingPauseButton());
                                    nowPlayingPauseButton.setText("| |");
                                    nowPlayingPauseButton
                                        .setMargin(new java.awt.Insets(0, 0, 0, 0));
                                    nowPlayingPauseButton
                                        .setToolTipText("Pauses or unpauses music playback. Jets can be manually controlled while playback is paused.");
                                    nowPlayingPauseButton
                                        .addActionListener(new ActionListener()
                                        {
                                            public void actionPerformed(ActionEvent evt)
                                            {
                                                nowPlayingPauseButtonActionPerformed(evt);
                                            }
                                        });
                                }
                                {
                                    nowPlayingStopButton = new JButton();
                                    nowPlayingSouth.add(getNowPlayingStopButton());
                                    nowPlayingStopButton.setText("<html>&#9744;");
                                    nowPlayingStopButton.setMargin(new java.awt.Insets(
                                        0, 0, 0, 0));
                                    nowPlayingStopButton
                                        .setToolTipText("Stops music playback.");
                                    nowPlayingStopButton
                                        .addActionListener(new ActionListener()
                                        {
                                            public void actionPerformed(ActionEvent evt)
                                            {
                                                nowPlayingStopButtonActionPerformed(evt);
                                            }
                                        });
                                }
                                {
                                    nowPlayingCenterButton = new JButton();
                                    nowPlayingSouth.add(getNowPlayingCenterButton());
                                    nowPlayingCenterButton.setText(">|<");
                                    nowPlayingCenterButton
                                        .setMargin(new java.awt.Insets(0, 0, 0, 0));
                                    nowPlayingCenterButton
                                        .setToolTipText("Centers the jet pattern view over the current position in the song.");
                                    nowPlayingCenterButton
                                        .addActionListener(new ActionListener()
                                        {
                                            public void actionPerformed(ActionEvent evt)
                                            {
                                                nowPlayingCenterButtonActionPerformed(evt);
                                            }
                                        });
                                }
                                {
                                    nowPlayingNextButton = new JButton();
                                    nowPlayingSouth.add(getNowPlayingNextButton());
                                    nowPlayingNextButton.setText(">>");
                                    nowPlayingNextButton.setMargin(new java.awt.Insets(
                                        0, 0, 0, 0));
                                    nowPlayingNextButton
                                        .setToolTipText("Skips to the next song in the playlist, if a playlist is currently playing.");
                                    nowPlayingNextButton
                                        .addActionListener(new ActionListener()
                                        {
                                            public void actionPerformed(ActionEvent evt)
                                            {
                                                nowPlayingNextButtonActionPerformed(evt);
                                            }
                                        });
                                }
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
                            topContentRightSplit.setDividerLocation(450);
                            topContentRightSplit.setContinuousLayout(true);
                            {
                                scheduleChatSplit = new JSplitPane();
                                topContentRightSplit.add(scheduleChatSplit,
                                    JSplitPane.RIGHT);
                                scheduleChatSplit
                                    .setOrientation(JSplitPane.VERTICAL_SPLIT);
                                scheduleChatSplit.setDividerLocation(220);
                                scheduleChatSplit.setContinuousLayout(true);
                                scheduleChatSplit.setResizeWeight(0.5);
                                scheduleChatSplit.setOneTouchExpandable(true);
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
                                            chatTextArea.setLineWrap(true);
                                            chatTextArea.setWrapStyleWord(true);
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
                                            chatTextField
                                                .setToolTipText("Type a chat message here, then click send.");
                                            chatTextField
                                                .addActionListener(new ActionListener()
                                                {
                                                    public void actionPerformed(
                                                        ActionEvent evt)
                                                    {
                                                        chatTextFieldActionPerformed(evt);
                                                    }
                                                });
                                        }
                                        {
                                            chatSendButton = new JButton();
                                            chatSouth.add(chatSendButton,
                                                BorderLayout.SOUTH);
                                            chatSendButton.setText("Send");
                                            chatSendButton
                                                .setMargin(new java.awt.Insets(0, 14,
                                                    0, 14));
                                            chatSendButton
                                                .setToolTipText("Sends the chat message typed above to all people who have 6jet Controller open.");
                                            chatSendButton
                                                .addActionListener(new ActionListener()
                                                {
                                                    public void actionPerformed(
                                                        ActionEvent evt)
                                                    {
                                                        chatSendButtonActionPerformed(evt);
                                                    }
                                                });
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
                                                    });
                                            scheduleList = new JList();
                                            jScrollPane2
                                                .setViewportView(getScheduleList());
                                            scheduleList.setModel(scheduleListModel);
                                            scheduleList
                                                .setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                                        }
                                    }
                                    {
                                        scheduleSouth = new JPanel();
                                        TableLayout scheduleSouthLayout =
                                            new TableLayout(new double[][] {
                                                { TableLayout.FILL, TableLayout.FILL },
                                                { TableLayout.FILL } });
                                        scheduleSouthLayout.setHGap(0);
                                        scheduleSouthLayout.setVGap(0);
                                        schedulePanel.add(scheduleSouth,
                                            BorderLayout.SOUTH);
                                        scheduleSouth.setLayout(scheduleSouthLayout);
                                        {
                                            scheduleAddButton = new JButton();
                                            scheduleSouth
                                                .add(scheduleAddButton, "0, 0");
                                            scheduleAddButton.setText("Add");
                                            scheduleAddButton
                                                .setMargin(new java.awt.Insets(0, 0, 0,
                                                    0));
                                            scheduleAddButton
                                                .setToolTipText("<html>Adds the playlist selected in the <b>Playlists</b> pane to the schedule. <br/>You will be asked what date and time the playlist should begin playing. <br/>To schedule a single piece of music, create a playlist with just that music in it, <br/>and then schedule the playlist.");
                                            scheduleAddButton
                                                .addActionListener(new ActionListener()
                                                {
                                                    public void actionPerformed(
                                                        ActionEvent evt)
                                                    {
                                                        scheduleAddButtonActionPerformed(evt);
                                                    }
                                                });
                                        }
                                        {
                                            scheduleDeleteButton = new JButton();
                                            scheduleSouth.add(scheduleDeleteButton,
                                                "1, 0");
                                            scheduleDeleteButton.setText("Delete");
                                            scheduleDeleteButton
                                                .setMargin(new java.awt.Insets(0, 0, 0,
                                                    0));
                                            scheduleDeleteButton
                                                .setToolTipText("Deletes the currently selected schedule item.");
                                            scheduleDeleteButton
                                                .addActionListener(new ActionListener()
                                                {
                                                    public void actionPerformed(
                                                        ActionEvent evt)
                                                    {
                                                        scheduleDeleteButtonActionPerformed(evt);
                                                    }
                                                });
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
                                        manualControlToggleCheckbox
                                            .setToolTipText("If checked, then clicking on a jet will toggle its state. If unchecked, clicking on a jet will simply fire it.");
                                    }
                                    {
                                        manualControlFixedCheckbox = new JCheckBox();
                                        centerPanelSouth
                                            .add(getManualControlFixedCheckbox());
                                        manualControlFixedCheckbox
                                            .setText("Fixed time");
                                        manualControlFixedCheckbox
                                            .setToolTipText("Irrelevant if toggle is checked. If this is checked, then jets will fire for a fixed amount of time when clicked. If unchecked, jets will fire for as long as the mouse is held down.");
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
                                    {
                                        aboutButton = new JButton();
                                        centerPanelSouth.add(aboutButton);
                                        aboutButton.setText("About");
                                        aboutButton.setMargin(new java.awt.Insets(0, 0,
                                            0, 0));
                                        aboutButton
                                            .addActionListener(new ActionListener()
                                            {
                                                public void actionPerformed(
                                                    ActionEvent evt)
                                                {
                                                    aboutButtonActionPerformed(evt);
                                                }
                                            });
                                    }
                                }
                                {
                                    jScrollPane6 = new JScrollPane();
                                    centerPanel.add(jScrollPane6, BorderLayout.CENTER);
                                    jScrollPane6
                                        .setPreferredSize(new java.awt.Dimension(398,
                                            342));
                                    {
                                        jetDisplayPanel = new JPanel();
                                        jScrollPane6
                                            .setViewportView(getJetDisplayPanel());
                                        BorderLayout jetDisplayPanelLayout =
                                            new BorderLayout();
                                        jetDisplayPanel
                                            .setLayout(jetDisplayPanelLayout);
                                    }
                                }
                            }
                        }
                        {
                            playlistSplit = new JSplitPane();
                            topContentLeftSplit.add(playlistSplit, JSplitPane.LEFT);
                            playlistSplit.setOrientation(JSplitPane.VERTICAL_SPLIT);
                            playlistSplit.setContinuousLayout(true);
                            playlistSplit.setDividerLocation(220);
                            playlistSplit.setResizeWeight(0.5);
                            playlistSplit.setOneTouchExpandable(true);
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
                                    TableLayout playlistsSouthLayout =
                                        new TableLayout(new double[][] {
                                            { TableLayout.FILL, TableLayout.FILL },
                                            { TableLayout.FILL, TableLayout.FILL } });
                                    playlistsPanel.add(playlistsSouth,
                                        BorderLayout.SOUTH);
                                    playlistsSouth.setLayout(playlistsSouthLayout);
                                    {
                                        playlistAddButton = new JButton();
                                        playlistsSouth.add(playlistAddButton, "0, 0");
                                        playlistAddButton.setText("Add");
                                        playlistAddButton
                                            .setMargin(new java.awt.Insets(0, 0, 0, 0));
                                        playlistAddButton
                                            .setToolTipText("Creates a new playlist. You will be asked for the name of the new playlist.");
                                        playlistAddButton
                                            .addActionListener(new ActionListener()
                                            {
                                                public void actionPerformed(
                                                    ActionEvent evt)
                                                {
                                                    playlistAddButtonActionPerformed(evt);
                                                }
                                            });
                                    }
                                    {
                                        playlistDeleteButton = new JButton();
                                        playlistsSouth
                                            .add(playlistDeleteButton, "1, 0");
                                        playlistDeleteButton.setText("Delete");
                                        playlistDeleteButton
                                            .setMargin(new java.awt.Insets(0, 0, 0, 0));
                                        playlistDeleteButton
                                            .setToolTipText("Deletes the selected playlist. You will be asked to confirm that you want to delete the selected playlist.");
                                        playlistDeleteButton
                                            .addActionListener(new ActionListener()
                                            {
                                                public void actionPerformed(
                                                    ActionEvent evt)
                                                {
                                                    playlistDeleteButtonActionPerformed(evt);
                                                }
                                            });
                                    }
                                    {
                                        playlistPlayButton = new JButton();
                                        playlistsSouth.add(playlistPlayButton, "1, 1");
                                        playlistPlayButton.setText("Play");
                                        playlistPlayButton
                                            .setMargin(new java.awt.Insets(0, 0, 0, 0));
                                        playlistPlayButton
                                            .setToolTipText("Plays the selected playlist, stopping any currently-playing music if necessary.");
                                        playlistPlayButton
                                            .addActionListener(new ActionListener()
                                            {
                                                public void actionPerformed(
                                                    ActionEvent evt)
                                                {
                                                    playlistPlayButtonActionPerformed(evt);
                                                }
                                            });
                                    }
                                    {
                                        playlistRenameButton = new JButton();
                                        playlistsSouth.add(getPlaylistRenameButton(),
                                            "0, 1");
                                        playlistRenameButton.setText("Rename");
                                        playlistRenameButton
                                            .setMargin(new java.awt.Insets(0, 0, 0, 0));
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
                                            new DefaultComboBoxModel(new String[] {});
                                        playlistList = new JList();
                                        jScrollPane3.setViewportView(getPlaylistList());
                                        playlistList.setModel(playlistListModel);
                                        playlistList
                                            .setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
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
                                        playlistDeleteItemButton
                                            .setToolTipText("Deletes the currently selected playlist item. You will be asked to confirm that you want to delete the playlist item.");
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
                                                playlistAddDelayButton
                                                    .setToolTipText("Adds a new delay to the playlist. You will be asked how long the delay should be.");
                                            }
                                            {
                                                playlistAddMusicButton = new JButton();
                                                editPlaylistAddPanel
                                                    .add(playlistAddMusicButton);
                                                playlistAddMusicButton.setText("Music");
                                                playlistAddMusicButton
                                                    .setMargin(new java.awt.Insets(0,
                                                        0, 0, 0));
                                                playlistAddMusicButton
                                                    .setToolTipText("<html>Adds the music selected in the <b>Music</b> pane to the playlist.");
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
                                                editPlaylistUpButton
                                                    .setToolTipText("Moves the selected playlist item up within the playlist.");
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
                                                editPlaylistDownButton
                                                    .setToolTipText("Moves the selected playlist item down within the playlist.");
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
                                                 });
                                        editPlaylistList = new JList();
                                        jScrollPane4
                                            .setViewportView(getEditPlaylistList());
                                        editPlaylistList
                                            .setModel(editPlaylistListModel);
                                        editPlaylistList
                                            .setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            pack();
            this.setSize(740, 750);
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
    
    public JList getMusicList()
    {
        return musicList;
    }
    
    public JButton getNowPlayingPreviousButton()
    {
        return nowPlayingPreviousButton;
    }
    
    public JButton getNowPlayingRestartButton()
    {
        return nowPlayingRestartButton;
    }
    
    public JButton getNowPlayingPauseButton()
    {
        return nowPlayingPauseButton;
    }
    
    public JButton getNowPlayingStopButton()
    {
        return nowPlayingStopButton;
    }
    
    public JButton getNowPlayingCenterButton()
    {
        return nowPlayingCenterButton;
    }
    
    public JButton getNowPlayingNextButton()
    {
        return nowPlayingNextButton;
    }
    
    private void playlistAddButtonActionPerformed(ActionEvent evt)
    {
        SixjetController.playlistAddButtonActionPerformed();
    }
    
    private void playlistDeleteButtonActionPerformed(ActionEvent evt)
    {
        SixjetController.playlistDeleteButtonActionPerformed();
    }
    
    private void playlistPlayButtonActionPerformed(ActionEvent evt)
    {
        System.out.println("playlistPlayButton.actionPerformed, event=" + evt);
        // TODO add your code for playlistPlayButton.actionPerformed
    }
    
    private void scheduleAddButtonActionPerformed(ActionEvent evt)
    {
        System.out.println("scheduleAddButton.actionPerformed, event=" + evt);
        // TODO add your code for scheduleAddButton.actionPerformed
    }
    
    private void scheduleDeleteButtonActionPerformed(ActionEvent evt)
    {
        System.out.println("scheduleDeleteButton.actionPerformed, event=" + evt);
        // TODO add your code for scheduleDeleteButton.actionPerformed
    }
    
    private void chatSendButtonActionPerformed(ActionEvent evt)
    {
        /*
         * This does the same thing as firing an event on the chat text field,
         * so we'll just call the same method.
         */
        SixjetController.chatTextFieldActionPerformed();
    }
    
    private void chatTextFieldActionPerformed(ActionEvent evt)
    {
        SixjetController.chatTextFieldActionPerformed();
    }
    
    private void musicUploadButtonActionPerformed(ActionEvent evt)
    {
        System.out.println("musicUploadButton.actionPerformed, event=" + evt);
        // TODO add your code for musicUploadButton.actionPerformed
    }
    
    private void musicPlayButtonActionPerformed(ActionEvent evt)
    {
        System.out.println("musicPlayButton.actionPerformed, event=" + evt);
        // TODO add your code for musicPlayButton.actionPerformed
    }
    
    private void musicDeleteButtonActionPerformed(ActionEvent evt)
    {
        System.out.println("musicDeleteButton.actionPerformed, event=" + evt);
        // TODO add your code for musicDeleteButton.actionPerformed
    }
    
    private void musicDownloadButtonActionPerformed(ActionEvent evt)
    {
        System.out.println("musicDownloadButton.actionPerformed, event=" + evt);
        // TODO add your code for musicDownloadButton.actionPerformed
    }
    
    private void nowPlayingPreviousButtonActionPerformed(ActionEvent evt)
    {
        System.out.println("nowPlayingPreviousButton.actionPerformed, event=" + evt);
        // TODO add your code for nowPlayingPreviousButton.actionPerformed
    }
    
    private void nowPlayingRestartButtonActionPerformed(ActionEvent evt)
    {
        System.out.println("nowPlayingRestartButton.actionPerformed, event=" + evt);
        // TODO add your code for nowPlayingRestartButton.actionPerformed
    }
    
    private void nowPlayingPauseButtonActionPerformed(ActionEvent evt)
    {
        System.out.println("nowPlayingPauseButton.actionPerformed, event=" + evt);
        // TODO add your code for nowPlayingPauseButton.actionPerformed
    }
    
    private void nowPlayingStopButtonActionPerformed(ActionEvent evt)
    {
        System.out.println("nowPlayingStopButton.actionPerformed, event=" + evt);
        // TODO add your code for nowPlayingStopButton.actionPerformed
    }
    
    private void nowPlayingCenterButtonActionPerformed(ActionEvent evt)
    {
        System.out.println("nowPlayingCenterButton.actionPerformed, event=" + evt);
        // TODO add your code for nowPlayingCenterButton.actionPerformed
    }
    
    private void nowPlayingNextButtonActionPerformed(ActionEvent evt)
    {
        System.out.println("nowPlayingNextButton.actionPerformed, event=" + evt);
        // TODO add your code for nowPlayingNextButton.actionPerformed
    }
    
    public JPanel getJetDisplayPanel()
    {
        return jetDisplayPanel;
    }
    
    private void aboutButtonActionPerformed(ActionEvent evt)
    {
        JOptionPane.showMessageDialog(this, "<html>6jet Controller<br/>"
            + "http://6jet.googlecode.com<br/>" + "&copy; 2009 Alexander Boyd<br/>"
            + "Released under the terms of the GNU Lesser General Public License");
    }
    
    public JButton getPlaylistRenameButton()
    {
        return playlistRenameButton;
    }
    
}
