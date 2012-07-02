package org.opengroove.sixjet.common.ui;

import info.clearthought.layout.TableLayout;
import java.awt.BorderLayout;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;

import javax.swing.WindowConstants;
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
public class LoginFrame extends javax.swing.JFrame
{
    private JPanel mainPanel;
    private JPasswordField passwordField;
    private JPanel jPanel1;
    private JLabel serverLabel;
    private JTextField serverField;
    private JTextField usernameField;
    private JButton loginButton;
    private JLabel passwordLabel;
    private JLabel usernameLabel;
    
    /**
     * Auto-generated main method to display this JFrame
     */
    public static void main(String[] args)
    {
        SwingUtilities.invokeLater(new Runnable()
        {
            public void run()
            {
                LoginFrame inst = new LoginFrame();
                inst.setLocationRelativeTo(null);
                inst.setVisible(true);
            }
        });
    }
    
    public LoginFrame()
    {
        super();
        initGUI();
    }
    
    private void initGUI()
    {
        try
        {
            setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
            this.setTitle("6jet");
            {
                jPanel1 = new JPanel();
                getContentPane().add(jPanel1, BorderLayout.CENTER);
                {
                    mainPanel = new JPanel();
                    jPanel1.add(mainPanel);
                    TableLayout mainPanelLayout =
                        new TableLayout(new double[][] {
                            { TableLayout.PREFERRED, TableLayout.FILL,
                                TableLayout.PREFERRED },
                            { TableLayout.PREFERRED, TableLayout.PREFERRED,
                                TableLayout.PREFERRED, TableLayout.FILL,
                                TableLayout.PREFERRED } });
                    mainPanelLayout.setHGap(5);
                    mainPanelLayout.setVGap(5);
                    mainPanel.setLayout(mainPanelLayout);
                    mainPanel.setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 1));
                    {
                        usernameLabel = new JLabel();
                        mainPanel.add(usernameLabel, "0, 1");
                        usernameLabel.setText("Username:");
                    }
                    {
                        passwordLabel = new JLabel();
                        mainPanel.add(passwordLabel, "0, 2");
                        passwordLabel.setText("Password:");
                    }
                    {
                        loginButton = new JButton();
                        mainPanel.add(loginButton, "2, 4");
                        loginButton.setText("Log in");
                        loginButton.setToolTipText("<html>Click here to log in. You must be connected to the internet before you click this button. <br/>If you don't want to log in anymore, just close the window.");
                    }
                    {
                        usernameField = new JTextField();
                        mainPanel.add(usernameField, "1, 1, 2, 1");
                        usernameField.setColumns(12);
                        usernameField
                            .setToolTipText("The username to log in as. For example, \"alexlaptop\" or \"kitchen\".");
                    }
                    {
                        passwordField = new JPasswordField();
                        mainPanel.add(passwordField, "1, 2, 2, 2");
                        passwordField.setColumns(12);
                        passwordField
                            .setToolTipText("The password to log in as. For example, \"1234\".");
                    }
                    {
                        serverLabel = new JLabel();
                        mainPanel.add(serverLabel, "0, 0");
                        serverLabel.setText("Server:");
                    }
                    {
                        serverField = new JTextField();
                        mainPanel.add(getServerField(), "1,0,2,0");
                        serverField.setText("localhost");
                        serverField.setColumns(12);
                        serverField
                            .setToolTipText("The server to connect to. To connect to Alex's 6jet server, use 192.168.1.61 for this field.");
                    }
                }
            }
            pack();
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }
    
    public JButton getLoginButton()
    {
        return loginButton;
    }
    
    public JTextField getUsernameField()
    {
        return usernameField;
    }
    
    public JPasswordField getPasswordField()
    {
        return passwordField;
    }
    
    public JTextField getServerField()
    {
        return serverField;
    }
    
}
