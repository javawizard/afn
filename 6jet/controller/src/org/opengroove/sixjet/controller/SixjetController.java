package org.opengroove.sixjet.controller;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.ConnectException;
import java.net.Socket;

import javax.swing.JOptionPane;

import org.opengroove.sixjet.common.com.Packet;
import org.opengroove.sixjet.common.com.PacketSpooler;
import org.opengroove.sixjet.common.com.packets.JetControlPacket;
import org.opengroove.sixjet.common.com.packets.setup.DescriptorFilePacket;
import org.opengroove.sixjet.common.com.packets.setup.LoginPacket;
import org.opengroove.sixjet.common.com.packets.setup.LoginResponse;
import org.opengroove.sixjet.common.format.d.DescriptorFile;
import org.opengroove.sixjet.common.ui.JetDisplayComponent;
import org.opengroove.sixjet.common.ui.JetDisplayListener;
import org.opengroove.sixjet.common.ui.LoginFrame;
import org.opengroove.sixjet.controller.ui.frames.MainFrame;

public class SixjetController
{
    public static MainFrame mainFrame;
    
    public static LoginFrame loginFrame;
    
    public static Socket socket;
    
    public static ObjectInputStream inStream;
    
    public static ObjectOutputStream outStream;
    
    public static JetDisplayComponent jetDisplay;
    
    public static DescriptorFile jetDescriptor;
    
    private static PacketSpooler spooler;
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        loginFrame = new LoginFrame();
        loginFrame.getLoginButton().addActionListener(new ActionListener()
        {
            
            public void actionPerformed(ActionEvent e)
            {
                new Thread()
                {
                    public void run()
                    {
                        attemptLogin();
                    }
                }.start();
            }
        });
        loginFrame.setDefaultCloseOperation(loginFrame.EXIT_ON_CLOSE);
        loginFrame.setLocationRelativeTo(null);
        loginFrame.setTitle("Log in - 6jet Controller");
        loginFrame.show();
    }
    
    protected static void attemptLogin()
    {
        String server = loginFrame.getServerField().getText();
        String username = loginFrame.getUsernameField().getText();
        String password = loginFrame.getPasswordField().getText();
        try
        {
            loginFrame.getServerField().setEnabled(false);
            loginFrame.getUsernameField().setEnabled(false);
            loginFrame.getPasswordField().setEnabled(false);
            loginFrame.getLoginButton().setEnabled(false);
            socket = new Socket(server, 56538);
            inStream = new ObjectInputStream(socket.getInputStream());
            outStream = new ObjectOutputStream(socket.getOutputStream());
            outStream.writeObject(new LoginPacket(username, password));
            LoginResponse response = (LoginResponse) inStream.readObject();
            if (response.isSuccessful())
            {
                loginFrame.dispose();
                DescriptorFilePacket descriptorPacket =
                    (DescriptorFilePacket) inStream.readObject();
                jetDescriptor = descriptorPacket.getFile();
                setupController();
                return;
            }
            else
            {
                loginFrame.getServerField().setEnabled(true);
                loginFrame.getUsernameField().setEnabled(true);
                loginFrame.getPasswordField().setEnabled(true);
                loginFrame.getLoginButton().setEnabled(true);
                socket.close();
                JOptionPane.showMessageDialog(loginFrame, "Login failed: "
                    + response.getReason());
            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
            loginFrame.getServerField().setEnabled(true);
            loginFrame.getUsernameField().setEnabled(true);
            loginFrame.getPasswordField().setEnabled(true);
            loginFrame.getLoginButton().setEnabled(true);
            JOptionPane.showMessageDialog(loginFrame,
                "<html>An error has occured while logging in:<br/>" + e.getMessage()
                    + "<br/><br/>Class: " + e.getClass().getName());
            if (e instanceof ConnectException)
            {
                JOptionPane
                    .showMessageDialog(
                        loginFrame,
                        "<html>This error means that 6jet Controller couldn't connect<br/>"
                            + "to the server. This could be because the server is not running.<br/>"
                            + "Check to make sure that the server computer is running, and try again.");
            }
        }
    }
    
    private static void setupController()
    {
        mainFrame = new MainFrame();
        mainFrame.setLocationRelativeTo(null);
        mainFrame.show();
        jetDisplay = new JetDisplayComponent(jetDescriptor);
        addJetDisplayListener();
        mainFrame.getJetDisplayPanel().add(jetDisplay);
        spooler = new PacketSpooler(outStream, 200);
        spooler.start();
        startReceivingThread();
        mainFrame.invalidate();
        mainFrame.validate();
        mainFrame.repaint();
    }
    
    private static void addJetDisplayListener()
    {
        jetDisplay.addJetListener(new JetDisplayListener()
        {
            
            public void jetDown(int jet)
            {
                if (mainFrame.getManualControlToggleCheckbox().isSelected())
                {
                    send(new JetControlPacket(jet, !jetDisplay.getState(jet)));
                }
                else if (mainFrame.getManualControlFixedCheckbox().isSelected())
                {
                    
                }
                else
                {
                    send(new JetControlPacket(jet, true));
                }
            }
            
            public void jetUp(int jet, boolean in)
            {
                if (mainFrame.getManualControlToggleCheckbox().isSelected())
                {
                    
                }
                else if (mainFrame.getManualControlFixedCheckbox().isSelected())
                {
                    
                }
                else
                {
                    send(new JetControlPacket(jet, false));
                }
            }
        });
    }
    
    public static void send(Packet packet)
    {
        if (!spooler.send(packet))
            throw new RuntimeException("Couldn't send packet "
                + packet.getClass().getName() + " " + packet);
    }
    
    private static void startReceivingThread()
    {
        new Thread()
        {
            public void run()
            {
                try
                {
                    while (mainFrame.isShowing())
                    {
                        Packet packet = (Packet) inStream.readObject();
                        process(packet);
                    }
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    if (mainFrame.isShowing())
                    {
                        JOptionPane
                            .showMessageDialog(
                                mainFrame,
                                "6jet Controller has been disconnected from the"
                                    + " server. Please restart 6jet Controller, and try again.");
                    }
                }
                try
                {
                    Thread.sleep(2000);
                }
                catch (Exception exception)
                {
                    exception.printStackTrace();
                }
                System.exit(2);
            }
        }.start();
    }
    
    protected static void process(Packet packet)
    {
        if (packet instanceof JetControlPacket)
            processJetControlPacket((JetControlPacket) packet);
    }
    
    private static void processJetControlPacket(JetControlPacket packet)
    {
        /*
         * For now, all we do is show this on the screen.
         */
        jetDisplay.setState(packet.getJet(), packet.isState());
    }
}
