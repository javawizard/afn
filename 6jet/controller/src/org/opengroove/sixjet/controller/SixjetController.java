package org.opengroove.sixjet.controller;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;

import javax.swing.JOptionPane;

import org.opengroove.sixjet.common.com.packets.setup.LoginPacket;
import org.opengroove.sixjet.common.com.packets.setup.LoginResponse;
import org.opengroove.sixjet.common.ui.LoginFrame;
import org.opengroove.sixjet.controller.ui.frames.MainFrame;

public class SixjetController
{
    public static MainFrame mainFrame;
    
    public static LoginFrame loginFrame;
    
    public static Socket socket;
    
    public static ObjectInputStream inStream;
    
    public static ObjectOutputStream outStream;
    
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
            loginFrame.getServerField().setEnabled(true);
            loginFrame.getUsernameField().setEnabled(true);
            loginFrame.getPasswordField().setEnabled(true);
            loginFrame.getLoginButton().setEnabled(true);
            JOptionPane.showMessageDialog(loginFrame,
                "<html>An error has occured while logging in:<br/>");
            e.printStackTrace();
        }
    }
    
    private static void setupController()
    {
        // TODO Auto-generated method stub
        
    }
}
