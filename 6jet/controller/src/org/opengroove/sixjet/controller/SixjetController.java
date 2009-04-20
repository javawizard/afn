package org.opengroove.sixjet.controller;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.ByteArrayInputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.net.ConnectException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.Socket;
import java.text.SimpleDateFormat;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.concurrent.ArrayBlockingQueue;

import javax.swing.JOptionPane;

import org.opengroove.sixjet.common.com.Packet;
import org.opengroove.sixjet.common.com.PacketSpooler;
import org.opengroove.sixjet.common.com.StopPacket;
import org.opengroove.sixjet.common.com.packets.ChatMessage;
import org.opengroove.sixjet.common.com.packets.JetControlPacket;
import org.opengroove.sixjet.common.com.packets.ServerChatMessage;
import org.opengroove.sixjet.common.com.packets.setup.DescriptorFilePacket;
import org.opengroove.sixjet.common.com.packets.setup.LoginPacket;
import org.opengroove.sixjet.common.com.packets.setup.LoginResponse;
import org.opengroove.sixjet.common.format.d.DescriptorFile;
import org.opengroove.sixjet.common.ui.JetDisplayComponent;
import org.opengroove.sixjet.common.ui.JetDisplayListener;
import org.opengroove.sixjet.common.ui.LoginFrame;
import org.opengroove.sixjet.controller.ui.frames.MainFrame;
import org.opengroove.sixjet.server.ControllerHandler;

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
    
    public static DatagramSocket datagramSocket;
    
    public static LinkedHashSet<String> processedPacketIds =
        new LinkedHashSet<String>();
    
    private static long authToken;
    
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
        String fixedServer = System.getenv("SIXJC_SERVER");
        String fixedUsername = System.getenv("SIXJC_USERNAME");
        String fixedPassword = System.getenv("SIXJC_PASSWORD");
        if (fixedUsername != null && fixedPassword != null && fixedServer != null)
        {
            loginFrame.getUsernameField().setText(fixedUsername);
            loginFrame.getPasswordField().setText(fixedPassword);
            loginFrame.getServerField().setText(fixedServer);
            attemptLogin();
        }
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
            OutputStream oo = new FilterOutputStream(socket.getOutputStream())
            {
                
                public void flush() throws IOException
                {
                    System.out.println("flushing");
                    super.flush();
                    System.out.println("flushed");
                }
            };
            outStream = new ObjectOutputStream(oo);
            outStream.writeObject(new LoginPacket(username, password));
            LoginResponse response = (LoginResponse) inStream.readObject();
            if (response.isSuccessful())
            {
                authToken = response.getToken();
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
        mainFrame.setDefaultCloseOperation(mainFrame.EXIT_ON_CLOSE);
        mainFrame.setLocationRelativeTo(null);
        mainFrame.show();
        jetDisplay = new JetDisplayComponent(jetDescriptor);
        addJetDisplayListener();
        mainFrame.getJetDisplayPanel().add(jetDisplay);
        spooler = new PacketSpooler(outStream, null, null, 0, 200);
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
                System.out.println("jet " + jet + " down");
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
                System.out.println("jet " + jet + " up");
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
    
    protected static ArrayBlockingQueue<Packet> packetsToProcess =
        new ArrayBlockingQueue(300);
    
    private static void startReceivingThread()
    {
        try
        {
            datagramSocket = new DatagramSocket(56538 + 10);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        final byte[] datagramBuffer = new byte[32768];
        final DatagramPacket datagram =
            new DatagramPacket(datagramBuffer, datagramBuffer.length);
        new Thread()
        {
            public void run()
            {
                while (true)
                {
                    try
                    {
                        datagramSocket.receive(datagram);
                        if (datagram.getLength() > datagramBuffer.length)
                        {
                            System.err
                                .println("Datagram received that is too large ("
                                    + datagram.getLength()
                                    + " bytes). It will be dropped.");
                            continue;
                        }
                        System.out.println("receiving datagram of length "
                            + datagram.getLength() + " from address "
                            + datagram.getAddress());
                        ByteArrayInputStream byteIn =
                            new ByteArrayInputStream(datagramBuffer, 0, datagram
                                .getLength());
                        ObjectInputStream objectIn = new ObjectInputStream(byteIn);
                        Packet packet = (Packet) objectIn.readObject();
                        if (validateAndAdd(packet.getPacketId()))
                            packetsToProcess.add(packet);
                    }
                    catch (Exception exception)
                    {
                        exception.printStackTrace();
                    }
                }
            }
        }.start();
        new Thread()
        {
            public void run()
            {
                try
                {
                    while (true)
                    {
                        Packet packet = (Packet) inStream.readObject();
                        String packetId = packet.getPacketId();
                        boolean shouldProcess = validateAndAdd(packetId);
                        if (shouldProcess)
                            packetsToProcess.add(packet);
                    }
                }
                catch (Exception exception)
                {
                    if (exception instanceof IllegalStateException)
                    {
                        System.err.println("Processing queue backed up; server "
                            + "is sending commands too fast");
                    }
                    exception.printStackTrace();
                }
                try
                {
                    packetsToProcess.put(new StopPacket());
                }
                catch (InterruptedException e)
                {
                    e.printStackTrace();
                    System.err
                        .println("InterruptedException encountered while trying to stop "
                            + "processor thread. Restart 6jet Controller immediately.");
                }
            }
        }.start();
        new Thread()
        {
            public void run()
            {
                try
                {
                    while (mainFrame.isShowing())
                    {
                        Packet packet = (Packet) packetsToProcess.take();
                        if (packet instanceof StopPacket)
                            throw new Exception("Terminated on stop packet");
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
                    Thread.sleep(1000);
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
        else if (packet instanceof ServerChatMessage)
            processServerChatMessage((ServerChatMessage) packet);
    }
    
    private static void processServerChatMessage(ServerChatMessage packet)
    {
        mainFrame.getChatTextArea().append(
            "(" + new SimpleDateFormat("M/dd h:mmaa").format(packet.getWhen()) + ")");
        mainFrame.getChatTextArea().append(" " + packet.getFrom());
        String text = packet.getMessage();
        boolean isMe = text.startsWith("/me ");
        if (isMe)
        {
            text = text.substring("/me ".length());
        }
        else
        {
            mainFrame.getChatTextArea().append(":");
        }
        mainFrame.getChatTextArea().append(" " + text + "\n\n");
        mainFrame.getChatTextArea().setCaretPosition(
            mainFrame.getChatTextArea().getDocument().getLength());
    }
    
    private static void processJetControlPacket(JetControlPacket packet)
    {
        /*
         * For now, all we do is show this on the screen.
         */
        jetDisplay.setState(packet.getJet(), packet.isState());
    }
    
    public static void chatTextFieldActionPerformed()
    {
        String text = mainFrame.getChatTextField().getText();
        if (text.trim().equals(""))
            return;
        ChatMessage message = new ChatMessage(text);
        send(message);
        mainFrame.getChatTextField().setText("");
    }
    
    /**
     * Validates that the packet specified is not in the processed packets list.
     * If it is not in the list, then it is added, and the list is trimmed back
     * to the maximum list size (currently 300).
     * 
     * @param packetId
     *            The id of the packet to check
     * @return True if this packet has not yet been processed, and should
     *         therefore be processed, or false if the packet has already been
     *         processed, and should not be processed again
     */
    public static boolean validateAndAdd(String packetId)
    {
        synchronized (processedPacketIds)
        {
            boolean alreadyReceived = processedPacketIds.contains(packetId);
            if (alreadyReceived)
                return false;
            /*
             * The packet is new. We'll add it, trim the list, and return true.
             */
            processedPacketIds.add(packetId);
            /*
             * numOver is the number of elements in the list in excess of the
             * maximum number (300)
             */
            int numOver = processedPacketIds.size() - 300;
            if (numOver > 0)
            {
                Iterator iter = processedPacketIds.iterator();
                for (int i = 0; i < numOver; i++)
                {
                    iter.next();
                    iter.remove();
                }
            }
            return true;
        }
    }
    
}
