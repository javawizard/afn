package com.googlecode.jwutils.mailnotify;

import java.awt.Color;
import java.awt.Desktop;
import java.awt.Graphics2D;
import java.awt.MenuItem;
import java.awt.PopupMenu;
import java.awt.SystemTray;
import java.awt.TrayIcon;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import javax.imageio.ImageIO;
import javax.mail.Flags;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Store;
import javax.mail.Flags.Flag;
import javax.mail.search.FlagTerm;

import net.sf.opengroove.common.utils.StringUtils;

import static java.awt.image.BufferedImage.TYPE_INT_ARGB;

public class MailNotifier
{
    public static class InboxActionListener implements ActionListener
    {
        
        @Override
        public void actionPerformed(ActionEvent e)
        {
            try
            {
                Desktop.getDesktop().browse(new URI("http://mail.trivergia.com"));
            }
            catch (IOException e1)
            {
                e1.printStackTrace();
            }
            catch (URISyntaxException e1)
            {
                e1.printStackTrace();
            }
        }
        
    }
    
    public static class ExitActionListener implements ActionListener
    {
        
        @Override
        public void actionPerformed(ActionEvent e)
        {
            try
            {
                SystemTray.getSystemTray().remove(trayIcon);
            }
            catch (Exception ex)
            {
                ex.printStackTrace();
            }
            System.exit(0);
        }
        
    }
    
    public static class RefreshActionListener implements ActionListener
    {
        
        @Override
        public void actionPerformed(ActionEvent e)
        {
            System.out.println(checkQueue.offer(new Object()));
        }
        
    }
    
    private static final long CHECK_DELAY = 60 * 1000;
    public static volatile boolean delayOnce = false;
    public static Color backgroundColor = new Color(239, 235, 231);
    public static BufferedImage normalImage;
    public static BufferedImage activeImage;
    public static BufferedImage offlineImage;
    public static TrayIcon trayIcon;
    private static int backgroundColorAverage = average(backgroundColor.getRed(),
            backgroundColor.getGreen(), backgroundColor.getBlue());
    public static volatile int rotation = 0;
    public static BlockingQueue<Object> checkQueue = new LinkedBlockingQueue<Object>(100);
    
    public static enum State
    {
        offline, normal, active
    }
    
    public static volatile State state = State.offline;
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        /*
         * Set up images
         */
        normalImage = ImageIO.read(MailNotifier.class
                .getResourceAsStream("gmail-icons-normal.png"));
        System.out.println("Image is " + normalImage.getWidth() + "x"
                + normalImage.getHeight());
        offlineImage = grayscale(normalImage);
        normalImage = center(normalImage, 24, 22);
        offlineImage = center(offlineImage, 24, 22);
        activeImage = new BufferedImage(normalImage.getWidth(), normalImage.getHeight(),
                TYPE_INT_ARGB);
        Graphics2D g = activeImage.createGraphics();
        g.setColor(backgroundColor);
        g.fillRect(0, 0, activeImage.getWidth(), activeImage.getHeight());
        PopupMenu menu = new PopupMenu();
        MenuItem refreshItem = new MenuItem("Refresh");
        MenuItem exitItem = new MenuItem("Exit");
        MenuItem inboxItem = new MenuItem("Inbox");
        refreshItem.addActionListener(new RefreshActionListener());
        exitItem.addActionListener(new ExitActionListener());
        inboxItem.addActionListener(new InboxActionListener());
        menu.add(refreshItem);
        menu.add(exitItem);
        menu.add(inboxItem);
        trayIcon = new TrayIcon(offlineImage, "MailNotifier", menu);
        trayIcon.addActionListener(new InboxActionListener());
        SystemTray.getSystemTray().add(trayIcon);
        /*
         * Start the image updater thread
         */
        new Thread()
        {
            public void run()
            {
                while (true)
                {
                    try
                    {
                        doImageLoop();
                    }
                    catch (Exception e)
                    {
                        e.printStackTrace();
                    }
                }
            }
        }.start();
        /*
         * Now we do the main connect loop
         */
        try
        {
            doConnectLoop();
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        while (true)
        {
            try
            {
                checkQueue.poll(CHECK_DELAY, TimeUnit.MILLISECONDS);
                doConnectLoop();
            }
            catch (Exception e)
            {
                e.printStackTrace();
                state = State.offline;
            }
        }
    }
    
    private static void doConnectLoop() throws MessagingException
    {
        long time = System.currentTimeMillis();
        String host = "imap.gmail.com";
        String username = StringUtils.readFile(new File("storage/username"));
        String password = StringUtils.readFile(new File("storage/password"));
        
        // Create empty properties
        Properties props = new Properties();
        final String SSL_FACTORY = "javax.net.ssl.SSLSocketFactory";
        // IMAP provider
        props.setProperty("mail.imap.socketFactory.class", SSL_FACTORY);
        // Get session
        Session session = Session.getDefaultInstance(props, null);
        
        // Get the store
        Store store = session.getStore("imap");
        System.out.println("connecting");
        store.connect(host, 993, username, password);
        // Get folder
        Folder folder = store.getFolder("INBOX");
        folder.open(Folder.READ_ONLY);
        if (folder.getUnreadMessageCount() > 0)
            state = State.active;
        else
            state = State.normal;
        // Get directory
        // Message[] messages = folder.search(new FlagTerm(new Flags(Flag.SEEN), false));
        //        
        // for (Message m : messages)
        // {
        // System.out.println("From " + m.getFrom()[0] + ": " + m.getSubject());
        // }
        
        // Close connection
        folder.close(false);
        store.close();
        System.out.println("Took " + (System.currentTimeMillis() - time) + "ms");
    }
    
    public static void doImageLoop() throws InterruptedException
    {
        if (state == State.offline || state == State.normal)
            Thread.sleep(5000);
        else
            Thread.sleep(700);
        rotation++;
        rotation %= 2;
        if (state == State.offline)
            setImage(offlineImage);
        else if (state == State.normal || rotation == 0)
            setImage(normalImage);
        else
            setImage(activeImage);
    }
    
    private static void setImage(BufferedImage image)
    {
        if (!image.equals(trayIcon.getImage()))
            trayIcon.setImage(image);
    }
    
    private static BufferedImage center(BufferedImage image, int width, int height)
    {
        BufferedImage result = new BufferedImage(width, height, TYPE_INT_ARGB);
        Graphics2D g = result.createGraphics();
        g.setColor(backgroundColor);
        g.fillRect(0, 0, width, height);
        int halfWidthSource = image.getWidth() / 2;
        int halfHeightSource = image.getHeight() / 2;
        int halfWidthTarget = width / 2;
        int halfHeightTarget = height / 2;
        g.drawImage(image, halfWidthTarget - halfWidthSource, halfHeightTarget
                - halfHeightSource, null);
        // g.setColor(Color.black);
        // g.drawRect(0, 0, width - 2, height - 3);
        return result;
    }
    
    public static BufferedImage grayscale(BufferedImage image)
    {
        BufferedImage result = new BufferedImage(image.getWidth(), image.getHeight(),
                TYPE_INT_ARGB);
        for (int x = 0; x < image.getWidth(); x++)
        {
            for (int y = 0; y < image.getHeight(); y++)
            {
                int rgb = image.getRGB(x, y);
                int alpha = rgb & 0xFF000000;
                rgb = average((rgb & 0xFF0000) >> 16, (rgb & 0xFF00) >> 8, rgb & 0xFF);
                rgb = average(rgb, rgb, rgb, rgb, rgb, backgroundColorAverage,
                        backgroundColorAverage, 255);
                rgb = rgb | (rgb << 8) | (rgb << 16);
                result.setRGB(x, y, alpha | rgb);
            }
        }
        return result;
    }
    
    public static int average(int... numbers)
    {
        int value = 0;
        for (int i : numbers)
        {
            value += i;
        }
        return value / numbers.length;
    }
    
}
