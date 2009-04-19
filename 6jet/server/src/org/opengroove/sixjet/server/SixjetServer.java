package org.opengroove.sixjet.server;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Properties;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.zip.ZipInputStream;

import net.sf.opengroove.common.utils.DataUtils;

import org.opengroove.sixjet.common.com.Packet;
import org.opengroove.sixjet.common.com.packets.JetControlPacket;
import org.opengroove.sixjet.common.format.d.DescriptorFile;
import org.opengroove.sixjet.common.format.d.DescriptorFile.DescriptorFileJet;
import org.opengroove.sixjet.common.format.m.ExtractUtils;
import org.opengroove.sixjet.server.output.ControllerBoard;

public class SixjetServer
{
    public static DescriptorFile descriptor;
    
    public static ControllerBoard controllerBoard;
    
    public static Properties controllerAuthProperties = new Properties();
    
    public static Properties musicBoxAuthProperties = new Properties();
    
    public static ServerSocket controllerServerSocket;
    
    public static ServerSocket musicServerSocket;
    
    public static DatagramSocket controllerDatagramSocket;
    
    public static Thread controllerServerThread;
    
    public static HashMap<String, ControllerHandler> controllerConnectionMap =
        new HashMap<String, ControllerHandler>();
    
    public static Thread musicServerThread;
    
    public static ThreadPoolExecutor tasks =
        new ThreadPoolExecutor(5, 20, 1, TimeUnit.MINUTES,
            new ArrayBlockingQueue<Runnable>(800));
    
    public static File authFolder;
    public static File musicFilesFolder;
    public static File musicFoldersFolder;
    public static File playlistsFolder;
    public static File schedulesFolder;
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        if (args.length < 3)
        {
            System.out.println("usage: java ... SixjetServer "
                + "<controller-board-class> <descriptor-file> <storage-folder>");
            System.out.println("controller-board-class is a class that implements"
                + "org.opengroove.sixjet.server.output.ControllerBoard");
            System.out.println("descriptor-file is the path to a .6jd.txt file");
            System.out.println("storage-folder is the storage folder, "
                + "which will be created if needed");
            return;
        }
        tasks.allowCoreThreadTimeOut(true);
        System.out.println("Loading controller board and descriptor...");
        String controllerBoardClassname = args[0];
        String descriptorPath = args[1];
        String storageFolderPath = args[2];
        File storageFolder = new File(storageFolderPath);
        authFolder = new File(storageFolder, "auth");
        musicFilesFolder = new File(storageFolder, "musicfiles");
        musicFoldersFolder = new File(storageFolder, "musicfolders");
        playlistsFolder = new File(storageFolder, "playlists");
        schedulesFolder = new File(storageFolder, "schedules");
        authFolder.mkdirs();
        musicFilesFolder.mkdirs();
        musicFoldersFolder.mkdirs();
        playlistsFolder.mkdirs();
        schedulesFolder.mkdirs();
        controllerBoard =
            (ControllerBoard) Class.forName(controllerBoardClassname).newInstance();
        descriptor = new DescriptorFile(new FileInputStream(descriptorPath));
        controllerBoard.init();
        controllerAuthProperties.load(new FileInputStream(new File(authFolder,
            "controllers.6ja.properties")));
        musicBoxAuthProperties.load(new FileInputStream(new File(authFolder,
            "musicboxes.6ja.properties")));
        System.out.println("Regenerating music folders...");
        regenerateMusicFolders();
        System.out.println("Resetting jets...");
        for (DescriptorFileJet jet : descriptor.getJets())
        {
            controllerBoard.setJetState(jet.number, false);
        }
        controllerBoard.flush();
        System.out.println("Starting controller server...");
        startControllerServer();
        System.out.println("Starting music server...");
        startMusicServer();
        System.out.println("Starting download server...");
        startDownloadServer();
        System.out.println("Starting upload server...");
        startUploadServer();
        System.out.println("6jet Server has successfully started up.");
        /*
         * TODO 2009.04.11: add stuff to listen for connections, start the music
         * playing thread (which checks a minimum of once every second for music
         * to play and for whether it's supposed to stop playing music during
         * playback), remember to send a descriptor file packet to controllers
         * right after they connect, broadcast most ui events, have a command
         * handler class or something (or maybe just a bunch of if/else
         * statements for different command types), add a method for
         * multicasting to all connected controllers (including the sender)
         */
    }
    
    protected static boolean isRunning = true;
    
    private static Thread controllerDatagramThread;
    
    private static void startControllerServer() throws IOException
    {
        controllerServerSocket = new ServerSocket(56538);
        controllerServerThread = new Thread()
        {
            public void run()
            {
                while (isRunning)
                {
                    try
                    {
                        Socket s = controllerServerSocket.accept();
                        ControllerHandler handler = new ControllerHandler(s);
                        handler.start();
                    }
                    catch (Exception exception)
                    {
                        exception.printStackTrace();
                    }
                }
            }
        };
        controllerServerThread.start();
        controllerDatagramSocket = new DatagramSocket(56538);
        byte[] datagramBuffer = new byte[32768];
        final DatagramPacket datagram =
            new DatagramPacket(datagramBuffer, datagramBuffer.length);
        controllerDatagramThread = new Thread()
        {
            public void run()
            {
                while (isRunning)
                {
                    try
                    {
                        
                        controllerDatagramSocket.receive(datagram);
                    }
                    catch (Exception exception)
                    {
                        exception.printStackTrace();
                    }
                }
            }
        };
        controllerDatagramThread.start();
    }
    
    private static void startMusicServer()
    {
        // TODO Auto-generated method stub
        
    }
    
    private static void startDownloadServer()
    {
        // TODO Auto-generated method stub
        
    }
    
    private static void startUploadServer()
    {
        // TODO Auto-generated method stub
        
    }
    
    private static void regenerateMusicFolders() throws IOException
    {
        File[] files = musicFilesFolder.listFiles();
        for (int i = 0; i < files.length; i++)
        {
            File file = files[i];
            if (!file.getName().endsWith(".6jm.zip"))
            {
                System.out.println("warning: file " + file.getName()
                    + " is in the music folder but does not end with "
                    + ".6jm.zip. This file will be skipped.");
                continue;
            }
            File target =
                new File(musicFoldersFolder, file.getName().substring(0,
                    file.getName().length() - ".6jm.zip".length()));
            if (target.exists())
                DataUtils.recursiveDelete(target);
            target.mkdirs();
            System.out.println("Extracting " + file.getName());
            ExtractUtils.extract(file, target);
        }
        
    }
    
    public static void extractZipFile(File zipFile, File targetFolder)
        throws IOException
    {
        ZipInputStream in = new ZipInputStream(new FileInputStream(zipFile));
    }
    
    /**
     * Sends the packet specified to all connected and authenticated
     * controllers. This method does not block while the packet is sent.
     * 
     * @param packet
     *            The packet to send
     */
    public static void controllerBroadcast(final Packet packet)
    {
        tasks.execute(new Runnable()
        {
            public void run()
            {
                ArrayList<ControllerHandler> handlers;
                synchronized (controllerConnectionMap)
                {
                    handlers =
                        new ArrayList<ControllerHandler>(controllerConnectionMap
                            .values());
                }
                for (ControllerHandler handler : handlers)
                {
                    handler.trySend(packet);
                }
            }
        });
    }
    
    /**
     * A partly-synchronous version of {@link #controllerBroadcast(Packet)}.
     * This takes the current user list and sends the packet to them sometime in
     * the future. controllerBroadcast obtains the user list in the future.
     * 
     * @param packet
     *            The packet to send
     */
    public static void synchronousControllerBroadcast(final Packet packet)
    {
        ArrayList<ControllerHandler> handlers;
        synchronized (controllerConnectionMap)
        {
            handlers =
                new ArrayList<ControllerHandler>(controllerConnectionMap.values());
        }
        for (ControllerHandler handler : handlers)
        {
            handler.trySend(packet);
        }
    }
    
    /**
     * Sets the specified jet to the specified state, broadcasting this to all
     * connected clients in the process. This method, however, does not flush
     * the controller board.
     * 
     * @param jet
     *            The jet, as defined by the descriptor, to change
     * @param state
     *            The new state for the jet, true for on, false for off
     */
    public static void setJetState(int jet, boolean state)
    {
        controllerBoard.setJetState(jet, state);
        JetControlPacket packet = new JetControlPacket(jet, state);
        controllerBroadcast(packet);
    }
    
    /**
     * Flushes the controller board.
     */
    public static void flushBoard()
    {
        controllerBoard.flush();
    }
    
    public static boolean getJetState(int number)
    {
        return controllerBoard.getJetState(number);
    }
}
