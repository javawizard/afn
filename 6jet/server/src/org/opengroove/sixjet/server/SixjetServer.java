package org.opengroove.sixjet.server;

import java.io.FileInputStream;

import org.opengroove.sixjet.common.format.d.DescriptorFile;
import org.opengroove.sixjet.server.output.ControllerBoard;

public class SixjetServer
{
    public static DescriptorFile descriptor;
    
    public static ControllerBoard controllerBoard;
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        if (args.length < 2)
        {
            System.out.println("usage: java ... SixjetServer "
                + "<controller-board-class> <descriptor-file>");
            System.out.println("controller-board-class is a class that implements"
                + "org.opengroove.sixjet.server.output.ControllerBoard");
            System.out.println("descriptor-file is the path to a .6jd.txt file");
        }
        System.out.println("Loading controller board and descriptor...");
        String controllerBoardClassname = args[0];
        String descriptorPath = args[1];
        controllerBoard =
            (ControllerBoard) Class.forName(controllerBoardClassname).newInstance();
        descriptor = new DescriptorFile(new FileInputStream(descriptorPath));
        controllerBoard.init();
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
}
