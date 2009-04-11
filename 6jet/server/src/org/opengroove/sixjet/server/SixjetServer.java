package org.opengroove.sixjet.server;

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
        String controllerBoardClassname = args[0];
        String descriptorPath = args[1];
        controllerBoard =
            (ControllerBoard) Class.forName(controllerBoardClassname).newInstance();
        descriptor = new DescriptorFile(new FileInputStream(descriptorPath));
    }
}
