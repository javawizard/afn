package tests;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import org.opengroove.sixjet.common.com.Packet;

public class Test01
{
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        System.out.println("creating folders and doing write");
        File file = File.createTempFile("test", "test");
        ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(file));
        for (int i = 0; i < 50000; i++)
        {
            out.writeObject(new Packet());
        }
        out.flush();
        out.close();
        System.out.println("doing read");
        ObjectInputStream in = new ObjectInputStream(new FileInputStream(file));
        for (int i = 0; i < 50000; i++)
        {
            Packet packet = (Packet) in.readObject();
        }
        in.close();
        System.out.println("deleting file");
        file.delete();
        System.out.println("done.");
    }
    
}
