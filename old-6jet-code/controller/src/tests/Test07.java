package tests;

import java.io.File;
import java.io.FileOutputStream;

public class Test07
{
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        File f = new File("LPT1");
        System.out.println(f.exists());
        FileOutputStream fos = new FileOutputStream(f);
    }
    
}
