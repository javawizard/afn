package tests;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.OutputStream;

public class Test09
{
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        OutputStream o = new OutputStream()
        {
            
            public void write(int b) throws IOException
            {
                // TODO Auto-generated method stub
                
            }
            
            public void flush()
            {
                System.out.println("flushed");
                new Exception().printStackTrace(System.out);
            }
        };
        ObjectOutputStream oo = new ObjectOutputStream(o);
        oo.flush();
    }
    
}
