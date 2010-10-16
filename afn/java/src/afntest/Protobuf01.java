package afntest;

import java.util.Arrays;

import afn.libautobus.protocol.Protobuf.*;

public class Protobuf01
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        Message m = new Message();
        m.deserialize(new byte[] { 13, 1, 0, 0, 0, 18, 29, 10, 5, 115, 112, 101, 97, 107,
                26, 20, 83, 97, 121, 115, 32, 115, 116, 117, 102, 102, 32, 111, 117, 116,
                32, 108, 111, 117, 100, 46, (byte) 225, 3, (byte) 210, 4, 0, 0, 0, 0, 0, 0,
                (byte) 162, 6, 1, 50 });
        printBytes(m.serialize());
    }
    
    public static void printBytes(byte[] bytes)
    {
        int[] ints = new int[bytes.length];
        for (int i = 0; i < ints.length; i++)
        {
            ints[i] = bytes[i];
            if (ints[i] < 0)
                ints[i] += 256;
        }
        System.out.println(Arrays.toString(ints));
    }
    
}
