package afn;

import java.net.Socket;

public class Socketutils
{
    public static void close(Socket socket)
    {
        try
        {
            if (!socket.isClosed())
                socket.close();
        }
        catch (Exception ex)
        {
            ex.printStackTrace();
        }
    }
}
