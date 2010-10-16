package afn.libautobus;

import java.net.Socket;
import java.util.List;

import com.google.protobuf.Descriptors.Descriptor;
import com.google.protobuf.Descriptors.FieldDescriptor;

public class ProtocolUtils
{
    /**
     * Closes the specified socket without throwing an exception if it already closed or
     * another problem happens.
     * 
     * @param socket
     */
    public static void close(Socket socket)
    {
        try
        {
            socket.close();
        }
        catch (Exception e)
        {
        }
    }
    
}
