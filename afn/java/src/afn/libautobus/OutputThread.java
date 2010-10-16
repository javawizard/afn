package afn.libautobus;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.Socket;
import java.util.Queue;
import java.util.concurrent.BlockingQueue;

import static afn.libautobus.protocol.Protobuf.Message;

/**
 * @author Alexander Boyd
 * 
 */
public class OutputThread extends Thread
{
    private Socket socket;
    private AutobusConnection bus;
    
    public OutputThread(Socket socket, AutobusConnection bus)
    {
        this.socket = socket;
        this.bus = bus;
    }
    
    public void run()
    {
        try
        {
            OutputStream socketOut = socket.getOutputStream();
            DataOutputStream out = new DataOutputStream(socketOut);
            while (true)
            {
                Message message = bus.readNextMessage();
                if (message == null)
                    break;
                byte[] messageData = message.serialize();
                out.writeInt(messageData.length);
                out.write(messageData);
                out.flush();
                bus.messageWriteFinished();
            }
        }
        catch (IOException e)
        {
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        finally
        {
            ProtocolUtils.close(socket);
        }
    }
}
