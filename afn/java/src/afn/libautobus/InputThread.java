package afn.libautobus;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;
import java.util.concurrent.BlockingQueue;

import afn.libautobus.protocol.Protobuf.Message;

public class InputThread extends Thread
{
    public static interface OnClose
    {
        public void onClose();
    }
    
    private Socket socket;
    private AutobusConnection bus;
    
    public InputThread(Socket socket, AutobusConnection bus)
    {
        this.socket = socket;
        this.bus = bus;
    }
    
    public void run()
    {
        try
        {
            InputStream socketIn = socket.getInputStream();
            DataInputStream in = new DataInputStream(socketIn);
            while (true)
            {
                int messageLength = in.readInt();
                byte[] messageData = new byte[messageLength];
                in.readFully(messageData);
                Message message = new Message().deserialize(messageData);
                bus.messageArrived(message);
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
            bus.inputClosed();
        }
    }
}
