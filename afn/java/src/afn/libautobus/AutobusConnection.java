package afn.libautobus;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.json.simple.JSONValue;

import afn.Pair;
import afn.Socketutils;

import static afn.libautobus.MessageTypes.*;

public class AutobusConnection
{
    static void sleep(double delay)
    {
        try
        {
            Thread.sleep((long) (delay * 1000));
        }
        catch (InterruptedException e)
        {
        }
    }
    
    static long toLong(Object o)
    {
        return ((Number) o).longValue();
    }
    
    static <T> T poll(BlockingQueue<T> queue, int timeout, TimeUnit unit)
    {
        try
        {
            return queue.poll(timeout, unit);
        }
        catch (InterruptedException e)
        {
            return null;
        }
    }
    
    static <T> T take(BlockingQueue<T> queue)
    {
        try
        {
            return queue.take();
        }
        catch (InterruptedException e)
        {
            return null;
        }
    }
    
    public static class EmptyRunnable implements Runnable
    {
        @Override
        public void run()
        {
        }
        
    }
    
    class InputThread extends Thread
    {
        private Socket socket;
        private DataInputStream in;
        
        InputThread(Socket socket)
        {
            this.socket = socket;
        }
        
        public void run()
        {
            try
            {
                in = new DataInputStream(socket.getInputStream());
                while (true)
                {
                    int messageLength = in.readInt();
                    byte[] messageData = new byte[messageLength];
                    in.readFully(messageData);
                    Map message = (Map) JSONValue.parse(new String(messageData));
                    messageArrived(message);
                }
            }
            catch (EOFException e)
            {
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
                Socketutils.close(socket);
                inputClosed();
            }
        }
    }
    
    class OutputThread extends Thread
    {
        private Socket socket;
        private DataOutputStream out;
        
        public OutputThread(Socket socket)
        {
            this.socket = socket;
        }
        
        public void run()
        {
            try
            {
                this.out = new DataOutputStream(socket.getOutputStream());
                while (true)
                {
                    Map message = readNextMessage();
                    if (message == EMPTY)
                        break;
                    String messageData = JSONValue.toJSONString(message);
                    byte[] data = messageData.getBytes();
                    out.writeInt(data.length);
                    out.write(data);
                    out.flush();
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
                Socketutils.close(socket);
            }
        }
    }
    
    public static final int DEFAULT_PORT = 28862;
    static final Map EMPTY = Collections.unmodifiableMap(new HashMap()); // We're using
    // this since you can't offer null to a queue, which is really stupid in my opinion
    private Socket socket = null;
    private String host = null;
    private int port;
    private boolean reconnect = true;
    private Runnable onConnect = new EmptyRunnable();
    private Runnable onDisconnect = new EmptyRunnable();
    private boolean printExceptions = false;
    private final Object onConnectLock = new Object();
    private InputThread inputThread = null;
    private OutputThread outputThread = null;
    private boolean isShutDown = false;
    private BlockingQueue<Map> sendQueue = new LinkedBlockingQueue<Map>();
    private Map<Long, BlockingQueue<Map>> receiveQueues =
            new HashMap<Long, BlockingQueue<Map>>();
    private Map<Pair<String, String>, List<EventListener>> eventListeners =
            new HashMap<Pair<String, String>, List<EventListener>>();
    private Map<Pair<String, String>, Object> objectValues =
            new HashMap<Pair<String, String>, Object>();
    private Map<Pair<String, String>, List<ObjectListener>> objectListeners =
            new HashMap<Pair<String, String>, List<ObjectListener>>();
    
    public AutobusConnection()
    {
        this(null);
    }
    
    public AutobusConnection(String host)
    {
        this(null, 0);
    }
    
    public AutobusConnection(String host, int port)
    {
        if (host == null)
            host = System.getenv("AUTOBUS_SERVER");
        if (host == null)
            host = System.getProperty("autobus.server");
        if (host == null)
            host = "localhost";
        if (port == 0 && System.getenv().containsKey("AUTOBUS_PORT"))
            port = Integer.parseInt(System.getenv("AUTOBUS_PORT"));
        if (port == 0)
            port = DEFAULT_PORT;
        this.host = host;
        this.port = port;
    }
    
    public void shutdown()
    {
        isShutDown = true;
        Socketutils.close(socket);
    }
    
    public void startConnecting()
    {
        new Thread()
        {
            public void run()
            {
                try
                {
                    connect(0);
                }
                catch (Exception e)
                {
                }
            }
        }.start();
    }
    
    public void connect()
    {
        connect(1);
    }
    
    public void connect(int attempts)
    {
        int progress = 0;
        double delay = 0.1;
        double delayIncrement = 1.5;
        while ((attempts == 0 || progress < attempts) && !isShutDown)
        {
            progress += 1;
            delay *= delayIncrement;
            if (delay > 20)
                delay = 20;
            try
            {
                socket = new Socket(host, port);
            }
            catch (Exception e)
            {
                sleep(delay);
                continue;
            }
            synchronized (onConnectLock)
            {
                connectionEstablished();
            }
            return;
        }
        throw new RuntimeException("Couldn't connect");
    }
    
    void connectionEstablished()
    {
        inputThread = new InputThread(socket);
        outputThread = new OutputThread(socket);
        sendQueue = new LinkedBlockingQueue<Map>();
        receiveQueues = new HashMap<Long, BlockingQueue<Map>>();
        inputThread.start();
        outputThread.start();
        onConnect.run();
    }
    
    void messageArrived(Map message)
    {
        BlockingQueue<Map> queue = receiveQueues.get(message.get("message_id"));
        if (queue != null)
        {
            queue.offer(message);
            receiveQueues.remove(message.get("message_id"));
            return;
        }
        String action = (String) message.get("action");
        if (action.equals(RunFunctionCommand))
            processRunFunctionCommand(message);
        else if (action.equals(FireEventCommand))
            processFireEventCommand(message);
        else if (action.equals(SetObjectCommand) || action.equals(WatchObjectResponse))
            processSetObjectCommand(message);
        else if (action.equals(PingCommand))
            processPingCommand(message);
        else
            System.out.println("Not processing message for action " + action);
    }
    
    private void processRunFunctionCommand(Map message)
    {
        System.out.println("processRunFunctionCommand not yet implemented");
    }
    
    private void processFireEventCommand(Map message)
    {
        System.out.println("processFireEventCommand not yet implemented");
    }
    
    private void processSetObjectCommand(Map message)
    {
        System.out.println("processSetObjectCommand not yet implemented");
    }
    
    private void processPingCommand(Map message)
    {
        System.out.println("processPingCommand not yet implemented");
    }
    
    Map query(Map message)
    {
        return query(message, 30);
    }
    
    Map query(Map message, int timeout)
    {
        BlockingQueue<Map> queue = new LinkedBlockingQueue<Map>(1);
        if (receiveQueues == null || inputThread == null)
            throw new NotConnectedException();
        receiveQueues.put(toLong(message.get("message_id")), queue);
        send(message);
        Map response = poll(queue, timeout, TimeUnit.SECONDS);
        if (response == EMPTY)
        {
            receiveQueues.remove(toLong(message.get("message_id")));
            throw new TimeoutException();
        }
        return response;
    }
    
    void send(Map message)
    {
        if (message != null && message.get("message_type") != null)
        {
            BlockingQueue<Map> queue = sendQueue;
            if (queue == null)
                throw new NotConnectedException();
            queue.offer(message);
        }
    }
    
    void inputClosed()
    {
        sendQueue.offer(EMPTY);
        for (BlockingQueue<Map> queue : new ArrayList<BlockingQueue<Map>>(receiveQueues
                .values()))
        {
            queue.offer(EMPTY);
        }
        receiveQueues.clear();
        inputThread = null;
        outputThread = null;
        sendQueue = null;
        receiveQueues = null;
        objectValues.clear();
        objectValues = new HashMap<Pair<String, String>, Object>();
        onDisconnect.run();
        if (reconnect && !isShutDown)
            startConnecting();
    }
    
    Map readNextMessage()
    {
        BlockingQueue<Map> queue = sendQueue;
        if (queue == null)
            return null;
        return take(queue);
    }
    
    public InterfaceWrapper getInterface(String name)
    {
        return new InterfaceWrapper(this, name);
    }
}
