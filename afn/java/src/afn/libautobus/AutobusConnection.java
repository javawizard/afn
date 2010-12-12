package afn.libautobus;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStreamReader;
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
import static afn.libautobus.Messages.*;
import static afn.libautobus.Translation.*;

/**
 * A connection to an Autobus server.<br/><br/>
 * 
 * DEVELOPERS: This library is intended to be almost a line-for-line port of Python's
 * libautobus for maintainability. Try to keep it as close to that as possible while still
 * allowing the library to be Java-like in its usage.
 * 
 * @author Alexander Boyd
 * 
 */
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
        private BufferedReader in;
        
        InputThread(Socket socket)
        {
            this.socket = socket;
        }
        
        public void run()
        {
            try
            {
                in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                String line;
                while ((line = in.readLine()) != null)
                {
                    if (line.trim().equals(""))
                        continue;
                    Map message = (Map) JSONValue.parse(line);
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
                    out.write(data);
                    out.write("\r\n".getBytes());
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
    
    public void addObjectWatch(String interfaceName, String objectName,
            ObjectListener listener)
    {
        Pair<String, String> objectSpec =
                new Pair<String, String>(interfaceName, objectName);
        if (!objectListeners.containsKey(objectSpec))
            objectListeners.put(objectSpec, new ArrayList<ObjectListener>());
        objectListeners.get(objectSpec).add(listener);
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
        for (Pair<String, String> objectSpec : objectListeners.keySet())
        {
            Map message =
                    createMessage(WatchObjectCommand, COMMAND, "interface_name",
                            objectSpec.first, "object_name", objectSpec.second);
            send(message);
        }
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
        String interfaceName = (String) message.get("interface_name");
        String objectName = (String) message.get("object_name");
        Object value = message.get("value");
        Pair<String, String> objectSpec =
                new Pair<String, String>(interfaceName, objectName);
        objectValues.put(objectSpec, decodeObject(value));
        notifyObjectListeners(objectSpec);
    }
    
    private void notifyObjectListeners(Pair<String, String> objectSpec)
    {
        Object value = objectValues.get(objectSpec);
        if (objectListeners.keySet().contains(objectSpec))
        {
            for (ObjectListener listener : objectListeners.get(objectSpec))
            {
                try
                {
                    listener.changed(value);
                }
                catch (Throwable e)
                {
                    e.printStackTrace();
                }
            }
        }
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
