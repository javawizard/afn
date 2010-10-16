package afn.libautobus;

import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import afn.libautobus.protocol.Protobuf.Message;

/**
 * A connection to an Autobus server. This is typically the class that you'll use the most
 * when connecting to Autobus.<br/><br/>
 * 
 * This class's API approximately mirrors the API provided by the Python version of
 * libautobus, except where language-specific variations make sense. If a method on this
 * class is missing documentation, refer to the documentation of the equivalent method in
 * the Python libautobus for information on what it does.<br/><br/>
 * 
 * To use this class, you'll typically do something like this:<br/><br/>
 * 
 * <pre>
 * AutobusConnection bus = new AutobusConnection(&quot;localhost&quot;, Autobus.DEFAULT_PORT);
 * bus.connect();
 * </pre>
 * 
 * <br/><br/>
 * 
 * Now, let's say you want to invoke the function say_text on the interface speak, passing
 * in the string "the_time is :t:12:34pm". There are three ways to do this:<br/><br/>
 * 
 * <pre>
 * InterfaceWrapper speak = bus.get_interface(&quot;speak&quot;);
 * FunctionWrapper say_text = bus.get_function(&quot;say_text&quot;);
 * say_text.invoke(&quot;the_time is :t:12:34pm&quot;);
 * </pre>
 * 
 * <br/><br/>
 * 
 * This way works great, but it's a little verbose. We can cut one line of code
 * out:<br/><br/>
 * 
 * <pre>
 * InterfaceWrapper speak = bus.get_interface(&quot;speak&quot;);
 * speak.invoke(&quot;say_text&quot;, &quot;the_time is :t:12:34pm&quot;);
 * </pre>
 * 
 * <br/><br/>
 * 
 * This is great, but it's still verbose. There's a better way to do it if the function
 * names are known in advance: we can create an interface and get the connection to
 * instantiate a proxy instance of the interface, like this:<br/><br/>
 * 
 * <pre>
 * --- SpeakInterface.java ---
 * public interface SpeakInterface {
 *     public void say_text(String text);
 *     ...other functions you might wish to add...
 * }
 * 
 * --- The code you're writing ---
 * SpeakInterface speak = bus.get_interface(&quot;speak&quot;, SpeakInterface.class);
 * speak.say_text(&quot;the_time is :t:12:34pm&quot;);
 * </pre>
 * 
 * <br/><br/>
 * 
 * This is a lot better. Of course, it's only practical if you know the names of the
 * functions you're going to invoke in advance, but you usually will. The function could
 * have declared a return value, too: if the actual function hadn't returned anything, the
 * proxy function would have returned null or thrown a NullPointerException if the return
 * value was declared to be primitive.<br/><br/>
 * 
 * Proxy interface instances created in this manner are good for as long as the
 * AutobusConnection object lasts. If a method is called and the AutobusConnection does
 * not currently have a connection to the server, an exception will be thrown, but the
 * method can be called again successfully once the AutobusConnection
 * reconnects.<br/><br/>
 * 
 * The name of the Java interface used as the proxy does not matter. We could have named
 * it Example, and then called <tt>bus.get_interface("speak", Example.class)</tt> and
 * everything would have worked great.
 * 
 * @author Alexander Boyd
 * 
 */
public class AutobusConnection
{
    public static final int DEFAULT_PORT = 28862;
    private Socket socket;
    private String host;
    private int port;
    private boolean reconnect;
    private Object onConnectLock;
    private Map<String, LocalInterface> interfaces;
    private boolean isShutDown = false;
    private BlockingQueue<Message> sendQueue;
    private Map<String, BlockingQueue<Message>> receiveQueues;
    private Map<NamePair, Object> objectValues;
    private Map<NamePair, List<ObjectListener>> objectListeners;
    private InputThread inputThread;
    private OutputThread outputThread;
    
    public AutobusConnection(String host, int port)
    {
        this(host, port, true);
    }
    
    public AutobusConnection(String host, int port, boolean reconnect)
    {
        this.host = host;
        this.port = port;
        this.reconnect = reconnect;
        this.onConnectLock = new Object();
        this.interfaces = new HashMap<String, LocalInterface>();
        this.sendQueue = new LinkedBlockingQueue<Message>();
        this.receiveQueues = new HashMap<String, BlockingQueue<Message>>();
        this.objectValues = new HashMap<NamePair, Object>();
        this.objectListeners = new HashMap<NamePair, List<ObjectListener>>();
    }
    
    public void shutdown()
    {
        this.isShutDown = true;
        ProtocolUtils.close(this.socket);
    }
    
    public void addInterface(String name)
    {
        addInterface(name, null);
    }
    
    public void addInterface(String name, Object interfaceObject)
    {
        LocalInterface localInterface = new LocalInterface(name, getDoc(interfaceObject));
        this.interfaces.put(name, localInterface);
        localInterface.connection = this;
        if (interfaceObject != null)
            localInterface.addAll(interfaceObject);
    }
    
    public void addFunction(String interfaceName, String functionName, String doc,
            FunctionTarget function)
    {
        this.interfaces.get(interfaceName).registerFunction(
                new LocalFunction(functionName, doc, function));
    }
    
    public void addObjectWatch(String interfaceName, String objectName,
            ObjectListener listener)
    {
        NamePair objectSpec = new NamePair(interfaceName, objectName);
        if (!this.objectListeners.containsKey(objectSpec))
            this.objectListeners.put(objectSpec, new ArrayList<ObjectListener>());
        this.objectListeners.get(objectSpec).add(listener);
    }
    
    public LocalObject addObject(String interfaceName, String objectName, String doc,
            Object value)
    {
        LocalObject object = new LocalObject(objectName, doc, value);
        this.interfaces.get(interfaceName).registerObject(object);
        return object;
    }
    
    public void startConnecting()
    {
        new Thread()
        {
            public void run()
            {
                AutobusConnection.this.connect(0);
            }
        }.start();
    }
    
    public void connect(int attempts)
    {
        int progress = 0;
        double delay = 0.1;
        double delayIncrement = 1.5;
        while ((attempts == 0 || progress < attempts) && !this.isShutDown)
        {
            progress += 1;
            delay *= delayIncrement;
            if (delay < 20)
                delay = 20;
            try
            {
                this.socket = new Socket(this.host, this.port);
            }
            catch (Exception e)
            {
                sleep(delay);
                continue;
            }
            synchronized (onConnectLock)
            {
                this.connectionEstablished();
            }
        }
        throw new RuntimeException("Couldn't connect");
    }
    
    protected void connectionEstablished()
    {
        this.inputThread = new InputThread(socket, this);
        this.outputThread = new OutputThread(socket, this);
        this.sendQueue = new LinkedBlockingQueue<Message>();
        this.receiveQueues = new HashMap<String, BlockingQueue<Message>>();
        this.inputThread.start();
        this.outputThread.start();
        for (LocalInterface localInterface : this.interfaces.values())
        {
            
        }
    }
    
    /**
     * Returns the documentation contained on the specified object's class's Doc
     * annotation, or the empty string if the class of the object in question does not
     * have any such annotation.<br/><br/>
     * 
     * If the specified object is an instance of {@link java.lang.Class}, then the object
     * itself is used as the class to look up. Otherwise, the object's class is the class
     * checked. In other words, getDoc(someNonClassObject) is the same as
     * getDoc(someNonClassObject.getClass()).
     * 
     * @param object
     * @return
     */
    String getDoc(Object object)
    {
        Class<?> type = ((object instanceof Class) ? (Class<?>) object : object.getClass());
        if (type.isAnnotationPresent(Doc.class))
            return type.getAnnotation(Doc.class).value();
        return "";
    }
    
    static void sleep(double seconds)
    {
        try
        {
            Thread.sleep((long) (seconds * 1000));
        }
        catch (InterruptedException e)
        {
        }
    }
    
    static <T extends Builder> MessagePair<T> createMessagePair(Class<T> )
    {
        
    }
}
