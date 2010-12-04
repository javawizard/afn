package afntest;

import java.util.List;
import java.util.Map;

import afn.libautobus_jython.AutobusConnection;
import afn.libautobus_jython.FunctionWrapper;
import afn.libautobus_jython.InterfaceWrapper;
import afn.libautobus_jython.ObjectListener;

public class Autobus01
{
    public static interface SpeakInterface
    {
        public void say_text(String text);
        
        public String get_default_voice();
        
        public void set_default_voice(String voice);
        
        public List<String> get_voice_names();
        
        public int get_pid();
        
        // A method that doesn't exist on the actual speak interface that I was using to
        // test out what would happen when a nonexistant method is called
        public void bogus();
    }
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        AutobusConnection bus =
                AutobusConnection.create("localhost", AutobusConnection.DEFAULT_PORT);
        bus.add_object_watch("example", "test", new ObjectListener<?>()
        {
            
            @Override
            public void changed(Object value)
            {
                System.out.println("New object value: " + value);
                if (value instanceof List)
                    System.out.println("List. First value: " + ((List) value).get(0));
            }
        });
        bus.connect(1);
        SpeakInterface speak = bus.get_interface_proxy("speak", SpeakInterface.class);
        speak.say_text("timer 7");
        System.out.println("Default voice is " + speak.get_default_voice());
        System.out.println("Speak server's pid is " + speak.get_pid());
        // bus.shutdown();
    }
}
