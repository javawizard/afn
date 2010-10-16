package afntest;

import java.util.List;
import java.util.Map;

import afn.libautobus.AutobusConnection;
import afn.libautobus.FunctionWrapper;
import afn.libautobus.InterfaceWrapper;

public class Autobus01
{
    public static interface SpeakInterface
    {
        public void say_text(String text);
        
        public String get_default_voice();
        
        public void set_default_voice(String voice);
        
        public List<String> get_voice_names();
        
        public int get_pid();
    }
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        AutobusConnection bus =
                AutobusConnection.create("localhost", AutobusConnection.DEFAULT_PORT);
        bus.connect(1);
        SpeakInterface speak = bus.get_interface_proxy("speak", SpeakInterface.class);
        speak.say_text("timer 7");
        System.out.println("Default voice is " + speak.get_default_voice());
        System.out.println("Speak server's pid is " + speak.get_pid());
        bus.shutdown();
    }
}
