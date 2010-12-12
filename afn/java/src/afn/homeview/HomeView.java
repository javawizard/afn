package afn.homeview;

import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.GridLayout;
import java.awt.Panel;
import java.awt.ScrollPane;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Map;

import afn.libautobus.AutobusConnection;
import afn.libautobus.ObjectListener;

public class HomeView
{
    public static class Listener implements ObjectListener<Map<String, Map<String, ?>>>
    {
        
        @Override
        public void changed(Map<String, Map<String, ?>> value)
        {
            if (value == null)
            {
                panel.removeAll();
                return;
            }
            ArrayList<String> existing = new ArrayList<String>();
            for (int i = 0; i < panel.getComponentCount(); i++)
            {
                Module module = (Module) panel.getComponent(i);
                if (!value.keySet().contains(module.getAddress()))
                {
                    panel.remove(i);
                    i -= 1;
                    continue;
                }
                existing.add(module.getAddress());
                Map<String, ?> moduleMap = value.get(module.getAddress());
                module.setName(moduleMap.containsKey("name") ? (String) moduleMap
                        .get("name") : "");
            }
            ArrayList<String> newModules = new ArrayList<String>(value.keySet());
            newModules.removeAll(existing);
            for (String address : newModules)
            {
                Map moduleMap = value.get(address);
                Module module =
                        new Module(address,
                                moduleMap.containsKey("name") ? (String) moduleMap
                                        .get("name") : "");
                boolean added = false;
                for (int i = 0; i < panel.getComponentCount(); i++)
                {
                    if (((Module) panel.getComponent(i)).getAddress().compareTo(address) > 0)
                    {
                        added = true;
                        panel.add(module, i);
                        break;
                    }
                }
                if (!added)
                    panel.add(module);
            }
            frame.invalidate();
            frame.validate();
            frame.repaint();
        }
        
    }
    
    public static Frame frame;
    
    public static Panel panel;
    
    public static AutobusConnection bus;
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        frame = new Frame();
        Panel inScroll = new Panel();
        ScrollPane scroll = new ScrollPane(ScrollPane.SCROLLBARS_AS_NEEDED);
        scroll.add(inScroll);
        frame.add(scroll);
        inScroll.setLayout(new BorderLayout());
        panel = new Panel();
        panel.setLayout(new GridLayout(0, 2));
        inScroll.add(panel, BorderLayout.NORTH);
        frame.setSize(240, 320);
        frame.setLocationRelativeTo(null);
        frame.show();
        bus = new AutobusConnection();
        bus.addObjectWatch("configure.home", "options", new Listener());
        bus.startConnecting();
    }
    
}
