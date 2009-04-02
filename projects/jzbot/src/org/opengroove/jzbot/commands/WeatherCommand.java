package org.opengroove.jzbot.commands;

import java.net.URL;

import org.jdom.Document;
import org.jdom.input.SAXBuilder;
import org.opengroove.jzbot.Command;

public class WeatherCommand implements Command
{
    
    public String getName()
    {
        return "weather";
    }
    
    public void run(String channel, boolean pm, String sender, String hostname,
        String arguments)
    {
        try
        {
            Document doc = new SAXBuilder(false).build(new URL(""));
        }
        catch (Exception e)
        {
            throw new RuntimeException(e.getClass().getName() + ": " + e.getMessage(),
                e);
        }
    }
    
}
