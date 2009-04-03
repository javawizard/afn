package org.opengroove.jzbot.commands;

import java.net.URL;
import java.net.URLEncoder;

import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.ResponseException;

public class GoogleCommand implements Command
{
    
    public String getName()
    {
        return "google";
    }
    
    public void run(String channel, boolean pm, String sender, String hostname,
        String arguments)
    {
        if (arguments.equals(""))
            throw new ResponseException("specify search terms after the google command");
        try
        {
            URL url =
                new URL(
                    "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=opengroove"
                        + URLEncoder.encode(arguments));
        }
        catch (Exception e)
        {
            throw new RuntimeException(e.getClass().getName() + ": " + e.getMessage(),
                e);
        }
    }
    
}
