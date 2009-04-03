package org.opengroove.jzbot.commands;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.net.URL;
import java.net.URLEncoder;

import net.sf.opengroove.common.utils.StringUtils;

import org.json.JSONObject;
import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.ResponseException;
import org.opengroove.utils.OneTimeIterable;

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
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            InputStream in = url.openStream();
            StringUtils.copy(in, baos);
            in.close();
            JSONObject j = new JSONObject(new String(baos.toByteArray()));
            String currentList = "";
            for (Object name : new OneTimeIterable(j.keys()))
            {
                currentList += name.toString() + "  ";
                if (currentList.length() > 200)
                {
                    JZBot.bot.sendMessage(pm ? sender : channel, currentList);
                    currentList = "";
                }
            }
            if (!currentList.equals(""))
                JZBot.bot.sendMessage(pm ? sender : channel, currentList);
        }
        catch (Exception e)
        {
            throw new RuntimeException(e.getClass().getName() + ": " + e.getMessage(),
                e);
        }
    }
    
}
