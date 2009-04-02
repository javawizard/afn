package org.opengroove.jzbot.commands;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.net.URL;
import java.util.HashMap;

import net.sf.opengroove.common.utils.StringUtils;

import org.jdom.Document;
import org.jdom.input.SAXBuilder;
import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.ResponseException;
import org.opengroove.jzbot.storage.Factoid;

public class WeatherCommand implements Command
{
    // yahoo: http://weather.yahooapis.com/forecastrss?p=94089
    // weatherbug: A7686974884
    
    public String getName()
    {
        return "weather";
    }
    
    public void run(String channel, boolean pm, String sender, String hostname,
        String arguments)
    {
        if (arguments.equals(""))
            throw new ResponseException(
                "You need to specify a zip code. For example, ~weather 12345");
        Factoid weatherFactoid = null;
        if (channel != null)
            weatherFactoid =
                JZBot.storage.getChannel(channel).getFactoid("weathertemplate");
        if (weatherFactoid == null)
            weatherFactoid = JZBot.storage.getFactoid("weathertemplate");
        if (weatherFactoid == null)
            throw new ResponseException("The weathertemplate factoid does not exist.");
        HashMap<String, String> map = new HashMap<String, String>();
        try
        {
            URL url =
                new URL(
                    "http://a7686974884.isapi.wxbug.net/WxDataISAPI/WxDataISAPI.dll?Magic=10991&RegNum=0&ZipCode="
                        + arguments.replace("&", "")
                        + "&Units=0&Version=7&Fore=0&t=123456");
            InputStream stream = url.openStream();
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            StringUtils.copy(stream, baos);
            stream.close();
            String[] tokens = new String(baos.toByteArray()).split("\\|");
            map.put("temp", tokens[3]);
            map.put("winddir", tokens[4]);
            map.put("windspeed", tokens[5]);
            map.put("pressure", tokens[10]);
            map.put("humid", tokens[11]);
            map.put("hightemp", tokens[12]);
            map.put("lowtemp", tokens[13]);
            map.put("dewpoint", tokens[14]);
            map.put("windchill", tokens[15]);
            String result =
                JZBot.runFactoid(weatherFactoid, channel, sender, new String[0], map);
            JZBot.bot.sendMessage(pm ? sender : channel, result);
        }
        catch (Exception e)
        {
            if (e instanceof ResponseException)
                throw (ResponseException) e;
            e.printStackTrace();
            throw new RuntimeException(e.getClass().getName() + ": " + e.getMessage(),
                e);
        }
    }
}
