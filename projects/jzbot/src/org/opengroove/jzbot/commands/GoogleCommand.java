package org.opengroove.jzbot.commands;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import net.sf.opengroove.common.utils.StringUtils;

import org.json.JSONArray;
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
            JSONObject j =
                new JSONObject(
                    /* new String(baos.toByteArray()) */"{\"responseData\": {\"results\":[{\"GsearchResultClass\":\"GwebSearch\",\"unescapedUrl\":\"http://www.opengroove.org/\",\"url\":\"http://www.opengroove.org/\",\"visibleUrl\":\"www.opengroove.org\",\"cacheUrl\":\"http://www.google.com/search?q\\u003dcache:6Y82w0RtFf4J:www.opengroove.org\",\"title\":\"Home \\u200e(\\u003cb\\u003eOpenGroove\\u003c/b\\u003e)\\u200e\",\"titleNoFormatting\":\"Home \\u200e(OpenGroove)\\u200e\",\"content\":\"\\u003cb\\u003eOpenGroove\\u003c/b\\u003e is a groupware program that allows users to synchronize data. Users   create workspaces, and then invite other users to those workspaces. \\u003cb\\u003e...\\u003c/b\\u003e\"},{\"GsearchResultClass\":\"GwebSearch\",\"unescapedUrl\":\"http://www.stepmania.naota3k.com/index.php?dir\\u003dTeam%20Opengroove%20Stuff/\",\"url\":\"http://www.stepmania.naota3k.com/index.php%3Fdir%3DTeam%2520Opengroove%2520Stuff/\",\"visibleUrl\":\"www.stepmania.naota3k.com\",\"cacheUrl\":\"http://www.google.com/search?q\\u003dcache:cSk6WSaDMV8J:www.stepmania.naota3k.com\",\"title\":\"Index of ./Team \\u003cb\\u003eOpengroove\\u003c/b\\u003e Stuff/\",\"titleNoFormatting\":\"Index of ./Team Opengroove Stuff/\",\"content\":\"home / stepmania / images / music / miscallanious. home. Index of . / Team   \\u003cb\\u003eOpengroove\\u003c/b\\u003e Stuff /. File \\u0026middot; Size \\u0026middot; Modified \\u0026middot; Description \\u0026middot; [dir] Parent   Directory \\u003cb\\u003e...\\u003c/b\\u003e\"},{\"GsearchResultClass\":\"GwebSearch\",\"unescapedUrl\":\"http://thepiratebay.org/torrent/4214753/In_The_Groove_3__Open_Groove_RC1\",\"url\":\"http://thepiratebay.org/torrent/4214753/In_The_Groove_3__Open_Groove_RC1\",\"visibleUrl\":\"thepiratebay.org\",\"cacheUrl\":\"http://www.google.com/search?q\\u003dcache:6j35N1o_BBoJ:thepiratebay.org\",\"title\":\"In The Groove 3: \\u003cb\\u003eOpen Groove\\u003c/b\\u003e RC1 (download torrent) - TPB\",\"titleNoFormatting\":\"In The Groove 3: Open Groove RC1 (download torrent) - TPB\",\"content\":\"Feb 11, 2009 \\u003cb\\u003e...\\u003c/b\\u003e In The Groove 3: \\u003cb\\u003eOpen Groove\\u003c/b\\u003e RC1. Type: Games \\u0026gt; PC; Files: 472; Size: 1.65 GiB (  1768820328 Bytes). Quality: +0 / -0 (0) \\u003cb\\u003e...\\u003c/b\\u003e\"},{\"GsearchResultClass\":\"GwebSearch\",\"unescapedUrl\":\"http://www.ohloh.net/p/opengroove\",\"url\":\"http://www.ohloh.net/p/opengroove\",\"visibleUrl\":\"www.ohloh.net\",\"cacheUrl\":\"http://www.google.com/search?q\\u003dcache:xhoVvwRDg9QJ:www.ohloh.net\",\"title\":\"\\u003cb\\u003eOpenGroove\\u003c/b\\u003e\",\"titleNoFormatting\":\"OpenGroove\",\"content\":\"\\u003cb\\u003eOpenGroove\\u003c/b\\u003e is a collaborative software program that allows users across multiple   locations to synchronize data. Users can create workspaces, and share those \\u003cb\\u003e...\\u003c/b\\u003e\"}],\"cursor\":{\"pages\":[{\"start\":\"0\",\"label\":1},{\"start\":\"4\",\"label\":2},{\"start\":\"8\",\"label\":3},{\"start\":\"12\",\"label\":4},{\"start\":\"16\",\"label\":5},{\"start\":\"20\",\"label\":6},{\"start\":\"24\",\"label\":7},{\"start\":\"28\",\"label\":8}],\"estimatedResultCount\":\"27800\",\"currentPageIndex\":0,\"moreResultsUrl\":\"http://www.google.com/search?oe\\u003dutf8\\u0026ie\\u003dutf8\\u0026source\\u003duds\\u0026start\\u003d0\\u0026hl\\u003den\\u0026q\\u003dopengroove\"}}, \"responseDetails\": null, \"responseStatus\": 200}");
            JSONObject responseDataObject = j.getJSONObject("responseData");
            JSONArray resultsObject = responseDataObject.getJSONArray("results");
            JZBot.bot.sendMessage(pm ? sender : channel, "Search results for "
                + arguments + ", " + resultsObject.length() + " result"
                + (resultsObject.length() == 1 ? "" : "s")
                + ", http://google.com/search?q=" + URLEncoder.encode(arguments));
            for (int i = 0; i < resultsObject.length(); i++)
            {
                JSONObject result = resultsObject.getJSONObject(i);
                System.out.println(result.names().toString());
            }
            JZBot.bot.sendMessage(pm ? sender : channel, "End of search results");
        }
        catch (Exception e)
        {
            throw new RuntimeException(e.getClass().getName() + ": " + e.getMessage(),
                e);
        }
    }
}
