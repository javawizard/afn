package jw.jzbot.utils.script;

import jw.jzbot.utils.script.Pastebin.Duration;

public class PastebinWrapper
{
    public String post(String poster, String post, String duration,
            String parent)
    {
        duration = duration.toLowerCase();
        Duration durationValue;
        if (duration.startsWith("d"))
            durationValue = Duration.DAY;
        else if (duration.startsWith("m"))
            durationValue = Duration.MONTH;
        else if (duration.startsWith("f"))
            durationValue = Duration.FOREVER;
        else
            throw new IllegalArgumentException(
                    "Duration should be either day, month, or forever.");
        return Pastebin.createPost(poster, post, durationValue, parent);
    }
    
    public String read(String url)
    {
        return Pastebin.readPost(url);
    }
}
