package jw.jzbot.utils.script;

import java.io.File;
import java.io.FileOutputStream;

import net.sf.opengroove.common.utils.StringUtils;
import jw.jzbot.JZBot;

public class MasterBotScriptObject
{
    public String[] list()
    {
        return JZBot.scriptStorageFolder.list();
    }
    
    public String get(String name)
    {
        if (!name.matches("[^\\/\\\\\\:]+\\.js"))
        {
            return null;
        }
        String content = StringUtils.readFile(new File(
                JZBot.scriptStorageFolder, name));
        return content;
    }
    
    public String getAsPastebin(String name)
    {
        return "http://pastebin.com/"
                + Pastebin.createPost("jz_master_interface", get(name),
                        Pastebin.Duration.DAY, null);
    }
    
    public String saveFromPastebin(String name, String pastebinUrl)
    {
        if (!name.matches("[^\\/\\\\\\:]+\\.js"))
        {
            return "Invalid name characters";
        }
        String content = Pastebin.readPost(pastebinUrl);
        File file = new File(JZBot.scriptStorageFolder, name);
        StringUtils.writeFile(content, file);
        return null;
    }
}
