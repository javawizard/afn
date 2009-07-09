package jw.jzbot;

import java.io.File;
import java.io.FileOutputStream;

import jw.jzbot.utils.script.Pastebin;

import net.sf.opengroove.common.utils.StringUtils;

import org.jibble.pircbot.PircBot;

/**
 * The master bot persistently joins
 * 
 * @author amboyd
 * 
 */
public class MasterBot extends PircBot
{
    public String channelName;
    
    @Override
    protected void onConnect()
    {
        joinChannel(channelName);
        sendMessage(channelName, "MasterBot/JZBot master interface 0.2");
    }
    
    @Override
    public void onDisconnect()
    {
        while (!isConnected())
        {
            try
            {
                reconnect();
            }
            catch (Exception e)
            {
                System.err.println("Master interface reconnect error");
                e.printStackTrace();
                try
                {
                    Thread.sleep(15 * 1000);
                }
                catch (Exception e2)
                {
                    e2.printStackTrace();
                }
            }
        }
    }
    
    @Override
    protected void onMessage(String channel, String sender, String login,
            String hostname, String message)
    {
        // FIXME: process the message
        if (message.startsWith("~ " + getNick() + " "))
        {
            String messageContent = message.substring(("~ " + getNick() + " ")
                    .length());
            try
            {
                processRealMessage(channel, hostname, messageContent);
            }
            catch (Exception e)
            {
                String pasteId = JZBot.pastebinStackTrace(e);
                sendMessage(channel,
                        "An error occured while processing: http://pastebin.com/"
                                + pasteId);
            }
        }
    }
    
    private void processRealMessage(String channel, String hostname,
            String message) throws Exception
    {
        if (!JZBot.isMasterScriptOp(hostname))
        {
            sendMessage(channel, "You're not a scriptop here.");
            return;
        }
        if (message.equals("list functions"))
        {
            sendMessage(channel, "This is not yet supported.");
        }
        else if (message.equals("list"))
        {
            String[] scripts = JZBot.scriptStorageFolder.list();
            JZUtils.ircSendDelimited(scripts, "   ", this, channel);
        }
        else if (message.startsWith("get "))
        {
            String name = message.substring("get ".length());
            /*
             * This regex ensures that the name does not have "\", "/", or ":"
             * in it, and that it ends with ".js", and contains at least one
             * character before that.
             */
            if (!name.matches("[^\\/\\\\\\:]+\\.js"))
            {
                sendMessage(channel, "invalid name characters");
                return;
            }
            String content = StringUtils.readFile(new File(
                    JZBot.scriptStorageFolder, name));
            String pastebinUrl = Pastebin.createPost("jz_master_interface",
                    content, Pastebin.Duration.DAY, null);
            sendMessage(channel, "" + name + ": http://pastebin.com/"
                    + pastebinUrl);
        }
        else if (message.startsWith("save "))
        {
            String[] tokens = message.substring("save ".length()).split(" ");
            if (tokens.length != 2)
            {
                sendMessage(
                        channel,
                        "save takes exactly 2 arguments, the name of the script "
                                + "(which can only contain letters, numbers, \"-\", and \"_\"), "
                                + "and the pastebin url that you stored the script at. You "
                                + "specified "
                                + tokens.length
                                + " arguments (which will be 1 even if you didn't specify "
                                + "any arguments), not 2.");
                return;
            }
            String name = tokens[0];
            String pastebinUrl = tokens[1];
            if (!name.matches("[^\\/\\\\\\:]+\\.js"))
            {
                sendMessage(channel, "invalid name characters");
                return;
            }
            String content = Pastebin.readPost(pastebinUrl);
            File file = new File(JZBot.scriptStorageFolder, name);
            boolean previousExists = file.exists();
            FileOutputStream fos = new FileOutputStream(file);
            fos.write(content.getBytes());
            fos.flush();
            fos.close();
            if (previousExists)
                sendMessage(channel, name + ": This script has been saved.");
            else
                sendMessage(channel, name + ": This script has been created.");
        }
        else if (message.startsWith("delete "))
        {
            String name = message.substring("get ".length());
            if (!name.matches("[^\\/\\\\\\:]+\\.js"))
            {
                sendMessage(channel, "invalid name characters");
                return;
            }
            File file = new File(JZBot.scriptStorageFolder, name);
            if (!file.exists())
            {
                sendMessage(channel, name + ": That script doesn't exist.");
                return;
            }
            if (!file.delete())
            {
                sendMessage(channel, name
                        + ": An error occured while deleting this script.");
                return;
            }
            sendMessage(channel, name + ": This script has been deleted.");
        }
        else if (message.startsWith("reload"))
        {
            sendMessage(channel, "Shutting down scripts...");
            JZBot.shutdownScripts(true, channel);
            sendMessage(channel, "Starting up scripts...");
            JZBot.startupScripts(channel);
            sendMessage(channel, "Reload complete.");
        }
        else if (message.startsWith("import local "))
        {
            String name = message.substring("import local ".length());
            if (!name.matches("[^\\/\\\\\\:]+\\.js"))
            {
                sendMessage(channel, "invalid name characters");
                return;
            }
            String content = StringUtils.readFile(new File("local-directory",
                    name));
            StringUtils.writeFile(content, new File(JZBot.scriptStorageFolder,
                    name));
            sendMessage(channel, "Done.");
        }
        else
        {
            sendMessage(channel, "Invalid command. Pm \"help\" for more info.");
        }
    }
    
    public void publicSetName(String name)
    {
        super.setName(name);
    }
    
    public void publicSetLogin(String login)
    {
        super.setLogin(login);
    }
    
    @Override
    protected void onPrivateMessage(String sender, String login,
            String hostname, String message)
    {
        if (message.equals("rejoin"))
        {
            if (JZBot.isMasterScriptOp(hostname))
            {
                joinChannel(channelName);
                sendMessage(sender, "joined.");
            }
            else
            {
                sendMessage(sender, "you're not a scriptop.");
            }
            return;
        }
        else if (message.equals("help"))
        {
            sendMessage(sender, "Use \"~ " + getNick()
                    + " ...\" in the channel.");
            sendMessage(sender,
                    "You can, however, pm rejoin to tell the bot to try to rejoin "
                            + "the channel, in case it was previously kicked. "
                            + "Or contact jcp or javawizard2539 for more info.");
        }
        else
        {
            try
            {
                processRealMessage(sender, hostname, message);
            }
            catch (Exception e)
            {
                String pasteId = JZBot.pastebinStackTrace(e);
                sendMessage(sender,
                        "An error occured while processing: http://pastebin.com/"
                                + pasteId);
            }
            
        }
    }
    
    public void setChannelName(String property)
    {
        this.channelName = property;
    }
    
}
