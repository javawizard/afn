package jw.jzbot;

import org.jibble.pircbot.PircBot;

/**
 * The master bot persistently joins
 * 
 * @author amboyd
 * 
 */
public class MasterBot extends PircBot
{
    String channelName;
    
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
        sendMessage(sender,
                "Currently, the master interface can only be controlled in a "
                        + "channel. Use \"~!! " + getNick()
                        + " ...\" in the channel.");
        sendMessage(sender,
                "You can, however, pm rejoin to tell the bot to try to rejoin "
                        + "the channel, in case it was previously kicked.");
    }
    
    public void setChannelName(String property)
    {
        this.channelName = property;
    }
    
}
