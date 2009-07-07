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
    
    @Override
    protected void onConnect()
    {
        // TODO Auto-generated method stub
        super.onConnect();
    }
    
    @Override
    protected void onDisconnect()
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
    
    @Override
    protected void onPrivateMessage(String sender, String login,
            String hostname, String message)
    {
        sendMessage(
                sender,
                "Currently, the master interface can only be controlled in a "
                        + "channel. Use \"~!! <nick> commands ...\" in the channel.");
    }
    
}
