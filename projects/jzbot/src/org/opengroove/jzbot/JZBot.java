package org.opengroove.jzbot;

import java.io.IOException;

import org.jibble.pircbot.IrcException;
import org.jibble.pircbot.NickAlreadyInUseException;
import org.jibble.pircbot.PircBot;

public class JZBot extends PircBot
{
    // protected void handleLine(String line)
    // {
    // System.out.println("HANDLE: " + line);
    // super.handleLine(line);
    // }
    
    private static int currentBarrel = 0;
    
    protected void onServerResponse(int code, String response)
    {
        System.out.println("RESPONSE: " + code + " " + response);
        super.onServerResponse(code, response);
    }
    
    private static int loadedBarrel = 0;
    
    public static void main(String[] args) throws Throwable
    {
        new JZBot().start();
    }
    
    protected void onMessage(String channel, String sender, String login,
        String hostname, String message)
    {
        if (message.startsWith("~roulette"))
        {
            if (message.endsWith("reset"))
            {
                currentBarrel = 0;
                loadedBarrel = 0;
                sendMessage(channel, "Roulette reset.");
            }
            else
            {
                if (currentBarrel == 0)
                {
                    System.out.println("random barrel");
                    loadedBarrel = (int) ((Math.random() * 6.0) + 1.0);
                    System.out.println("" + loadedBarrel);
                }
                currentBarrel++;
                if (currentBarrel == loadedBarrel)
                {
                    sendMessage(channel, "" + sender + ": (Chamber " + currentBarrel
                        + " of 6) *BANG*");
                    kick(channel, sender);
                    currentBarrel = 0;
                    loadedBarrel = 0;
                }
                else
                {
                    sendMessage(channel, "" + sender + ": (Chamber " + currentBarrel
                        + " of 6) *click*");
                }
            }
        }
        if (message.equals("reconnect"))
        {
            this.disconnect();
        }
    }
    
    protected void onDisconnect()
    {
        try
        {
            this.reconnect();
        }
        catch (NickAlreadyInUseException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (IOException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (IrcException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    
    protected void onNotice(String sourceNick, String sourceLogin,
        String sourceHostname, String target, String notice)
    {
        System.out.println("NOTICE from " + sourceNick + ": " + notice);
    }
    
    protected void onPrivateMessage(String sender, String login, String hostname,
        String message)
    {
        System.out.println("sent by " + sender + ", identified as " + login + ": "
            + message);
    }
    
    private void start()
    {
        try
        {
            setAutoNickChange(true);
            setName("jzbot");
            setLogin("jzbot");
            System.out.println("Connecting");
            connect("irc.freenode.net", 6667, "28574923");
        }
        catch (NickAlreadyInUseException e)
        {
            e.printStackTrace();
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
        catch (IrcException e)
        {
            e.printStackTrace();
        }
    }
    
    protected void onConnect()
    {
        System.out.println("onconnect");
        joinChannel("#opengroove");
        sendRawLineViaQueue("whois jcp");
    }
    
    protected void onUnknown(String line)
    {
        System.out.println("UNKNOWN: " + line);
    }
    
    protected void onUserMode(String targetNick, String sourceNick, String sourceLogin,
        String sourceHostname, String mode)
    {
        System.out.println("umode: " + targetNick + " by " + sourceNick + ": " + mode);
    }
}
