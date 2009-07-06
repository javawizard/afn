package jw.jzbot.com.script;

import java.io.IOException;

import jw.jzbot.com.irc.IRCBot;

import org.jibble.pircbot.IrcException;
import org.jibble.pircbot.NickAlreadyInUseException;
import org.jibble.pircbot.User;

/**
 * A connection to an IRC server, made available to the script. This just wraps
 * IRCBot, hiding those methods from the script that shouldn't be visible to it.
 * 
 * @author amboyd
 * 
 */
public class IRCConnection
{
    private IRCBot bot;
    
    IRCConnection()
    {
        this = 
    }
    
    public final void ban(String channel, String hostmask)
    {
        bot.ban(channel, hostmask);
    }
    
    public final void changeNick(String newNick)
    {
        bot.changeNick(newNick);
    }
    
    public final void connect(String hostname, int port, String password)
            throws IOException, IrcException, NickAlreadyInUseException
    {
        bot.connect(hostname, port, password);
    }
    
    public final void connect(String hostname, int port) throws IOException,
            IrcException, NickAlreadyInUseException
    {
        bot.connect(hostname, port);
    }
    
    public final void connect(String hostname) throws IOException,
            IrcException, NickAlreadyInUseException
    {
        bot.connect(hostname);
    }
    
    public final void deOp(String channel, String nick)
    {
        bot.deOp(channel, nick);
    }
    
    public final void deVoice(String channel, String nick)
    {
        bot.deVoice(channel, nick);
    }
    
    public final void disconnect()
    {
        bot.disconnect();
    }
    
    public final String[] getChannels()
    {
        return bot.getChannels();
    }
    
    public final String getLogin()
    {
        return bot.getLogin();
    }
    
    public final int getMaxLineLength()
    {
        return bot.getMaxLineLength();
    }
    
    public final long getMessageDelay()
    {
        return bot.getMessageDelay();
    }
    
    public final String getName()
    {
        return bot.getName();
    }
    
    public String getNick()
    {
        return bot.getNick();
    }
    
    public final int getOutgoingQueueSize()
    {
        return bot.getOutgoingQueueSize();
    }
    
    public final String getPassword()
    {
        return bot.getPassword();
    }
    
    public final int getPort()
    {
        return bot.getPort();
    }
    
    public final String getServer()
    {
        return bot.getServer();
    }
    
    public final User[] getUsers(String channel)
    {
        return bot.getUsers(channel);
    }
    
    public final String getVersion()
    {
        return bot.getVersion();
    }
    
    public final void identify(String password)
    {
        bot.identify(password);
    }
    
    public final boolean isConnected()
    {
        return bot.isConnected();
    }
    
    public final void joinChannel(String channel, String key)
    {
        bot.joinChannel(channel, key);
    }
    
    public final void joinChannel(String channel)
    {
        bot.joinChannel(channel);
    }
    
    public final void kick(String channel, String nick, String reason)
    {
        bot.kick(channel, nick, reason);
    }
    
    public final void kick(String channel, String nick)
    {
        bot.kick(channel, nick);
    }
    
    public final void listChannels()
    {
        bot.listChannels();
    }
    
    public final void listChannels(String parameters)
    {
        bot.listChannels(parameters);
    }
    
    public final void op(String channel, String nick)
    {
        bot.op(channel, nick);
    }
    
    public final void partChannel(String channel, String reason)
    {
        bot.partChannel(channel, reason);
    }
    
    public final void partChannel(String channel)
    {
        bot.partChannel(channel);
    }
    
    public final void quitServer()
    {
        bot.quitServer();
    }
    
    public final void quitServer(String reason)
    {
        bot.quitServer(reason);
    }
    
    public final void reconnect() throws IOException, IrcException,
            NickAlreadyInUseException
    {
        bot.reconnect();
    }
    
    public final void sendAction(String target, String action)
    {
        bot.sendAction(target, action);
    }
    
    public final void sendInvite(String nick, String channel)
    {
        bot.sendInvite(nick, channel);
    }
    
    public final void sendMessage(String target, String message)
    {
        bot.sendMessage(target, message);
    }
    
    public final void sendNotice(String target, String notice)
    {
        bot.sendNotice(target, notice);
    }
    
    public void setAutoNickChange(boolean autoNickChange)
    {
        bot.setAutoNickChange(autoNickChange);
    }
    
    public final void setMessageDelay(long delay)
    {
        bot.setMessageDelay(delay);
    }
    
    public final void setMode(String channel, String mode)
    {
        bot.setMode(channel, mode);
    }
    
    public final void setTopic(String channel, String topic)
    {
        bot.setTopic(channel, topic);
    }
    
    public final void unBan(String channel, String hostmask)
    {
        bot.unBan(channel, hostmask);
    }
    
    public final void voice(String channel, String nick)
    {
        bot.voice(channel, nick);
    }
    
}
