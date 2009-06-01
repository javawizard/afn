package org.opengroove.jzbot.com.irc;

import org.jibble.pircbot.PircBot;
import org.opengroove.jzbot.com.ProtocolContext;

public class IRCBot extends PircBot
{
    public IRCProtocol protocol;
    public ProtocolContext context;
    public IRCBot(IRCProtocol protocol)
    {
        this.protocol = protocol;
        this.context = protocol.context;
    }

    protected void onAction(String sender, String login, String hostname,
        String target, String action)
    {
        // TODO Auto-generated method stub
        super.onAction(sender, login, hostname, target, action);
    }

    protected void onConnect()
    {
        // TODO Auto-generated method stub
        super.onConnect();
    }

    protected void onDeop(String channel, String sourceNick, String sourceLogin,
        String sourceHostname, String recipient)
    {
        // TODO Auto-generated method stub
        super.onDeop(channel, sourceNick, sourceLogin, sourceHostname, recipient);
    }

    protected void onDisconnect()
    {
        // TODO Auto-generated method stub
        super.onDisconnect();
    }

    protected void onJoin(String channel, String sender, String login, String hostname)
    {
        // TODO Auto-generated method stub
        super.onJoin(channel, sender, login, hostname);
    }

    protected void onKick(String channel, String kickerNick, String kickerLogin,
        String kickerHostname, String recipientNick, String reason)
    {
        // TODO Auto-generated method stub
        super.onKick(channel, kickerNick, kickerLogin, kickerHostname, recipientNick, reason);
    }

    protected void onMessage(String channel, String sender, String login,
        String hostname, String message)
    {
        // TODO Auto-generated method stub
        super.onMessage(channel, sender, login, hostname, message);
    }

    protected void onNickChange(String oldNick, String login, String hostname,
        String newNick)
    {
        // TODO Auto-generated method stub
        super.onNickChange(oldNick, login, hostname, newNick);
    }

    protected void onNotice(String sourceNick, String sourceLogin,
        String sourceHostname, String target, String notice)
    {
        // TODO Auto-generated method stub
        super.onNotice(sourceNick, sourceLogin, sourceHostname, target, notice);
    }

    protected void onOp(String channel, String sourceNick, String sourceLogin,
        String sourceHostname, String recipient)
    {
        // TODO Auto-generated method stub
        super.onOp(channel, sourceNick, sourceLogin, sourceHostname, recipient);
    }

    protected void onPart(String channel, String sender, String login, String hostname)
    {
        // TODO Auto-generated method stub
        super.onPart(channel, sender, login, hostname);
    }

    protected void onPrivateMessage(String sender, String login, String hostname,
        String message)
    {
        // TODO Auto-generated method stub
        super.onPrivateMessage(sender, login, hostname, message);
    }

    protected void onQuit(String sourceNick, String sourceLogin, String sourceHostname,
        String reason)
    {
        // TODO Auto-generated method stub
        super.onQuit(sourceNick, sourceLogin, sourceHostname, reason);
    }

    protected void onTopic(String channel, String topic, String setBy, long date,
        boolean changed)
    {
        // TODO Auto-generated method stub
        super.onTopic(channel, topic, setBy, date, changed);
    }
    
}
