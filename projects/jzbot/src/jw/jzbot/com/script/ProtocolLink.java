package jw.jzbot.com.script;

/**
 * A protocol link. This does nothing other than to ensure that all connections
 * are disconnected when the bot is reloaded.
 * 
 * @author amboyd
 * 
 */
public interface ProtocolLink
{
    public void shutdown();
}
