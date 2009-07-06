package jw.jzbot.com.script;

import jw.jzbot.com.irc.IRCConnection;

/**
 * Allows for connecting to IRC servers and sending messages and such. This is a
 * lightweight wrapper around most of the PircBot methods. It allows a
 * connection (an IRCConnection) to be made to a server, and then listeners (in
 * the form of javascript functions) can be added to the connection, and the
 * connection's settings can be modified.
 * 
 * @author amboyd
 * 
 */
public class IRCProtocol
{
    /**
     * Creates a connection, and attempts to connect it.
     * 
     * @param nick
     *            The nickname to use
     * @param password
     *            The password to log in with
     * @param server
     *            The server to connect to
     * @param port
     *            The port to use
     * @param reconnect
     *            True to automatically reconnect if the connection is lost,
     *            false not to
     * @return A new connection
     */
    public IRCConnection connect(String nick, String password, String server,
            String port, boolean reconnect)
    {
        return null;
    }
}
