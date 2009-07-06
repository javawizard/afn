package jw.jzbot.com;

/**
 * A protocol that the bot can use to communicate.<br/><br/>
 * 
 * If the protocol wishes to expose protocol-specific functionality to the
 * scripting-side of the bot, it can do this simply by adding extra public
 * methods to the protocol class. The protocol object itself is made available
 * to the script, and the script sees all public methods on the protocol object
 * as functions and all public fields as properties.<br/><br/>
 * 
 * This also means that a protocol should make all methods private or
 * package-scope that it doesn't want scripts to have access to. Scripts can
 * subclass other classes, so protected visibility does not help.<br/><br/>
 * 
 * The current protocol implementation only accounts for private messages (but
 * different routes within those methods, such as a facebook message vs. a
 * facebook wall post) and named chat rooms.
 * 
 * @author amboyd
 * 
 */
public interface Protocol
{
    /**
     * Connects to the specified server, throwing an exception if an error
     * occurs. This can be called multiple times for the same server, but with
     * different credentials, to establish multiple connections to the server.
     * 
     * @param server
     *            The server to connect to
     * @param credentials
     *            The credentials to use. This is a protcol-specific format, and
     *            an exception can be thrown if the credentials are not in an
     *            expected format.
     */
    public void connect(String server, String credentials);
    
    /**
     * Disconnects from the specified server.
     * 
     * @param server
     *            The server to disconnect from
     * @param credentials
     *            The credentials supplied when connecting
     */
    public void disconnect(String server, String credentials);
    
    /**
     * TODO: figure out recipient format, should authnames/nicknames be in java
     * or javascript? And therefore should the name be protocol-specific, and
     * javascript wrapper code handles it? Or should there not even be a
     * protocol interface, and protocol classes are created and then used by the
     * script to connect?
     * 
     * @param message
     */
    public void sendMessage(String message);
}
