package org.opengroove.jzbot.com;

import java.net.URI;
import java.net.URL;

/**
 * A protocol that jzbot can use to connect to a server.<br/>
 * <br/>
 * 
 * A protocol is responsible for making a connection to a particular server and
 * for joining rooms on that server. Protocols are always told to connect to a
 * server before they are told to join a room on that server. Protocols must
 * also validate that the room given is a valid one. The room can even be the
 * empty string, if this is valid (as it is for the bzflag protocol, since each
 * server is its own room).<br/>
 * <br/>
 * 
 * A protocol should automatically reconnect to a server or room if it is
 * unexpectedly disconnected from one. It does not currently need to notify
 * jzbot that it is doing this.<br/>
 * <br/>
 * 
 * Users are currently server-specific. There is no such thing as a user that
 * only exists on a particular room, but would be a different user on another
 * room. When referencing a user, the URL consists of the server that the user
 * is on, and either the query parameter a or the query parameter n. n is the
 * user's nickname and a is the user's authenticated name. Either form is valid,
 * but if the user is authenticated, then the authenticated form should be used
 * to validate authentication. For example, jcp at freenode could be referenced
 * by either "irc://irc.freenode.net/?n=jcp" or
 * "irc://irc.freenode.net/?a=unaffiliated/javawizard2539".<br/>
 * <br/>
 * 
 * Users have both a nickname and an authenticated name. The authenticated name
 * should be persistent; that is, it should not change for a particular user
 * over multiple sessions. The nickname, however, can change, and is what is
 * used when the bot needs to include a name for the user in some sort of text.
 * The authenticated name should never be null. IRC uses the user's hostname or
 * hostmask as the authenticated name, and <br/>
 * <br/>
 * 
 * Wherever a textual URL is used, the pound sign is treated as a literal
 * character instead of as the URL fragment character. This makes it so that a
 * URL like "irc://irc.freenode.net/##6jet" is a legal URL that references the
 * room ##6jet on the server irc.freenode.net.<br/>
 * <br/>
 * 
 * A protocol should be able to return a list of operators in a specific room.
 * What exactly an operator is is specific to a room. For example, bzflag
 * operators are those who are admins on a server or those who have identified
 * with /password. irc operators are channel ops. This functionality is only
 * partially used right now, in that room operators can kick jzbot from the room
 * or ban him (which causes jzbot to decline all requests to rejoin for a
 * specific amount of time).<br/>
 * <br/>
 * 
 * A protocol is responsible for notifying jzbot when a user joins or leaves a
 * room. It is also responsible for notifying jzbot when a message is sent in
 * the room.<br/>
 * <br/>
 * 
 * If a protocol has other events besides the standard join and part events that
 * it wants to allow jzbot to create a factoid for, then it should provide these
 * via the getExtendedEvents method. Then, when one of these events occurs, it
 * should call runExtendedEvent on the context. This will run the factoid by the
 * event's name (prefixed with the word "on"), if one exists, and notify any
 * plugins about the extended event. For example, bzflag defines an extended
 * event called "tk", which is triggered when a user on the server kills a
 * teammate. This would notify plugins that the "tk" extended event has been
 * triggered, and would run the factoid "ontk" if one exists.<br/>
 * <br/>
 * 
 * Protocols are also responsible for handling kicks and bans by jzbot, and
 * reporting to the bot whether it is authorized to kick and ban. Additionally,
 * the protocol is responsible for making jzbot aware of whether a ban duration
 * can be set, or whether all bans are indefinite, and whether or not a ban
 * message can be specified.<br/>
 * <br/>
 * 
 * Operators are specified as being a superop, a serverop, or an op. A protocol
 * is responsible for telling jzbot if a particular user is allowed as an op in
 * one of these groups. For example, bzflag denies all requests to become a
 * standard op, and only allows serverops. An op at one level is also an op at
 * any level below that, so superops are serverops at every server, and
 * serverops are standard ops at every room on that server.<br/>
 * <br/>
 * 
 * Only one instance of this class is created per protocol.<br/>
 * <br/>
 * 
 * 
 * 
 * @author Alexander Boyd
 * 
 */
public interface Protocol
{
    public void init(ProtocolContext context);
    
    /**
     * Gets the name of the protocol. This is what should appear in the scheme
     * part of all urls for this protocol.
     * 
     * @return
     */
    public String getName();
    
    /**
     * Returns true if a duration can be specified on a ban. If this is false,
     * then any bans will be indefinite, regardless of the duration specified.
     * 
     * @return
     */
    public boolean banDurationAllowed();
    
    /**
     * Returns true if this protocol supports ban messages. What exactly this
     * means is specific to the protocol. BZFlag servers, for example, tell the
     * user the ban message when the user tries to join but has been banned.
     * 
     * @return
     */
    public boolean banMessageAllowed();
    
    /**
     * Connects to the specified server, but doesn't join any rooms. This should
     * only return once the protocol is reasonably certain that connecting to
     * the server will be successful.
     * 
     * @param requester
     *            The requester of the join, or null if this is being called at
     *            system startup
     * @param server
     *            The server to join
     * @return null if this was successful, or the reason why if this failed
     *         (which could be because the user hasn't configured the protocol
     *         with a username and password).
     */
    public String connect(URI requester, URI server);
    
    /**
     * Disconnects from the specified server.
     * 
     * @param requester
     *            The user that requested the disconnect, or null if one isn't
     *            available
     * @param server
     *            The server to disconnect from
     * @return True if the disconnect succeeded, or the reason why if it failed
     */
    public String disconnect(URI requester, URI server);
    
    /**
     * Joins a particular room. If the join is because a serverop or a superop
     * used the join command, then <tt>requester</tt> is the url of the person
     * who requested the join. If the join is because JZBot is starting up and
     * this is a room that JZBot is supposed to join, then <tt>requester</tt> is
     * null.
     * 
     * @param requester
     *            The requester that requested the join
     * @param room
     *            The room to join
     * @return null if the join was successful, or the reason why if the join
     *         failed.
     */
    public String join(URI requester, URI room);
    
    /**
     * Leaves the specified room. This may or may not be called right before
     * shutdown, depending on what caused the shutdown.
     * 
     * @param requester
     *            The requester. This is the person that used the leave command,
     *            or the person that started a shutdown.
     * @param room
     *            The room to leave
     * @param isShutdown
     *            true if this is because a user requested shutdown, false if
     *            this is because of the leave command being used
     * @return
     */
    public String leave(URI requester, URI room, boolean isShutdown);
    
    /**
     * Returns null if the specified user is allowed to be added as an op for
     * the specified target. JZBot will validate that the requester is an op for
     * the target, so this does not need to be done here.
     * 
     * @param requester
     *            The person attempting to add a new op. This person will be an
     *            op for the target specified.
     * @param user
     *            The user that is to be added as an op. This will always be an
     *            authenticated url.
     * @param target
     *            The target. This is the url that the user will be an op at.
     * @param server
     *            True if this user is being added as a serverop (and
     *            <tt>target</tt> will be a server url, not a room url), false
     *            if this user is being added as a standard op (and
     *            <tt>target</tt> will be a room url, not a server url).
     * @return null if the user is allowed to be added as an op, and the reason
     *         why if the user is not allowed to become an op.
     */
    public String allowAddOp(URI requester, URI user, URI target, boolean server);
    
    /**
     * Sends a message to all room ops at the specified room. IRC sends these as
     * a pm to each room op at the specified channel, and bzflag sends these as
     * messages to the admin team.
     * 
     * @param room
     *            The room to send to
     * @param message
     *            The message to send
     */
    public void sendRoomOpMessage(URI room, String message);
    
    /**
     * Same as sendRoomOpMesssage, but sends it as an action.
     * 
     * @param room
     * @param message
     */
    public void sendRoomOpAction(URI room, String message);
    
    /**
     * Sends a message to the room.
     * 
     * @param room
     * @param message
     */
    public void sendMessage(URI room, String message);
    
    /**
     * Sends an action to the room.
     * 
     * @param room
     * @param message
     */
    public void sendAction(URI room, String message);
}
