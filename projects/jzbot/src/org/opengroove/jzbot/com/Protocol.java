package org.opengroove.jzbot.com;

import java.net.URI;
import java.net.URL;

import org.opengroove.jzbot.plugins.Message;

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
 * "irc://irc.freenode.net/?a=jcp!unaffiliated/javawizard2539".<br/>
 * <br/>
 * 
 * Users have both a nickname and an authenticated name. The authenticated name
 * should be persistent; that is, it should not change for a particular user
 * over multiple sessions. The nickname, however, can change, and is what is
 * used when the bot needs to include a name for the user in some sort of text.
 * The authenticated name can be null if the user can't be authenticated. IRC
 * uses the user's nickname plus their hostname or hostmask to authenticate
 * them, and bzflag uses the user's callsign if they are globally identified
 * (server-local identification does not count).<br/>
 * <br/>
 * 
 * User URLs can contain a fragment portion, which generally represents
 * information on how the user should be contacted, and is protocol-specific.
 * The only protocol that uses this right now is Facebook, and it allows the
 * fragment to be chat, message, or wall, which represent contacting the user
 * (and being contacted by the user) by sending a chat message, sending a
 * regular message, or posting on the user's wall, respectively. When comparing
 * users for equality and authentication, the fragment portion is dropped.<br/>
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
     *         with a username and password). * @throws InvalidInputException if
     *         <tt>options</tt> does not contain the options that this protocol
     *         was expecting. The message of the exception will be sent back to
     *         the user if this method call was a result of ~join, and if this
     *         method call was an initial join, then the server will be
     *         deactivated. ~proc edit jz_roulette
     */
    public String connect(URI requester, URI server, String options);
    
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
     * Returns the user uri that corresponds to this bot on the specified
     * server. This should return a uri even if the bot is not connected to the
     * server specified as long as it has been requested to. For example, if it
     * has been instructed to join an IRC server, but the IRC server is still
     * processing the request, then this method should sill return a uri,
     * although it would probably be a nickname uri instead of an authenticated
     * uri.
     * 
     * @param server
     * @return
     */
    public URI getSelfUri(URI server);
    
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
     * @throws InvalidInputException
     *             if <tt>options</tt> does not contain the options that this
     *             protocol was expecting. The message of the exception will be
     *             sent back to the user if this method call was a result of
     *             ~join, and if this method call was an initial join, then the
     *             server will be deactivated. ~proc edit jz_roulette
     */
    public String join(URI requester, URI room, String options);
    
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
     *         why if the user is not allowed to become an op. For example,
     *         bzflag only allows serverops, not room ops on a server, since
     *         there is only one room on a server.
     */
    public String allowAddOp(URI requester, URI user, URI target, boolean server);
    
    /**
     * Sends the messages specified to all room ops in the specified room. IRC
     * sends these as a pm to each room op, and bzflag sends these as a message
     * to the admin team.
     * 
     * @param room
     *            The room to send to
     * @param message
     *            The message to send
     */
    public void sendRoomOpMessage(URI room, Message[] messages);
    
    /**
     * sends some messages to the specified room or user.
     * 
     * @param target
     *            The target, which will either be a room or a user
     * @param messages
     *            The message to send
     * @param source
     *            An array of messages that is the exact same length as
     *            <tt>messages</tt>. For each message in <tt>messages</tt>, the
     *            message in <tt>source</tt> is the message that caused this one
     *            to be sent, or null if the source message isn't from this user
     *            or isn't known. This is typically present when the message
     *            being sent here is in response to a command invocation.
     * @throws NotAllowedException
     *             if the protocol is not allowed to send a message to that
     *             target. For example, IRC users can't send messages to
     *             channels they haven't joined, and AIM bots can't send
     *             messages to users that haven't started a conversation with
     *             them and haven't added them to their buddy list.
     */
    public void sendMessage(URI target, Message[] messages, Message[] source);
    
    /**
     * Returns true if this protocol supports actions, and false if this
     * protocol does not support actions. Actions can still be handed to the
     * protocol, though, and it should convert these to standard messages
     * however it likes. This is more of a hint to jzbot.
     * 
     * @return
     */
    public boolean isActionSupported();
    
    /**
     * Indicates that the user is attempting to run a protocol-specific command.
     * Protocol-specific commands are executed by running the command
     * "protocol PROTOCOLNAME COMMAND". Technically, the protocol itself would
     * be able to see this and filter it out before it even gets to jzbot, but
     * this method is preferred, since it allows jzbot to enforce that only
     * superops can use the protocol command.<br/>
     * <br/>
     * 
     * The protocol should call its own sendMessage method to send a reply,
     * passing in <tt>source</tt> as the target.
     * 
     * @param source
     *            The source of the command. This is either a room, if the
     *            command was sent to a room, or a user, if the command was sent
     *            in a pm.
     * @param user
     *            The user that sent the command
     * @param command
     *            The command itself
     */
    public void protocolSpecificCommand(URI source, URI user, String command);
    
    /**
     * Returns a normalized form of the specified server url. This should remove
     * the port if it is the protocol's default port, and otherwise convert the
     * url so that all urls that reference a given server are converted to the
     * same url string. The returned url's path component should be empty.
     * 
     * @param uri
     * @return
     */
    public URI normalizeServerUrl(URI uri);
    
    /**
     * Returns an authenticated uri for the specified user uri. This should
     * return null if the user cannot be authenticated. The returned uri should
     * be canonical, and should not be a nickname uri.
     * 
     * @param uri
     * @return
     */
    public URI getAuthenticatedUrl(URI uri);
    
    /**
     * Returns a nickname uri from the specified uri. This should never return
     * null, and should always return a canonical uri (meaning that two
     * different uris that denote the same actual user should cause the same uri
     * to be returned from this method).
     * 
     * @param uri
     * @return
     */
    public URI getNicknameUrl(URI uri);
    
    /**
     * Validates that the specified new options for the server specified are
     * valid. This doesn't necessarily need to validate that any credentials
     * specified are valid; only that the general format is valid. In
     * otherwords, this method should not incure a request to the server.
     * 
     * @param server
     * @param options
     */
    public void validateNewServerOptions(URI server, String options);
    
    /**
     * Validates that the specified new room options are valid.
     * 
     * @param room
     * @param options
     */
    public void validateNewRoomOptions(URI room, String options);
    
    /**
     * Requests that the user in question be kicked from the room in question.
     * In general, this will only be called if jzbot is a room op at the
     * specified room. IRC servers kick the user off the room, and bzflag
     * servers kick the user off the server.
     * 
     * @param user
     * @param room
     * @param requester
     *            The user that requested the kick, or null if one isn't known
     *            (for example, if some internal function of the bot made the
     *            decision to kick all on its own)
     */
    public void kick(URI user, URI room, URI requester);
    
    /**
     * Requests that the user in question be banned from the room in question.
     * In general, this will only be called when the bot is a room op at the
     * specified room.
     * 
     * @param user
     * @param room
     * @param duration
     * @param message
     */
    public void ban(URI user, URI room, URI requester, long duration, String message);
    
    /**
     * Returns a display name for the specified user. This is the name that the
     * user should be referenced as when referring to them in messages. This
     * should also be a name that will ping the user, if the protocol supports
     * it. It does not, however, have to be the user's nickname (as specified by
     * the nickname uri), although it can be. IRC uses the user's nickname
     * 
     * @param user
     * @return
     */
    public String getDisplayName(URI user);
}
