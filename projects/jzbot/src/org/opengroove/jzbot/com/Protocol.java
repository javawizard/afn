package org.opengroove.jzbot.com;

/**
 * A protocol that jzbot can use to connect to a server.<br/>
 * <br/>
 * 
 * A protocol is responsible for making a connection to a particular room, and
 * for asking the user for more info if not enough was provided. For example,
 * the url bzflag://2.bztraining.org:5167 would be enough for the bzflag
 * protocol to connect to that server (provided a default username and password
 * had been configured), but irc://irc.freenode.net:6667 wouldn't provide enough
 * information, since the irc protocol requires the name of a channel to connect
 * to. irc://irc.freenode.net:6667/##6jet would be enough to connect.<br/>
 * <br/>
 * 
 * A protocol should automatically reconnect to any rooms that it might be
 * disconnected from. For an irc protocol, this would include rejoining all
 * rooms joined, and reauthenticating with nick services, if applicable.<br/>
 * <br/>
 * 
 * A protocol is expected to provide a nickname form of any users it knows
 * about, and an authenticated, canonical form of any users that it can
 * authenticate. The irc connector, for example, uses the user's nickname as
 * their nickname form and their hostname or cloak as the authenticated portion
 * of the user. The bzflag protocol uses the user's callsign as both the
 * nickname and the authenticated nickname, unless the user is not globally
 * identified, in which case the authenticated name is null.<br/>
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
 * should call JZBot.runExtendedEvent. This will run the factoid by the event's
 * name, if one exists, and notify any plugins For example, bzflag defines an
 * extended event called "ontk", which is triggered when a user on the server
 * kills a teammate.
 * 
 * @author Alexander Boyd
 * 
 */
public interface Protocol
{
    
}
