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
 * identified, in which case the authenticated name is null.
 * 
 * @author Alexander Boyd
 * 
 */
public interface Protocol
{
    
}
