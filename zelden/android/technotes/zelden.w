Zelden is a distributed IRC client. It's much the same as Quassel in what it does, but the design and functionality are a little bit different.

The protocol is plain-text. It's similar to IRC's protocol in that it's line-oriented, but the underlying content of these lines differs significantly from IRC. I'm still working out the specifics.

A Zelden server has a single password. It can connect to multiple IRC servers but it can't authenticate clients with separate passwords. This reflects the idea that a single Zelden server will be used by a single person from multiple devices that they connect from.

Connection authentication is done by challenge-response. When the client first connects, it sends a message telling the server hi. This is so that the server doesn't just blindly broadcast information about what it is, the idea being to prevent naive port scanners from picking up information on Zelden.

The server responds to this with a random string. The client appends the user's password to the end of that string and sends back a hash (protocol TBD) of this resulting string. The server computes the same hash from the user's password it has stored; if they match, it sends back a message telling the client that authentication was successful. If they don't, it sends back a message to the client indicating such and disconnects the client.

I haven't worked out the specifics of what's sent back after that, but I think it will somehow comprise the list of servers and channels (channels that are remembered, not just ones that are currently joined, but this would include whether or not the channel is currently joined), the list of favorite users (the ones queried with ISON by the server on a regular basis, and potentially added to the server's user watchlist or whatever it's called if I can figure out how to work it), and the current values of all of the server's settings (potentially along with their names and descriptions; I haven't yet decided if the client or the server is the one that stores this information). It then indicates it's done sending initial state. Then, if there are any outstanding highlights, it sends them as normal highlights.

Zelden Server stores in the logs not only all of the incoming and outgoing IRC messages but also events such as when a client connects or disconnects or when a client attaches or detaches to or from a channel (which I'll cover in a bit). Each client provides a name to use when it authenticates, which is used here. I haven't yet decide what will happen if two clients try to connect with the same name, but I'm thinking the old client will be disconnected to allow for the new client to connect.

Zelden has a concept of conversation attachment. A conversation is simply an ongoing grouping of messages, the most common type of which are channels. PMs with users are also a type of conversation, and each server has a conversation containing informational messages from the server such as the motd.

Conversations have a type, which, right now, is one of channel, user, or special. Clients can attach to and detach from conversations at will. When a client connects, it starts out as not attached to any conversations.

Zelden Server only sends the client chat messages occurring at conversations that it's attached to. This is important in saving bandwidth; it's usually pointless to receive every message at every channel on an IRC network that a client on, say, a cell phone is connected to, since they likely won't read even 5% of those messages. Zelden Server still sends the client a message when a special event, such as the user getting kicked, receiving a mode, or being highlighted, occurs. It's then up to the client to notify the user and attach to the channel when the user sees the notification and so chooses.

When a client wishes to attach to a conversation, it sends a message to the server telling it so. In this message, it includes the timestamp of the last message it has in its local log, if it keeps logs (otherwise it should send some date far enough in the past that modern computers didn't even exist then), and the maximum number of playback messages it wants the server to send. The server will then send either every message after the specified timestamp or the specified number of messages, whichever contains less messages.

The idea with this whole playback join thing is that a particular client can indicate that it only keeps, for example, 100 messages in the buffer at a time, so it would only like 100 playback messages at a maximum (since it would just discard the rest anyway), but that it might already have some messages in the buffer in the case of the client being previously attached and only having detached for a short amount of time, in which case the messages the client already has in its buffer won't be re-sent to it by the server.

After playback is complete, the server sends a message indicating so. I'm not yet sure what this would be used for, but it might come in useful for some clients. I'm also thinking the server might log when a particular client attaches to and detaches from a particular channel, in which case the two messages (the end-of-playback message and the you-attached message from the log) would come one after the other.

Timestamps are stored according to the server's time, but they're sent in milliseconds since January 1, 1970 UTC, so the client can show them in the local timezone.

Zelden Server maintains a list of favorite channels and possibly a list of favorite users. This is shared among all clients. It also allows clients to view and search logs on request, and it will probably manage notifications, meaning that a client can specify that the user has responded to a notification and that the notification should therefore be dismissed on all other clients.

Channels can either be enabled or disabled. Enabled channels are those that the server will automatically try to join (and, if configured, will keep attempting to join after a kick at a frequency which can be configured). Disabled channels are those that the server will not try to join, and will indeed try to part from if it's currently joined to them. An enabled channel to which the server is not currently joined because of a ban or some similar circumstance will most likely show up different to the client, although the client can ignore the flags specifying the difference since this will be included in messages sent to the client.

Servers can also be disabled.

An arbitrary number of nicknames (perhaps I may cap this at five) can be specified for each server, along with a setting specifying whether the server will switch to the highest-priority nickname if it becomes available. NickServ authentication is also supported, and there will probably be some generic mechanism to perform actions at server connect (with the ability to mask parts of the action in case I decide to make NickServ authentication a part of this).

So I've decided setting information will be stored on the server, which would allow for plugins to create settings themselves if future support for plugins is added. At the top level of the settings hierarchy there can be any number of settings and folders. Each setting has a path, a type (integer, boolean, single-line text, multi-line text, choice, etc), a human-readable name, and a human-readable description. Folders are essentially a particular type of setting that can contain other settings.

==Multi-protocol==
So I'm thinking I'm going to have Zelden have support for multiple protocols like Pidgin does. Support for various protocols can be added by means of some sort of plugin that would be written for Zelden Server. Zelden Client wouldn't have a notion of plugins for now; all pluggable functionality would have to be implemented as server-side plugins.

So, we need to add features present in other non-IRC protocols that might be used. Some of those that I'm thinking of are:

 * File transfers (which should obviously be stored on Zelden Server and relayed to clients as needed
 * Embedded images (although I might just have those show up as file transfers)
 * Inline message formatting
 * Sending the timestamp of a message's arrival as reported by the protocol in addition to the timestamp at which Zelden Server was first informed of the message (TBD: figure out how this works with servers that send us the last 30 messages at a room in XMPP even if we've already received them before)
 * Aliasing individual users and possibly rooms, although information on the users' and rooms' actual names should be made available in some form or other in the buddy list
 * User information, above what IRC itself supports (username, hostname, gecos, etc). This may need some consideration about how and where to show this information. Perhaps the plugin can report a string (or two or something) to display additional info about the user in the buddy list, and it can decide what it wants to put in there (or it can leave it blank).
 * Presence. IRC should use periodic ISON and away message detection to update an IRC user's preference, while XMPP would use the presence messages sent to it by the server.
 * Some form of passing contact information to the plugin so it can update the backing contact store if there is one (XMPP's roster, for example)
 * Potentially groups, to make contact organization easier. Each contact within a group should /clearly/ show the protocol and account they're coming from, perhaps in a small message below the contact or something, and it should be possible to view contacts temporarily grouped by protocol and account if one wanted to.

Ok, we need to make sure this stays simple so that it's not too difficult to create a client for it. I'll be writing both a GTK+ client for Linux, Mac, and Windows, and an Android client, and possibly an iOS client if I decide to at some point. So this needs to be simple.

I'm thinking the message markup language should be really simple but not based on HTML to prevent against possible attacks.

Hm... I'm also thinking that perhaps the settings schema (the set of all available settings, along with their names, types, descriptions, etc) shouldn't be modifiable at runtime, meaning that the schema wouldn't ever change without Zelden Server restarting. This would result in the client not needing to have any special functionality for updating the setting schema and its UI representation during a connection; it can instead create the entire UI on connect and destroy it on disconnect. I think this will work well.

So I'm also thinking that I'm not going to worry about formatting in message text for now. I'll just have messages be plain-text, and once I finish everything else up I'll add support for formatting in messages.

I'm also thinking there should be a concept of active conversations, which are separate from attached conversations but are more like the ones the user has "open" right now. This might map one-to-one with the windows that are opened in the Zelden desktop client, but I'm not sure as that could have the potential to intrude on privacy if a user opens a conversation on their phone not expecting it to also open on their desktop machine. More thought needs to be put into this; perhaps the active conversations (or open conversations, if I decide to use that terminology) could show up in the right-click menu (or left-click menu if there is one, or they could be the same menu, just either type of click opens the menu) after all the other items (like "exit", "show launchbar", etc). Perhaps pending notifications could show up here too.

Also, the server should be able to create virtual conversations where it can issue messages to its specific operation. Plugins should either be able to create these or have one assigned to them (and perhaps another one for each protocol or something) that they can use for messages. These should support notifications, and maybe the level of notifications should be configurable, either on a per-server or per-client basis.

Notifications are also server-managed, meaning that notifications show up on all devices until one dismisses a notification; all others act as if it had been dismissed there too. Perhaps there should be a way to indicate that a notification should not be dismissed on a particular computer, although maybe that should be added later after the rest of Zelden is working.

It should also be possible to remotely hide all conversation windows in a client or tell the client to just hide itself period except for the tray icon, in case someone leaves a computer unattended and doesn't want others reading through their conversations.
























