
module Zelden.IRC where

import Zelden.Protocol
import Control.Concurrent.STM
import Control.Concurrent.STM.SaneTChan
import qualified Network.IRC.Base as I
import qualified Data.Map as M

data IRCProtocol = IRCProtocol

data IRCConnection
    = IRCConnection {
        getConfigVars :: M.Map String String
    }

data IRCSession
    = IRCSession {
        getIrcConnection :: IRCConnection,
        getReader :: Endpoint I.Message,
        getWriter :: Queue I.Message
    }

instance Protocol IRCProtocol where
    likesPastebin _ = True
    createConnection _ callback = do
        return IRCConnection

type IRCM a = ReaderT IRCSession (MaybeT IO)

{-
So, how are we going to actually do connections?

Well, let's have a dispatch thread that does all of the network communication.
(Oh, and I'm thinking the largeish bits of code that handle handshaking can be
in MaybeT or something, with a function in MaybeT to get the next value from
the queue representing the socket or abort with Nothing if the enabled/disabled
TVar gets set to disabled, which would automatically take care of disconnecting
us when we're requested to disconnect, even if in the middle of a handshake.
The same function could also use a timeout to abort with Nothing if the server
doesn't respond in a timely manner.)

So, the dispatch thread (using the function mentioned above) sits there and
retries until enabled is true. Then it makes one connection attempt to the
server, and once that's done and the server's disconnected, it sleeps for a
certain number of seconds (in the future, increasing depending on how many
times the connect attempt failed) and then goes back to the top, where it waits
for enabled to be true.

To make a connection attempt, the dispatch thread opens a socket (with a
timeout) to the server, creates queues and endpoints for it, then enters the
MaybeT IO block of code. That bit writes the USER message out, then writes
(incidentally, I'm thinking of having the read function also automatically
filter out PINGs and respond with PONGs accordingly, which will get us proper
PING/PONG support within handshakes, as some servers like to do) NICK messages
in a loop waiting for something other than the nick already in use status code.
Once it gets it, it stuffs it back onto the socket endpoint and continues.

Then we send a Connected event (TODO: are we allowed to issue Disconnected
multiple times? I'm tempted to say no, in which case we'll also need to store
into a TVar that we issued a Connected so that when we're done with the attempt
we need to issue a Disconnected) since once we stop getting nick already in use
back we know what our user key will be. Then we log all of the messages we
receive up to 376 (end of motd), when the handshake is done. We'll also store
off the 005 messages that arrive in some sort of capabilities dictionary, and
in particular we'll store off the PREFIX bit, which we'll use to map modes to
prefixes. (Most IRC client libraries I've seen aren't smart enough to work out
the server's prefixes, and instead rely on a hard-coded list; I really want
this IRC library to automatically detect what's what, and perhaps use a sane
default if the server never sends a 005 PREFIX.)

Then we go into normal message receive mode, where we deal with each message
that we receive as we receive it.

When we receive a privmsg, join channel, part channel, kick, quit, and so on,
we just issue the corresponding event.

When we receive a message that we don't recognize, we issue a server custom
event with information about what happened. (TODO: create such an event, and
figure out how to specify a user key that means a server, or just use our
actual server's name for now; need to think about how to do this. IRC doesn't
really care which server the message came from, but XMPP does, as a message
from opengroove.org is very different from a message from talk.google.com.)

Oh, when we finish the handshake, we also store off into a variable that we're
now connected. (Or maybe we just don't issue the Connected event until after
we've received the 376, and rely on Zelden not to send us stuff if we haven't
issued Connected yet. Although maybe that's a bad idea, as that would seem
prone to race conditions.)

Actually no, IRC servers seem perfectly content to ignore you (or rather, send
you an error response, I forget which code now) if you try to join channels or
send messages while in the middle of a handshake, so I suppose that doesn't
matter quite as much. Although might still be a good idea to filter things out
if we're in the middle of a handshake.

So yeah, right before we send Connected, we set a variable indicating that
we're now connected, and right before we send Disconnected, we unset said
variable.

Then, when we receive a request from Zelden to join a channel or send a message
or things like that, we check to see if that var saying we're connected says
we're connected, and if not, we either ignore it or log to the server log that
we're not connected, not sure which yet. If we are connected, though, then we
convert it into an appropriate IRC message and send it off on its merry way.

I think that should be just about it for now.

(Zelden's going to do the work of keeping track of the current list of users at
any given channel, so we don't need to worry about that. I will probably have
that as a separate library that can be used independently from Zelden, though,
so that IRC bots can use that library on top of this one without using the rest
of Zelden, and Zelden can use it to 1: keep track of current state, and maybe,
depending on how I store things, 2: use it to run simulations of what a channel
might have looked like at a particular time. #2 will be something I'll probably
wait on, though, so I'll just plan on #1 for now.)

Also, it might be a good idea to be able to pass to our little read function
a custom timeout variable (or multiple timeout variables) to time out on in
addition to the one it creates. This would allow an overall timeout for how
long the handshake can take to be created just before the handshake starts and
passed throughout the handshake. Also might want to create a ReaderT on the
queue and endpoint for reading/writing from/to the socket, which would make the
read function even simpler; could also have the ReaderT contain a list of
timeout variables for read to additionally make use of, as well as the amount
of time that the timeout it creates should be for. Probably should use a custom
data type for this.

We ought also to have the logic for processing ordinary everyday messages from
the connection be inside MaybeT as well, so that the few messages that require
us to stop and wait for other messages (such as the initial topic on a join; we
wait for the part that indicates who initially set it) can just operate as
typical calls to read and still abort properly when we're asked to disconnect.
-}

instance Connection IRCConnection where

