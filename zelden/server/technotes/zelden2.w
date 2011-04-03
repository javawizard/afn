Second revision in Zelden's design.

So... Thinking logs will be XML. Python has support for Zorba, an XQuery processor, so this could be used in some form or other.

Thinking either attributes specific to a protocol for a particular log entry (sender's nick, sender's hostname, etc) should either be stored on a tag with the protocol name in it (irc-options or some such, for example) or should be stored in their own namespace.

==UI==
I'm thinking a lot of UI stuff should be HTML. Not all, but a lot. When viewing a conversation, the contents would be HTML. Granted, the server only keeps track of an HTML snippet for each event in the log, which allows clients to remove these snippets from scrollback if the display of the message is getting to be lengthy.

===Events===
On that note, I should probably mention I'm thinking of making the log html viewer and the conversation html viewer fundamentally the same, with only some minor differences in rendering (such as that tapping on a nickname in the live view would likely pull up a menu of actions one can perform on that user). My thought was to allow renderers for event types to be registered, and these would be responsible for converting from the data for an event to the HTML used to display that event.
