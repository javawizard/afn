So, there's going to be four components to this whole mess: librtkclient, librtkinter, rtkinter, and newtviewer:

* librtkclient is a client library for RTK. It's completely toolkit-agnostic, meaning that it can be used for any RTK client written in Python.
* librtkinter is a viewer library that uses Tkinter. It can be embedded in any Tkinter application to allow viewing of RTK apps.
* rtkinter is a standalone viewer that uses librtkinter to view an RTK app.
* newtviewer is a standalone viewer using librtkclient that uses newt to show an RTK UI in a terminal.

First let's figure out librtkclient. Ideally it should be written agnostic of the backend communications layer so that some viewers could use asyncore and some viewers could use threads. Perhaps it should use the same function set and subclassing mechanism as the server Connection object does.

Hmm... That complicates some other things...

Ok, perhaps it should be done that way and then an instance of Dispatcher is passed into the connection (along with the socket). And, of course, the list of features. When the handshake is completed, the dispatcher would be initialized, passing in the connection (from which it could also extract the list of features the server supports). Whenever a message arrives from the server, a function pertaining to the action is called. The connection's send function can be called to send a message outward.

Hmm... What if we change Connection to be concrete? A Protocol subclass (or perhaps just a class defining the methods Protocol requires) could be passed into Connection. The server-side Connection class could be made to work similarly. Client-side Connection would also accept a Dispatcher that gets called when the handshake over the protocol has finished, as documented above.

Connection could probably also have functions like send_set_state and send_event to help with sending stuff. And, of course, Connection.close.

I think that should just about take care of librtkclient. Then we get to librtkinter.

Most of the widgets have items (think Tkinter.Button["text"]) corresponding pretty much directly to rtk widget properties. I think this is also true for state properties, although I'm not exactly sure how listeners and tkinter work.

This is another problem, I just realized, is how tkinter events should link with this whole newfangled system. And how that would work with asyncore. Tkinter generates events on its event thread, which could just be passed into the connection's various calls.






















