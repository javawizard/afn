So, thoughts on how to do the Python RTK server...

(and I'm copying a lot of this from afn-python/technotes/rtk.w, which I intend to get rid of soon)

So, connections. There should be a class called Server. This simply listens for connections, and when one arrives, it creates a Connection instance and hands the socket off to it, then tells it to start itself. The connection class should be externally visible so that server applications that want to use some other channel to establish a connection (or even some different technology altogether, as long as it emulates a few of the functions present on socket) can do so and create a Connection from it.

Server, in its constructor, takes either a hostname and port or a server socket. It also takes a function that will be invoked with a connection object whenever one is received, and a feature validator. It listens for connections on the specified socket, and when a connection is received, it creates a Connection and hands it off to it, along with the function to be invoked for new connections and the validator.

Connection sets up an input thread, an output thread, and an event thread. I'm thinking the input thread and the output thread will be instances of libautobus.InputThread and libautobus.OutputThread, respectively, and I'm thinking I might split the I/O side of libautobus into some external library like libjsonio or something. The event thread will be a thread that reads callables from a queue and invokes them.

The input thread, however, is configured to direct the first message it receives at the handshake queue, which is an object present on the connection. Once the threads are set up, the connection starts a thread to perform the handshake.

This thread (which invokes handshake() on the connection) reads from the handshake queue for the handshake packet sent by the client. It then checks to see if the handshake works out by calling the validator, telling it what features the client supports. If the validator throws a MissingFeatures, it sends back an error as the response and closes the connection. If it doesn't, the server sends a response indicating the features it supports and starts up the event thread. It then calls the function that's supposed to be called when connections are ready.

Connections have a map that maps live widget ids to the corresponding ResidentWidget objects. This is used when events are received to get the widget to dispatch the event to. When an event arrives, it's dispatched to the corresponding widget itself; Connection doesn't do any further processing on the event. The same thing happens with changes to state properties.

Connection also has a (something, figure out what the heck was supposed to go here, cause I went to bed before finishing that sentence and now I can't remember what I was going to put).

InputThread, OutputThread, and the socket are all linked together the same way as they are in libautobus. When the input thread detects an end of stream, it closes the socket and shuts down the output thread. In this way, the only thing that needs to be done by application code to kill a connection is to close the socket, which there's a function for (that performs the SHUT_RDWR and close). Connections shouldn't be closed like this until all of the output thread's data has been sent, and to that end the client should be the last thing to send any data before the server drops the connection (unless it's because of some issue where the server doesn't particularly care if they screw the client over, such as if the client's flooding them to try and slow stuff down). I'm adding an additional quit command. If the user closes the client, it sends that to the server and disconnects. If the server wants to end the connection, it sends destroy messages for all of the widgets, then sends a quit. The client will reply back to that with a quit and then close the connection. The server should wait several seconds for a reply to the quit message and only close the connection when the reply is received or if it doesn't come for some time.

So, server-side widgets are represented by instances of ResidentWidget. This can be created by instances of two classes, which are callable: ResidentWidgetConstructor and ThreadLocalWidgetConstructor, the latter of which simply looks up the thread-local connection and invokes an instance of the former.

There's a list of widgets built-in to librtk as some global field specifying the valid containers and widgets, along with their widget properties, state properties layout properties (for containers), etc. (This includes the default value for each state property that should be used before the client sends us its initial value.) This is used to construct ThreadLocalWidgetConstructor instances in librtk.threadlocal. Additionally, each Server receives a copy of this and stores it. This copy is what's referenced throughout the rest of the code, which allows outside code to add additional... Crap, this doesn't take into account features specifying stuff about widgets... or anything... Hmm... External code needs to be able to modify this to add new widget types...

Oh, and Connection needs to be able to operate without Server present....

Ok... so the validator passed into a connection (and passed into a server, which passes it into connections it creates) is responsible for checking the feature list and deciding whether it's acceptable or not. Perhaps, then, there should be a list of functions passed into the connection that will be called on the clone of the structure representing available widgets and properties and events and calls etc along with the list of features supported by the client. These functions can mutate the structure as needed, adding additional widgets, changing attributes, removing widgets or attributes, etc, as needed. Actually, this could tie in with the validator system, them both being the same; if one of these functions throws a MissingFeatures, the connection is dropped with the str() of the MissingFeatures thrown.

Additional widget types, then, would be registered by adding a function to this list that checks to see if the feature corresponding to the specified widget is present and, if it is, adds entries corresponding to that widget to the list. Requiring a particular widget type could be done by adding a function that raises a MissingFeatures if the widget's feature is not in the list. There would also be callable objects such as AndFilter, OrFilter, and NotFilter. AndFilter's constructor is variadic and, when the corresponding AddFeatures instance is called, calls all of the objects passed into the constructor, propagating the first MissingFeatures thrown if an object throws it. OrFilter calls all of its constructor objects until one does /not/ throw a MissingFeatures. If all of them do, it raises a MissingFeatures combining the messages from all of its children. NotFilter raises a MissingFeatures if its child does not and vice versa.

Actually, the widget structure could be an empty map when created, and Connection by default would add, as the first function, a function that clones RTK's built-in widget structure. The rest of the functions could then mutate this as needed.

So, Connection and Server both store this list of functions and allow additional functions to be added after they're created, although they shouldn't be added after their respective start functions are called. Server simply passes a clone of this list to Connection, which uses it as the initial list.

Connection's constructor also optionally accepts an argument representing a function to call when there's data to write and a function to call when the connection is to be closed. There's also a public method on Connection that can be called when data is available for it and a method that can be called if/when the connection dies. The functions deal with actual Python dicts and lists; it's up to the program using them to use something such as json.dumps to convert them to text before sending them off or the other way around. These mechanisms allow an external transport to be created, or even, for that matter, for a viewer to be embedded locally thus turning rtk into a traditional UI toolkit with the advantages (such as being threadsafe) that it offers. If these functions are not specified to Connection's constructor, it will create an InputThread and an OutputThread and link them (in the case of OutputThread, by means of a queue) into those functions.

Ok, changing this. Those optional arguments are now /required/, and Connection itself has no clue about any socket-level stuff. Two classes, ThreadedServer and AsyncoreServer, would be included with librtk by default, and these would implement the network knowledge. ThreadedServer, once it receives a connection, would hand it off to create_threaded_connection, which would create a new Connection with a pair of libautobus I/O threads to process data and an event thread to process events.

Changing this yet again. Connection doesn't accept arguments anymore. Instead, classes derive from it and provide implementations for protocol_init, protocol_start, protocol_send, protocol_close, and protocol_event_ready, which work like so:

* protocol_init is called by Connection.__init__ after all else is set up.
* protocol_start is called by Connection.start, and indicates to the protocol that it can start calling protocol_receive and protocol_connection_lost as needed (it should not call either of these two before protocol_start is called). protocol_start is guaranteed to have been called once Connection.start returns, so single-threaded systems can just make sure that they call Connection.start() on a connection before going back to code that would have the potential to call protocol_receive or protocol_connection_lost.
* protocol_send is called when Connection has an object it wants sent.
* protocol_close is called when Connection wants to close the connection; the subclass should make sure all data previously queued with protocol_send is sent before closing the connection.
* protocol_event_ready is called when a new event is to be called; ThreadedServer adds such events to the queue read from by the event dispatch thread. protocol_event_ready must not block and run the event then and there, as the event might cause further events to be generated. Instead, events should be queued up and processed later on or in another thread. All events must be processed serially and in order, meaning that no two events can run at the same time and events must run in precisely the order they were passed into protocol_event_ready.

Connection provides two functions for protocols to use, protocol_receive and protocol_connection_lost. protocol_receive should be called by the subclass when a message has arrived and is ready for Connection to process. That function will most likely cause other things to happen, like calls to protocol_event_ready for events that need to be processed, or protocol_send for sending a response to the handshake. These will all happen synchronously while protocol_receive is waiting. protocol_connection_lost can be called when the connection has been lost. If the connection has already been shut down, it does nothing. Otherwise, it shuts down the connection and calls protocol_close. It's possible that the connection might call protocol_send and protocol_event_ready right up until protocol_close is called; data from protocol_send can be discarded but events from protocol_event_ready must be processed. After protocol_close is called, no matter for what reason, no other protocol functions will be called.

So, Connection itself.

There's a map, as I was mentioning before, of widget ids (ones that haven't yet been destroyed) to widgets (ResidentWidget objects). When a set_state packet or an event packet is received, Connection looks up the corresponding ResidentWidget and dispatches it to that widget, ignoring the event if there's no such widget (which would happen if the widget had just barely been destroyed by the application while an event from the client was still going through the network to the server).

So I just realized that ResidentWidgetConstructor instances don't need to be assigned to a particular connection. They can derive the connection from the parent they're constructed on. The parent of a toplevel widget could be the connection itself (although it would likely be stored in a different field). When a widget is created, the parent is specified, and it sets self.connection to parent.connection. In that way the connection is handed down to each widget. If the widget is a toplevel widget, then it ensures that its parent is the connection and sets it on itself. A partially-applied ResidentWidgetConstructor for a connection on the Window toplevel is then set as the connection's Window attribute. Or maybe it should just be a function that calls the ResidentWidgetConstructor. Same thing either way, except that the latter could have a docstring, which might be useful.

So, let's walk through the life of an application. A subclass of Connection is created. This creates the widget-id-to-widget map, sets the socket passed into Connection into the socket field, sets up other relevant fields, and calls protocol_init. The connection is then set up as needed, perhaps adding additional validator functions or what not, and then the connection's start function is invoked. Right now, this would simply call protocol_start since the handshake is initiated by the client. protocol_receive will then be called at some point. It will check self.handshake_finished, which will be False, so it will interpret that packet as the handshake. It will extract the feature set and run it through the validators, also handing them the list of supported widgets. If any of them raise a MissingFeatures, protocol_receive sends a packet (via the send function, which I'm thinking for now will simply delegate to protocol_send) to the client telling them that the handshake failed and we're about to drop them, and then terminates the connection with Connection.close (which I'm also thinking will just delegate to protocol_close). If they don't raise MissingFeatures, the server part of the handshake is sent to the client, and the handshake is considered successful (and self.handshake_finished is set to True). There's one more thing this does, however.

That thing is the calling of the function passed to the Connection to be called when the connection has been successfully set up. Or more accurately, Python's partial function application is used to create a partially-applied object which will pass the connection into the function, and this object is passed into dispatch_event, which right now simply delegates to protocol_event_ready.

Oh, and the list of features supported by the client is stored in a field on the connection object, along with the list of supported widgets generated by the validators. And ResidentWidgetConstructor instances (which can be defined with a parent, in which case they don't need one when actually calling them; in this case the parent is self, namely the connection object) are created for all of the toplevel types in the supported widget list and assigned into fields on the connection object. And that's it for connection setup. The invocation of protocol_receive that triggered the handshake stuff now returns, and that's it.

So, the protocol eventually gets around to running our startup function, and it decides to create a toplevel Window object by calling connection.Window(title="Hello world"). This invokes the instance of ResidentWidgetConstructor assigned to the Window field of the connection object, which was previously constructed from the information in the list of supported widgets and the connection itself (so the call doesn't need a parent). ResidentWidgetConstructor creates a new ResidentWidget, passing in the name of the widget and the connection as the parent and any additional keyword arguments. ResidentWidget's __init__ function gets all of this stuff. It generates a new id for itself and sets it as self.id, then checks to see if the passed-in parent is an instance of Connection. If it is, it stores in a local variable that this is supposed to be a toplevel and stores it in the connection field. If it isn't, it stores in a local variable that this is supposed to be a non-toplevel, and it sets the connection field from the specified parent's connection field. Then it uses the connection to get information about the widget that it's supposed to be. If this widget's toplevelness doesn't match how the ResidentWidget was constructed, it raises an exception about the problem.

If it matches, it then stores down the widget's property info and validates the keyword arguments passed in and stores their values off, probably in a dictionary. Layout attributes are validated against the widget's parent (toplevels can't have layout attributes).

The widget, in the process of the stuff above, should check and make sure that the parent really is a container. It should also validate that all required layout attributes are present. It then tells the parent to add this widget to itself. Right now all the parent does is add this widget to its list of child widgets and send off to the connection to tell the client to add the widget (which is done by a call to child.send_add, which takes as an argument the id of the actual parent that the widget is to be added to). If the parent is a connection, it calls the same function instead on the connection, which adds the widget to its list of toplevels and sends a message to the client to create it. I'm thinking there should be a function to look up the parent or something, returning the connection object (which has several methods in common with widgets) if the widget is a toplevel. [UPDATE: This has been added, in the form of the property owner on ResidentWidget instances.]

Widgets have a destroy function. When this is called, it destroys all children (iterating over a copy of the list, since each child destroyed will cause itself to be removed from the list) if it's a container, then looks up the parent's destroy_child function and calls it. Widgets implement this to remove the widget from the list of child widgets and send a message to the client telling it to remove the widget. Connections implement this to remove the widget from the list of toplevels and send a message to the client telling it to remove the widget. I'm thinking widgets should have a function called send_destroy that the parent calls in its destroy_child to send the destroy message. This function would be the one to actually send the destroy message.

Widgets have a reorder function that allows the widget's position in its parent to be changed. This delegates to the parent's reorder_child function, which validates the new index, moves the widget in the child list, and calls child.send_reorder with the new index.

When an attribute is set on a widget, the widget looks to see if it's present in object.__getattr__(self, "__dict__"). If it is, it sets it into the dict and returns. If it's not, it looks through its list of attribute names to figure out what type of attribute it is, and then performs an action for the attribute change:

* If it's a call, an event, or a state attribute, it throws an exception saying that you can't set the value of such an attribute.
* If it's a widget attribute, it updates its local map of widget attributes and calls send_set_widget, which sends the attribute change off.
* If it's a layout attribute, it updates its local map of layout attributes and calls parent.set_child_layout. ResidentWidget implements this to call child.send_set_layout to send off the change.

When an attribute is retrieved on a widget, the widget looks through its list of attribute names to figure out what the type of attribute it is. If it's not an attribute, it throws an exception. Otherwise, it looks up the attribute's value:

* If it's a widget attribute, it gets the attribute from its local map of widget attributes and returns it.
* If it's a layout attribute, it gets the attribute from its local map of layout attributes and returns it.
* It it's a state attribute, it gets the attribute from its local map of state attributes. This map is updated only by messages received from the client, I.E. there's no way for the user to change state attributes (which is how rtk is supposed to work).
* If it's a call, it creates a Call for the specified call and returns it. Call instances are callable objects that are created by specifying a widget id and the name of the call. Attribute value validation may be performed at some point in the future.
* If it's an event, it gets the Event object, which will have been created at ResidentWidget initialization time for every supported event, from a local map of Event objects and returns it. Event is a class that has methods for adding, removing, and firing listeners, so an event named "clicked", for example, would typically be listened for by doing widget.clicked.listen(some_function). (connect would likely be a synonym for listen for convenience to users coming from GTK.)

When an event or a state change arrives from the server, it pushes the change onto the event list. When the connection gets round to running the event, it looks up in its map of widget ids to widgets to find the widget, then calls its incoming_event or incoming_state_change function. ResidentWidget's implementation of incoming_event looks up the Event object and fires all of its listeners, and its implementation of incoming_state_change sets the new value of the state attribute in the state attribute map and fires any listeners that may have been added on it (which I'm still trying to decide how such listeners should be registered; at this point I'm thinking an Event should be created for each state attribute and stored in a map, and a state attribute named "text"'s event would be available as the widget attribute "text_changed"; these events would then be fired when the value of the text attribute is changed).

When a connection's close function is called, it checks to make sure it hasn't already been closed. If it has, it simply returns. If it hasn't, it checks its one optional argument to see if it should perform a hard close. Normally this is false, in which case the connection destroys all of its toplevels (iterating over a clone of its list of toplevels since destroying each one will cause it to remove itself from the list). If this is true, the connection skips this step, and goes straight to the next one, which is making itself as having been closed and then calling protocol_close.

protocol_connection_lost calls close, telling it to perform a hard close so that it doesn't actually send any messages. As usual, this will skip closing if the connection's already been closed, which allows the underlying connection to call protocol_connection_lost regardless of whether or not the connection being lost was as a result of protocol_close being called.

The structure containing the list of available widgets stored in the connection and constructed by the list of validators is a map. Keys are names of widget classes. Values are seven-element lists ('''not''' tuples so that they can be modified by validators); the first element is the name of the widget that should be used in actual protocol packets, the second element is one of the constants TOPLEVEL, CONTAINER, or WIDGET, and the remaining five elements are lists of widget attributes, layout attributes (which should be the empty list or None for widgets that are not TOPLEVEL or CONTAINER), state attributes, calls, and events, respectively.

Each item in the list of widget and layout attributes is another list of three elements. The first is the name of the attribute, the second is whether or not the attribute can be modified after the widget is created, and the third is the default value for the attribute or None if the attribute must be specified when the widget is constructed.

The list of state attributes contains lists of two items: the name of the attribute and the default value that the attribute should have before the client sends a message with the attribute's value for the first time.

Each list in the list of calls contains only one item right now, the name of the call. I might add argument validation in the future.

Each list in the list of events contains only one item for now, the name of the event. This is a list so it can easily be extended in the future.

So I've decided I'm going to call this whole structure the widget schema.

A QUICK NOTE: The user key sent back by events is ignored at present. In the future, this will likely be put into a threadlocal by the function that runs on the event thread just before it calls the event listeners or the state change listeners.









































