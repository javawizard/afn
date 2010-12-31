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

Connection's constructor also optionally accepts an argument representing a function to call when there's data to write and a function to call when the connection is to be closed. There's also a public method on Connection that can be called when data is available for it and a method that can be called if/when the connection dies. The functions deal with actual Python dicts and lists; it's up to the program using them to use something such as json.dumps to convert them to text before sending them off or the other way aroun. These mechanisms allow an external transport to be created, or even, for that matter, for a viewer to be embedded locally thus turning rtk into a traditional UI toolkit with the advantages (such as being threadsafe) that it offers. If these functions are not specified to Connection's constructor, it will create an InputThread and an OutputThread and link them (in the case of OutputThread, by means of a queue) into those functions. 








































