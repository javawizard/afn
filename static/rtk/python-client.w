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

This is another problem, I just realized, is how tkinter events should link with this whole newfangled system. And how that would work with asyncore. Tkinter generates events on its event thread, which could just be passed into the connection's various calls. When a message comes in, what should happen to it? Obviously it'll call the dispatcher function, probably on the event queue made available from the Protocol to prevent inbound messages from being run in parallel by accident. What happens thereafter?

I'm thinking that when a librtkinter viewer is constructed, you have to pass it a function that it can call to schedule events to be run on the tkinter event thread. This would then be called to perform the actual widget updates.

A per-connection map of widget type names to callables used to construct wrapping instances of that widget is used to figure out how to represent a new widget on creation. Resulting widget objects must support a number of functions, such as functions for adding children if they're a toplevel or a container, destroy, updating various attributes, etc. And there needs to be some way for these to be aware of their parents and children... and layout attributes... I'll think more about this tomorrow.

Ok, what if messages setting layout attributes get dispatched to the widget's parent? Reorderings would work the same way. Widgets... crud, this is starting to make this non-platform-agnostic anymore...

Oh wait, this is for librtkinter anyway, which doesn't need to be viewer-agnostic. So, we have one class per widget type that we're going to support, and we stuff those into a global map (the classes, not instances of those classes). Instances of these classes should have a field named widget that corresponds to the actual Tkinter widget they use. This is used by parents of the widget to lay out the widget in its parent.

Instances also have functions such as set_widget(properties) and set_layout(child, properties) (the latter of which is called on a widget's parent to update the layout properties for one of its children). call(name, args) and destroy() are also supported. add_child(child, layout_properties) is supported for containers and is called after the child is initialized. __init__(parent, connection, widget_properties) is supported; this should initialize self.widget with the specified properties and attach it to the specified parent's widget. It should also add any event listeners needed by the widget. After __init__ is called, parent and connection will be assigned by the connection automatically, so the widget doesn't need to worry about that. 

No, this is getting too complex to implement widgets. Perhaps the viewer shouldn't for now be extensible and should only support core tkinter types. Or something like that. But then we still have issues...

Ok, what about if we have... and then we have problems with the tkinter event loop...

Ok, let's do this. All inbound messages after the handshake are dispatched on the tkinter event loop using the function supplied to the connection. Each widget type is represented by a class. Create messages cause an instance of the widget class to be created. Its id, parent, connection, children, widget_properties, layout_properties, and state_properties attributes are initialized, the widget is added to the parent's children list at the index specified, and its setup function is called, which should initialize its widget field. Then the parent's setup_child function is called with the child and the index at which it was inserted as arguments. This will usually pack the widget as needed.

When a widget property is changed, widget_properties is updated and the widget's update_widget_properties function is called with the map containing the properties that changed. When a layout property is changed, layout_properties is updated and the widget's parent's update_layout_properties function is called, passing in the widget and the map of changed layout properties. When a widget is reordered, the parent's pre_reorder function, if it has one, is called, passing the child, the index at which the child is currently present, and the index to which the child will be moved. After this the widget is actually moved in its parent's child list and the parent's reorder function, if it has one, is called with the same arguments. I'll probably provide a utility function for computing which children shifted indexes (and in which direction) given the from and to indexes and the list of children.

When a call is received, the widget's call function is invoked. It should accept two arguments: the name of the call and the list of arguments passed to the call.

When a widget id destroyed, its parent's pre_destroy_child function, if any, is invoked, passing in the child. Then the child's destroy function is called, followed by the parent's destroy_child function if it exists (again passing in the child).

Connection maintains a map of existing widget ids to widget objects.

When events occur on the widget, it should call the connection's send_event or send_set_state functions, the latter of which I've still to work out some details as to how changes to those can be detected. Connection might even assign functions to the widget's send_event and send_set_state fields to simplify things.






















