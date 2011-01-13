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

Ok, let's do this. All inbound messages after the handshake are dispatched on the tkinter event loop using the function supplied to the connection. Each widget type is represented by a class. Create messages cause an instance of the widget class to be created. Its id, parent, type, connection, children, widget_properties, layout_properties, and state_properties attributes (the latter of which is set to an empty dictionary) are initialized, the widget is added to the parent's children list at the index specified, and its setup function is called, which should initialize its widget field. Then the parent's setup_child function is called with the child and the index at which it was inserted as arguments. This will usually pack the widget as needed.

When a widget property is changed, widget_properties is updated and the widget's update_widget_properties function is called with the map containing the properties that changed. When a layout property is changed, layout_properties is updated and the widget's parent's update_layout_properties function is called, passing in the widget and the map of changed layout properties. When a widget is reordered, the parent's pre_reorder function, if it has one, is called, passing the child, the index at which the child is currently present, and the index to which the child will be moved. After this the widget is actually moved in its parent's child list and the parent's reorder function, if it has one, is called with the same arguments. I'll probably provide a utility function for computing which children shifted indexes (and in which direction) given the from and to indexes and the list of children.

When a call is received, the widget's call function is invoked. It should accept two arguments: the name of the call and the list of arguments passed to the call.

When a widget id destroyed, its parent's pre_destroy_child function, if any, is invoked, passing in the child. Then the child's destroy function is called, followed by the parent's destroy_child function if it exists (again passing in the child).

Connection maintains a map of existing widget ids to widget objects.

When events occur on the widget, it should call the connection's send_event or send_set_state functions, the latter of which I've still to work out some details as to how changes to those can be detected. Connection might even assign functions to the widget's send_event and send_set_state fields to simplify things.

So I'm thinking widgets should have two fields: widget and container, the latter being only required for containers or toplevels. The Widget class, which most widgets would extend from, would, by default, implement container to be a property that returns the value of the widget field. Setting the value of this property would overwrite the property itself with the specified value. The idea behind separating widget and container is that widget represents the widget as visible to the widget's parent and container represents the widget that children should be attached to. This allows for composite widgets where the overall widget might not be where children are to be attached.

I think that should be enough to get started writing librtkinter. I'm thinking if applications want to have windows display in a specific parent instead of as an instance of Tk as they normally would, they should replace Window (potentially with their widget subclassing Window to allow it to use Tk instances some times but not others) or provide a custom function that sometimes returns Window instances and sometimes doesn't.

Oh, and rtkinter itself. At this point I'll just have it be command-line invoked, so you do rtkinter localhost 12345, or rtkinter rtk://localhost:12345 in the future, and it loads librtkinter, sets up a queue and a function that schedules itself once every eighth of a second and runs whatever's on the queue, and starts the application running. In the future, rtkinter will likely override Window to check and make sure there aren't more than a certain number of windows being displayed to prevent attacks that could arise from that, then it'd delegate over to Window to do the actual construction. (the replacement function could curry in the connection object to check how many windows there are.)

Ok, let's see if we can implement Window, VBox, Button, and Label. That would allow, for example, a program that shows two buttons and a label, vertically stacked. The upper button could be named "up" and the lower button could be named "down". The label could show 0, and every time the upper button's clicked it goes up, and every time the lower button's clicked it goes down. It could share state across all viewers so that if one user clicks up all of the users see the label go up.

So I got all those implemented, but now the line between librtkclient and librtkinter is getting confusing, and I think rtkinter is ending up implementing a lot of what librtkclient really could. For example, librtkclient doesn't track objects for each widget; I think it should since most toolkits would have such a notion.

In fact, I think the only two things at this point that are specific to Tkinter in librtkinter are the master passed into the dispatcher and the individual classes representing each widget. This is rather stupid to have all of this functionality that could be shared in librtkinter. I'm thinking I'll rewrite librtkclient to incorporate the functionality of librtkinter.Dispatcher into librtkclient.Connection.

The one potential problem I can see with this is that widget toolkits such as Tkinter provide an event thread (accessed via the after_idle function) on which UI-updating code should be run. Perhaps that could be solved by having an optional function that can be passed into the connection. It would delegate to this function for /all/ code that uses any UI mechanisms (including the connection's local map of widgets), passing in a no-argument function (which would usually be a partial application of one of connection's functions) to be run. Tkinter users would simply specify master.after_idle as this function.

The other problem, of course, is how to inform widgets, and specifically windows, of what master they should use. Here I'm thinking that perhaps the simplest way is going to be to set tkinter_master as a field on the connection (that librtkclient has no idea about); Window would then use the value stored in this field. This breaks encapsulation a bit but would likely be less verbose and cumbersome than having, for example, a context argument that the connection stores and makes available to child widgets.

So let's see... We no longer have a notion of dispatchers. All of the functions of a dispatcher should now be integrated into librtkclient.Connection. All of the functions are run using the UI toolkit event function passed into the connection, which is quite separate from the event system made available by the protocol.

So, the functions that we have (and these roughly correspond to messages in the rtk protocol) are:

* '''create''': This creates a new widget. It's passed the id, parent, type, index, widget properties, and layout properties of the widget. It instantiates the widget, which should subclass librtkclient.Widget, passing in the specified values (and a copy of the connection itself and the wiget's intended parent). Widget sets self.widget_properties and self.layout_properties and other fields (such as send_event) similarly to what librtkinter.TkinterDispatcher.create currently does. create then adds the widget to the parent's list of children (or the connection's list of children if the widget is a toplevel), then calls the parent's pre_setup, the child's setup, and the parent's post_setup functions.
** These three function types (pre_something on the parent, passing in the child, then something on the child with no arguments, then post_something on the parent, again passing in the child) are going to be referred to as the tri-function set. All of these are optional for any particular widget or container. Additional arguments mentioned are passed after the child in the case of the pre_something and post_something functions on the parent.
* '''destroy''': This destroys a widget. It's passed the id of the widget to destroy. It gets the widget out of the widget map, recurses on any children the widget has (in case the server forgets to destroy children; it should iterate over a copy of the child map), calls the tri-function destroy set on it, then removes it from the widget map and its parent's list of children (or the connection's list of toplevels if it's a toplevel).
* '''set_widget''': This sets a set of properties on a particular widget. It's passed the id of the widget and a map of properties to set. It gets the widget out of the map, updates its widget_properties map, and calls the single function set_widget, passing in the map of changed properties.
* '''set_layout''': This sets a set of layout properties on a particular widget. It's passed the id of the widget and a map of layout properties to set. It gets the widget out of the map, updates its layout_properties map, and calls the single function set_layout on the widget's parent, passing the widget and the properties that changed.
* '''close_widgets''': This destroys all of the toplevels using the destroy function (which recurses on any of the widget's children in case the server misses some of them or gets disconnected while destroying children). It should copy the list of toplevels since this list will be modified by the destroy function. It then calls the connection's close function. close_widgets will be called on the UI toolkit's event thread when the connection to the client is lost.

Widgets would subclass from librtkclient.Widget. librtkinter.Widget would subclass from librtkclient.Widget, and all Tkinter widgets in turn would subclass from this. 





























