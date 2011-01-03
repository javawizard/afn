Thinking on a NTW replacement that supports lots of additional stuff and is less tied to a particular widget toolkit (NTW is quite heavily influenced by GTK, which makes implementations written for other platforms somewhat difficult to write)...

It should have a handshake that's performed when a connection is established. The client informs the server of the features it supports, and the server has a list of features the app using it needs. If the client doesn't have all the requested features, the server sends them an error message and disconnects them.

If the application tries to use a feature the client has not said it supports, the server framework will throw an exception. This allows the client to require a specific set of features and allow some optional additional ones that will provide extra functionality but that aren't necessary. I might also make future versions of the server able to emulate features missing in some clients; for example, a tabbed pane could be roughly implemented by a vertical box having as its top component a horizontal box containing one toggle button per tab and as its bottom component a container whose contents are switched out as the user clicks on the toggle buttons in turn.

Things like check boxes and radio boxes allow the application to configure whether they change state when the user clicks on them or whether they just send an event back to the user. Same thing with toggle buttons, and most likely same thing with dropdown boxes. That would be useful.

Things like buttons that have an action signal dependent on whether the button is enabled also have a property specifying whether clicks received while the button is disabled will be filtered out on the server side.

State is tracked on the server side. All messages that go over the wire are asynchronous, with the exception of the initial handshake which must proceed in a defined order. This allows for the server-side library to be thread-safe while still allowing applications to block events while they mutate the UI. Event code is called on the thread that receives events from the server. This code can mutate the UI, which would result in sending packets to the client, which is why the protocol must be asynchronous.

RTK is thread-safe, but it has an event thread similar to Swing's event thread that allows for actions to be completed serially with receiving UI changes from the client. More accurately, there's one event thread per client and one thread to listen for new clients.

Metaview, the XML technology for creating a UI that mirrors data sources from various places like Autobus, will be an additional library that uses RTK to show stuff. This will allow Metaview to embed scripts in whatever language the user wants since it will run completely on the server-side.

There's essentially no support for MVC in RTK itself. Technologies like Metaview should be used if MVC support is needed.

RTK assumes that the network is at least reasonably fast. I've decided for now that listeners for events such as when a button is clicked will filter events on the server-side when the button is disabled. This will most likely simplify things a bit. I might add support for removing such filtering in the future, but I think it's going to make the server-side stuff a lot simpler for now.

Similarly, events are filtered out on the server side if a particular widget has been destroyed. Destroying a container recursively destroys its children, too.

To make it easier for viewers to be implemented in toolkits that do not support this, widgets cannot be reparented, and the parent must be specified when the widget is created. The only way to remove a widget from the container that it was created in is by destroying it. I might change the server-side library to add support for reparenting widgets at some point; the way it would do this is by destroying the widget on the client side and then re-creating the widget (and its children if the widget is a container) under the new parent. Not very efficient, but at least it would work.

A container's class represents what type of layout it performs. This mirrors GTK and GWT's use of containers and is easily compatible with Tkinter's packing approach and Java Swing's layout manager approach. I'm planning on four containers at this point, though I'm sure more will be added as optional features: HBox, VBox, Deck, and Table. HBox lays out its components horizontally. VBox lays out its components vertically. Deck shows only one of its components at a time; which component is currently showing can be changed programmatically, and could be used to implement a makeshift tabbed pane on systems that don't provide one. Table lays out its components in table cells. Components can take up multiple cells by specifying a rowspan and a colspan. Rows and columns in a table are required to dynamically size themselves; Tables must not restrict all rows and columns to being the same size. This could be implemented in Tkinter with grid packing and in Java Swing with TableLayout (a Google search should show where TableLayout can be obtained).

Oh, and of course, Frame is a container that does not have a parent. I'll add support for dialogs at some point in the future.

There are optional containers that viewers can support. The main one I'm thinking of right now is TabbedPane. This would correspond to a feature that a viewer can indicate its support for. Some optional containers (TabbedPane being among them) will be automatically implemented on the server side if the client indicates it does not support them. It's up to the particular server implementation how it wants to implement such containers; the Python server would implement a TabbedPane as a VBox containing two elements: an HBox containing one toggle button per tab and a Deck containing the components corresponding to the tabs.

Viewers providing tabbed panes have a feature indicating whether they allow the tab title to be a custom component.

I haven't yet decided what should happen if a tab or a tab title component is destroyed. The two possibilities are: 1, remove the tab, and 2, throw an exception (and require the tab to be removed by calling some method on the tabbed pane itself). They both have advantages and disadvantages.

Hmm... that brings up the additional complication of how parenting a component to be put into a TabbedPane should work, and particularly how that should work when the server is emulating a tabbed pane because of lack of client support. This needs thought, too.

Ok, I've decided how I'm going to do this for now. I'm not going to allow custom components in tab titles. I'll consider allowing that later. For now, what I'm going to do is have tabbed panes be just like ordinary containers with component constraints. And speaking of that, I should explain what exactly that is.

So, when a component is created, it has component-specific options (whether it's enabled, what its text should be, its state if it's a checkbox etc, the name of its group if it's a radio button etc) and it has layout options. Layout options are specific to the parent that the component's being created in and specify how the component should be displayed in that parent. The index in the container at which the component was inserted is tracked separately from all this. HBox and VBox don't need any constraints specified by their children. Table has constraints specifying the row and column a particular child should show up in, its colspan and rowspan, and so on. TabbedPane has the title of the tab under which the component should show. That's how tabbed panes are done. Just like any other container, a component in a tabbed pane is removed by destroying it, which removes the corresponding tab as well.

The protocol should be written such that containers, other than their properties, are all the same in functionality. This would allow the Python server to implement them all with one class that dynamically adjusts itself as needed. Virtual containers, those implemented on the server side, will have hooks that other containers will call (such as when a component is created under it or is destroyed under it). Basically, it needs to be easy to write an emulated TabbedPane.

Applications can create multiple frames on the client. Each has a title and four attributes specifying its location and size.

Widget attributes are either client attributes or server attributes. Client attributes are those specifying information from the client; they can only be modified by a message sent by the client, but the application can listen for changes to them. Server attributes are those specifying information from the server; they are modified by the application and sent to the client. A window's current size and position are examples of client-side attributes (set_size and set_location are functions that can be called to resize and move a window, but their changes are not visible in the local size properties until the message arrives back from the server). A window's title and a button's text are examples of server-side attributes.

Hmm, this attribute thing needs some rethought... Let's see what NTW does first...

Oh, and, the server should also be able to send back to the client what features it supports. I was thinking about this specifically in relation to event listeners. To simplify things, any event that can happen on the client side causes a message to be sent to the server side (except for mouse motion, which isn't supported right now). In the future, I'd like to change that so only events with listeners registered by the application are sent. This could be present as the send-only-on-listen feature: if the client sees that the server supports this feature, it will only send events for a widget if the server first tells it that the application would like to be notified when such an event occurs, and if the server sees this feature, it will send such notices when the application registers its first listener and send a notice that the event should no longer be sent when the application deregisters its last listener.

Ok so, I'm thinking there are client attributes and server attributes. Client attributes are /always/ set by the client and represent the last state known to be displaying in the client. These tend to lag behind attempts to change such attributes via functions because of network delays; therefore, the preferred way to look up state is to add a listener for a client-side attribute changing. (Actually, I don't know if I'll add such listeners yet or if there'll just be event listeners for other explicit events...)

Widgets have server attributes and client attributes. They also have layout attributes. Server attributes and client attributes are visible in the same namespace in the Python server library, which is on the object itself (and the allowed server and client attributes for each widget type are specified in some sort of tuple or something). Layout attributes are visible as the widget's layout attribute. This is a subscriptable object; assigning a map to it causes all the layout variables to be replaced in one go (the server side will programmatically diff the assigned value with the current value to figure out what attributes to set and delete). Assigning to a particular item of the object or deleting from an item causes that property to be set or removed, respectively.

At some point there might be custom functions for replacing all of a widget's server attributes at one time.

So I think I'm going to call server attributes widget attributes and client attributes state attributes. That should make clearer what their point is.

There should only be one class for actual widgets to avoid code redundancy. It should somehow be possible to specify each widget as the name of the widget on the client side, some predicate (which may be a lambda) that operates on the feature list sent by the client to determine whether or not the client supports the widget in question, and, optionally, a function or something that can be called, given a list of other features supported by the client, to determine a suitable virtual replacement widget.

So, let's imagine we were working at this from a perspective containing only resident (non-virtual) widgets. I could imagine two classes being present: ResidentWidgetConstructor and ResidentWidget. When a connection comes in, RTK performs the handshake and determines the features made available by the client and the corresponding set of resident widgets that are available. It creates a ResidentWidgetConstructor for each one and assigns it to the appropriate attribute on the connection/session object. When the ResidentWidgetConstructor is called, it creates a corresponding instance of ResidentWidget (which accepts into its constructor attributes such as the client-side name of the widget and the widget and layout attributes which should have been passed into the ResidentWidgetConstructor call) and invokes a function on it that sends the command to the client to create the widget. It also invokes a function on the specified parent ResidentWidget to tell it that it's now a child. Actually, it invokes this child-registering function first thing, which gives the ResidentWidget a chance to throw an exception if it's not a container.

When a ResidentWidget is destroyed, it would check its list of registered children and destroy each of them in sequence. (It needs to clone the list of children first since each destroy will remove that child from the list.) It then sends a message to the client that it's being destroyed, then looks up its parent (which it stores in a field) and removes itself from its parent's child widget list.

When a ResidentWidget has widget and layout attributes modified, it sends a message to the client indicating such. When the client modifies state attributes on the ResidentWidget... hmm... This needs to have some thought. Perhaps ResidentWidgets should also register themselves with a connection-specific id-to-widget map from which the widget can be looked up and told about events and changes to state attributes. Destroying a widget would remove it from this table.

So I'm thinking I'm just going to support resident widgets for now. I'll try to leave it open for adding virtual widgets later, but I think it's going to be more of a pain than it's worth at the moment. The main thing I forsee making it a pain is that the apparent layout attributes for a resident widget whose parent is a virtual widget are generally completely different than the actual layout attributes since the apparent layout attributes serve to instruct the virtual widget as to how to display the child but the real layout attributes are specified by the virtual widget when actually attaching the resident widget to an underlying resident container. So for now it's resident widgets only.

There'll still be a list of resident widgets and the features they depend on, but for now it won't have a function for generating a virtual widget if the features required aren't present.

So, the server library is going to be implemented in librtk. librtkviewer will contain the (embeddable and extendable) Tkinter viewer; that will be discussed in another file. This file is just going to discuss RTK itself and librtk.

Let's see... the protocol. Commands I can think of from server to client are create, destroy, reorder, set_widget, set_layout, call, and error. Commands the other way around are set_state, event, and error. Note that errors are not fatal; server-to-client errors show a message in a box to the user and client-to-server errors cause a message to be logged. If a fatal error happens, such as the server telling the client to create a component that it doesn't support, an error is sent and the connection is immediately dropped. To use the nonexistent component example, the client would send an error to the server and then immediately drop the connection, close all the windows, and pop up a dialog telling the user about the problem.

So, the protocol itself is json-based. It uses lines, similar to Autobus. Each line is a json object containing the following keys:

	action: The action to take, which is one of the commands above (create, destroy, event, etc).
	
	id: The id of the widget that this action pertains to, if it pertains to a widget (as most do)

Additional keys are present depending on the type of command. Since commands are always notifications, to borrow Autobus protocol terminology, there's no message_type and message_id keys or the like.

Now, let's start documenting the actions themselves. They are:

	create: Sent to the client to tell it to create a new widget. id is the new id for the widget. parent is the id of the widget that this widget is to be created under (which should be a container); this should not be present for toplevels such as windows. type is the name of the widget's type. index is the index at which the widget is to be created, which can range from 0 to the number of components currently in the widget, inclusive (with the latter indicating the widget should be added to the end). p_widget is a map containing the widget properties. p_layout is a map containing the layout properties.
	
	destroy: Sent to the client to tell it to destroy a widget. A container's children must be destroyed by the server before the container itself is destroyed; the client should report an error and drop the connection if the server tries to destroy a non-empty container.
	
	reorder: Sent to the client to tell it to logically reorder a widget within its parent. id is the id of the widget. index is the new index within its parent at which it should be located. Clients don't really have to reorder the widget; they simply have to make it show up in the container's layout as if it were present at that position.
	
	set_widget: Sent to the client to modify widget properties. properties is a key containing a map of properties whose values are to be set. Some properties can't be changed after the widget is created; these are documented on a per-widget basis, and attempting to change one of them will result in the client sending back an error and dropping the connection.
	
	set_layout: Same format and use as set_widget, but sets a widget's layout properties.
	
	call: Sent to the client to instruct it to call a function on a particular widget. The set of functions available for a widget varies, but they are generally things like set_state for checkboxes, switch_tab for tabbed panes, etc. Two keys are present, name and args, the former of which is the name of the function to call and the latter of which is a list of arguments to pass to the function.
	
	error: Sent either way to indicate an error. Right now, this has one key, text, which contains a textual description of the message. More keys will likely be added later.
	
	drop: Sent either way to indicate a huge error that means the respective client is about to disconnect.
	
	set_state: Same format as set_widget, but from client to server to notify of a change to a state property, and with one additional key: user, which is true if the change was a result of a user action (such as clicking a checkbox) and false if the change was a result of a function call (such as calling set_state on a checkbox). State changes are always sent before any events that may have been triggered by the same action.
	
	event: Sent to the server when a client-side event happens, after any set_state messages relating to the event have been sent. This contains three keys, name, args, and user. Name is the name of the event (such as "clicked" for a button or a checkbox), args is a list of the arguments to the event (which are widget-specific), and user has the same meaning as the user key in the set_state command.

These are all of the commands that I think are needed for now. Widget-specific commands (such as set_state for a checkbox or add_item or set_selection for a list or dropdown box) are implemented simply as calls.

When a client first connects, it sends what's called the request line. Right now, this contains three keys, action, features, and application. Action contains the string "connect". Features is a list of strings representing the features the client supports, but in the future this may contain additional keys for specifying a particular application to show (thereby allowing multiple applications to be served from a single port) or other such request parameters. Application may or may not be used by the server, and specifies the name of the application to view. This allows for the future potential for multiple applications to be served by the same server; in the future, there may be a protocol command to ask the viewer to load up an application with a particular name. (This also allows for the potential of future rtk URLs of the style rtk://server:port/application_name.)

The server then responds with the response line, which contains two keys which are the same format as the request's action and features keys (except that the action is "accept"). If the server decides that the client doesn't support enough features to run the application on even a basic level, it can instead send a packet with its action set to "drop" and one additional key, text, which contains the reason for the drop. Additional keys may be added to such a packet later.

Clients and servers should ignore keys in the connect and accept packets that they don't support.

Commands as specified above commence from then on.

I think that's just about it for the protocol. Now all that's left is to document the core set of widgets that should be supported.

So, let's see... The container types I'm thinking should be supported are HBox, VBox, Deck, Table, and TabbedPane. And, of course, Window. The presence of each of these widgets corresponds to a feature named widget:WidgetName, where WidgetName is the widget's type (so widget:HBox, widget:TabbedPane, etc).

The widget types I'm thinking should be supported, then, are Checkbox, RadioButton, ToggleButton, Button, TextBox, TextArea, ColorButton, DropdownBox, Label, ListBox, ProgressBar, ScrollBar, ScrollPane, Separator, and Slider. These are all matched by a corresponding widget:WidgetName feature same as containers. A viewer is not required to implement all these, particularly if the backing GUI toolkit it uses to display the application does not support one of these widgets. However, it may not be able to view all applications provided by all servers.

All widgets have a tooltip widget property that specifies the widget's tooltip. The server generally should not word-wrap this; it's up to the client to wrap it how it wants. It's also up to the client how to display this. Most clients will use tooltips to display these, but some clients, such as those running on pocket pcs, may not have the ability to detect mouse hovers and so might implement this with an extra "?" menubar option that, when clicked, opens a window displaying the tooltip of the widget clicked immediately thereafter. Clients that provide a meaningful display of the tooltip should provide the tooltips feature, but they should still allow the tooltip widget property to be set (and most likely ignore it) without causing a fatal error even if they do not provide any meaningful display of the tooltip.

So, the properties, events, etc supported by the widgets:

I decided to move them to afn-static/rtk/widgets.w so they're web-accessible on opengroove.org.






























