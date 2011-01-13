This page documents all of the widgets available to RTK programs.

= Containers =
== HBox ==
This container lays its contents out horizontally. Components are laid out left to right.

* No widget properties.
* No layout properties.
* No state properties.
* No functions.
* No events.

== VBox ==
Same as HBox, but lays out its components top to bottom instead of left to right.

* No widget properties.
* No layout properties.
* No state properties.
* No functions.
* No events.

== BorderPanel ==
A container that can contain exactly one child element. It places a border around the child. The border's size, style, and color can be set; clients are not required to support all of these, and the ones they do support can be discovered by the presence of the features widget:BorderPanel:size, widget:BorderPanel:style, and widget:BorderPanel:color. In the future, features will specify the availability of particular styles.

* Widget properties:
** '''size''': The size, in pixels, of the border.
** '''style''': The style of the border. Right now this is a string, one of "solid", "raised", "etched", "sunken", etc. TODO: decide on the supported names, and perhaps write a function in connection that chooses the first supported border type of a list of types based on the connection's features

== Deck ==
This container shows only one of its children at a time.

* Widget properties:
** '''index''': The 0-based index of the child that should show. 
* No layout properties.
* No state properties.
* No functions.
* No events.

== TabbedPane ==
This container shows a series of tabs, one per child.

* No widget properties.
* Layout properties:
** '''title''': The text that should be used as the tab's title.
** '''tooltip''': The text that should be used as the tab title's tooltip. This is optional.
* State properties:
** '''index''': The 0-based index of the currently-selected tab.
* Functions:
** '''switch(index)''': Switches the tabbed pane to show the tab at the specified index.
* No events.

== Table ==
This container lays out its components in a grid. Rows and columns take on the size of the largest component in the row or column, and components can take up more than one cell.

* No widget properties.
* Layout properties:
** '''row''': The row that the widget should be placed on.
** '''col''': The column that the widget should be placed on.
** '''rowspan''': How many rows high the widget should be.
** '''colspan''': How many columns wide the widget should be.
* No state properties.
* No functions.
* No events.

== Window ==
This container represents a toplevel window. Unlike other widgets, windows do not have a parent. They can only contain one widget; adding a second widget without removing the first will cause a fatal client-side error.

* Widget properties:
** '''title''': The title of the window.
* No layout properties.
* State properties:
** '''size''': The size of the window, as a list of length 2.
* Functions:
** '''pack()''': Resizes the window to fit the components it contains.
** '''resize(width, height)''': Resizes the window to the specified dimensions.
** '''relocate(x, y)''': Moves the window to a different location on the screen.
** '''center()''': Centers the window on the screen.
* Events:
** '''close_request()''': Fired when the user clicks the window's close button. Most applications will want to have this destroy the window, which they could do in, for example, the Python RTK library with window.close_request.listen(window.destroy).

= Widgets =
== Checkbox ==
This widget is a checkbox. It lets the user click on it to change its state.

* Widget properties:
** '''text''': The text of this checkbox
** '''flip''': True if clicking on this checkbox toggles whether or not it's marked, false if it doesn't. Most UI toolkits act as if this was true; setting this to false causes a click to fire the clicked event but neither the checkbox's state nor the state attribute will change.
* State properties:
** '''state''': True if this checkbox is currently marked, false if it is not.
* Functions:
** '''set_state(state)''': Sets whether or not this checkbox is marked.
** '''mark()''': Same as set_state(true).
** '''clear()''': Same as set_state(false).
* Events:
** '''clicked()''': Fired when this checkbox is clicked. This is fired even if the state is not changed because flip is set to false.

== RadioButton ==
This widget is a radio button. It's similar to a checkbox, but it appears different, generally can't be cleared by the user, and can be grouped with other radio buttons. Only one radio button in a group can be selected at a time. The radio button's group cannot be changed after it's created; attempting to do so will cause a fatal client-side error.

Selection changes, whether server-generated or user-generated, result in a change to the state property of the radio button losing selection being sent before the change to the state property of the radio button gaining selection.

* Widget properties:
** '''text''': The text of this radio button
** '''group''': The name of the group that this radio button belongs to. This can be any arbitrary string; any radio buttons with the same group name are considered to be in the same group.
** '''flip''': Same as flip on a checkbox. However, the state property of a radio button with flip set to false can still be changed if another radio button in the same group with flip set to true is clicked, causing this radio button to lose selection.
* State properties:
** '''state''': True if this radio button is currently marked, false if it is not.
* Functions:
** '''set_state(state)''': Sets whether or not this radio button is marked. If this is called with true as the argument, any other selected radio button in this radio button's group will lose selection.
** '''mark()''': Same as set_state(true).
** '''clear()''': Same as set_state(false).
* Events:
** '''clicked()''': Same as the clicked event on checkboxes.

== ToggleButton ==
A toggle button is exactly identical to a checkbox in behavior, but it displays as a button instead of as a checkbox. Refer to checkbox's documentation for the attributes, functions, and events supported by this widget.

== Button ==
This widget is a button that can be clicked. Clicking a button causes an event to be fired.

* Widget properties:
** '''text''': The text of this button
* No state properties.
* No functions.
* Events:
** '''clicked()''': Fired when this button is clicked.

== TextBox ==
TODO: This widget needs to be thought out, especially how often updates as to the current text should be sent back to the client and whether they should be sent as a diff, and what should happen if the server attempts to modify the text field's text and if it should provide a diff or what.

== TextArea ==
TODO: Same problems with TextBox apply here.

== ColorButton ==
This widget is a button that, when clicked, opens a color chooser allowing the user to select a color.

* No widget properties.
* State properties:
** '''color''': The currently-selected color, as a list of three integers in the order red/green/blue, which are all in the range 0-255.
* No functions.
* No events.

== DropdownBox ==
== Label ==
This widget shows a piece of text that the user cannot change. It's typically used for labeling UI fields.

* Widget properties:
** '''text''': The text that should show in the label
* No state properties.
* No functions.
* No events.

== Spacer ==
This widget is an empty region of space that has a fixed width and height.

* Widget properties:
** '''width''': The width of the spacer, in pixels
** '''height''': The height of the spacer, in pixels
* No state properties.
* No functions.
* No events.

== ListBox ==
== ProgressBar ==
== ScrollBar ==
== ScrollPane ==
== Separator ==

