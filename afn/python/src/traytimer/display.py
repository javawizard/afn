
import gobject
from gobject import idle_add
import gtk
from concurrent import as_new_thread

"""
A module that knows how to show windows browsing timer set models. Instances
of TimerSetDisplay can be created, passing in a controller object from which
the model will be obtained and to which all timer modifications by the user
will be sent.

When using an instance of TimerSetDisplay to view a timer server connected over
Autobus, the Autobus proxy representing the interface registered by the timer
server should be passed in as the controller object. TimerSetDisplay will take
care of everything else. Due to a current bug in libautobus, the proxy must be
obtained and the TimerSetDisplay constructed *before* calling the connection's
connect() or start_connecting() functions for the first time.

NOTE: Applications using this class must import gobject and call
gobject.threads_init() before creating instances of this class. Failure to do
so will result in Python deadlocking.
"""

class TimerSetDisplay(object):
    def __init__(self, title, controller, prefs_function):
        """
        Creates a new TimerSetDisplay whose window will use the specified
        title. The display will also use the specified cotroller object.
        When the preferences button on the toolbar is clicked, the specified
        preferences function will be called in order to let the application
        itself deal with preferences.
        """
        self.title = title
        self.controller = controller
        self.prefs_function = prefs_function
        self.builder = gtk.Builder()
        self.builder.add_from_file("glade/traytimer/traytimer.xml")
        self.builder.connect_signals(self)
        self.window = self.builder.get_object("window")
        self.tabs = self.builder.get_object("tabs")
        controller["object":"timers"].watch(self.timers_object_changed)
        controller["object":"startup"].watch(self.startup_object_changed)
    
    @as_new_thread
    def on_timer_add(self, button):
        number = self.controller.create()
        idle_add(self.switch_to_tab, number)
    
    @as_new_thread
    def on_timer_delete(self, button):
        timer = self.get_current_timer()
        dialog = gtk.MessageDialog(self.window, type=gtk.MESSAGE_QUESTION,
                buttons=gtk.BUTTONS_YES_NO, message_format="Are you sure you "
                "want to delete timer " + str(timer) + "?")
        response = dialog.run()
        dialog.destroy()
        if response == gtk.RESPONSE_YES:
            self.controller.delete(timer)
    
    def on_timer_announce(self, button):
        self.controller.announce(self.get_current_timer())
    
    def on_timer_rename(self, button):
        pass
    
    def on_preferences(self, button):
        self.prefs_function()
    
    def switch_to_tab(self, timer_number):
        """
        Must be called on the GTK thread.
        """
        for p in self.tabs.get_n_pages():
            label = self.tabs.get_tab_label_text(self.tabs.get_nth_tab(p))
            if self.extract_number_from_name(label) == timer_number:
                self.tabs.set_current_page(p)
                return
        print "Switch to timer " + str(timer_number) + ": no such timer"
    
    def get_current_timer(self):
        """
        Must be called on the GTK thread.
        """
        page_number = self.tabs.get_current_page()
        if page_number == -1:
            return None
        label = self.tabs.get_tab_label_text(self.tabs.get_nth_tab(page_number))
        return self.extract_number_from_name(label)
    
    def timers_object_changed(self, timers_object):
        pass
    
    def startup_object_changed(self, startup_object):
        pass
    
    def extract_number_from_name(self, name):
        """
        Extracts the number of a timer from its tab name.
        """
        if not name.lower().startswith("timer "):
            raise ValueError("Invalid timer name: " + name)
        name = name[len("timer "):]
        if ":" in name:
            name = name[:name.find(":")]
        return int(name)










