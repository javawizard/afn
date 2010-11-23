
# This is essentially a dumping ground for various things I'm trying out that
# have to do specifically with GTK. It loads from glade/landfill/landfill.xml
# and shows the UI therein present.

import gobject
import gtk

gobject.threads_init()

checkbox_ignore = False
togglebutton_ignore = False

class Signals(object):
    def on_checkbox_clicked2(self, checkbox):
        """
        print "Checkbox clicked"
        global checkbox_ignore
        if checkbox_ignore:
            checkbox_ignore = False
            return
        checkbox_ignore = True
        checkbox.set_active(not checkbox.get_active())
        """
    
    def toggle_box_happened(self, box):
        return False
    
    def on_togglebutton_clicked2(self, togglebutton):
        print "Togglebutton clicked"
        global togglebutton_ignore
        if togglebutton_ignore:
            togglebutton_ignore = False
            return
        togglebutton_ignore = True
        togglebutton.set_active(not togglebutton.get_active())
    
    def toggle_box(self, button):
        print "Toggle checkboxbox button clicked"
        global checkbox_ignore
        checkbox_ignore = True
        checkbox = builder.get_object("checkbox")
        checkbox.set_active(not checkbox.get_active())
    
    def toggle_button(self, button):
        print "Toggle togglebutton button clicked"
        global togglebutton_ignore
        togglebutton_ignore = True
        togglebutton = builder.get_object("togglebutton")
        togglebutton.set_active(not togglebutton.get_active())
    
    def gtk_main_quit(self, *args):
        gtk.main_quit()

def main():
    global builder
    builder = gtk.Builder()
    builder.add_from_file("glade/landfill/landfill.xml")
    builder.connect_signals(Signals())
    builder.get_object("window").show()
    gtk.main()
 