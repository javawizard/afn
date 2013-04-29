
from bindings import bind, gtk2_bind as g, bind_utils
import sys, json
from afn.fileutils import File
import gtk, pango

class TaskItemView(g.DHBox):
    def __init__(self, task):
        g.DHBox.__init__(self)
        self.task = task
        label = g.make(g.DLabel(), props={"xalign": 0, "ellipsize": pango.ELLIPSIZE_END})
        completed_box = g.make(g.DCheckButton(), child_props={"expand": False})
        edit_button = g.make(g.DButton(), props={"label": "Edit"}, child_props={"expand": False})
        edit_button.widget.connect("clicked", self.show_editor)
        add_button = g.make(g.DButton(), props={"label": "Add"}, child_props={"expand": False})
        add_button.widget.connect("clicked", lambda *args: create_task(task["children"]))
        bind.key_bind(label, "label", task, "text")
        # Show the full text in the tooltip
        bind.key_bind(label.props, "tooltip-text", task, "text")
        bind.key_bind(completed_box, "active", task, "completed")
        self.children += [label, completed_box, edit_button, add_button]
        visible = bind.BinaryViewer(lambda s, c: (s or (not c)), False, False)
        bind.v_key_bind(visible.a, show_all, "active")
        # TODO: Binding to task_completed seems not to work, tasks don't disappear
        # when marked completed. Figure out why.
        bind.v_key_bind(visible.b, completed_box, "active")
        bind.key_bind_v(self, "should_be_displayed", visible.output)
    
    def show_editor(self, *args):
        text_box = g.DEntry()
        bind.key_bind(text_box, "delayed_text", self.task, "text")
        w = g.make(g.DWindow(), props={"title": "Task Editor"}, children=[text_box])
        w.widget.resize(250, 1)
        text_box.widget.grab_focus()

class TaskView(g.DVBox):
    def __init__(self, task):
        g.DVBox.__init__(self)
        self.task = task
        if "children" not in task:
            task["children"] = bind.PyList()
        item_view = TaskItemView(task)
        self.children.append(item_view)
        bind.key_bind(self, "visible", item_view, "should_be_displayed")
        alignment = g.make(g.DAlignment(), props={"left-padding": 20, "xscale": 1}, children=[TaskListView(task["children"])])
        self.children.append(alignment)

class TaskListView(g.DVBox):
    def __init__(self, task_list):
        g.DVBox.__init__(self)
        self.task_list = task_list
        template = bind.ListTranslator(TaskView, None)
        bind.bind(template.a, task_list)
        bind.bind(self.children, template.b)

def create_task(task_list):
    t = bind.PyDict()
    t["text"] = ""
    t["completed"] = False
    t["children"] = bind.PyList()
    task_list.append(t)

task_file = File(sys.argv[1])
if task_file.exists:
    tasks = bind_utils.json_to_bindable(json.load(task_file.open("r")))
else:
    tasks = bind.PyList()

w = g.make(g.DWindow(), props={"title": task_file.name + " - Task List"})
w.widget.resize(440, 520)

show_all = g.make(g.DCheckButton(), props={"label": "Show all tasks"}, child_props={"expand": True})
add = g.make(g.DButton(), props={"label": "Add"}, child_props={"expand": False})
save = g.make(g.DButton(), props={"label": "Save"}, child_props={"expand": False})
def on_add(*args):
    create_task(tasks)
def on_save(*args):
    json.dump(bind_utils.bindable_to_json(tasks), task_file.open("w"), sort_keys=True, indent=2)
add.widget.connect("clicked", on_add)
save.widget.connect("clicked", on_save)

task_list_view = g.make(TaskListView(tasks), child_props={"expand": False})
task_parent_vbox = g.make(g.DVBox(), children=[task_list_view, g.make(g.DLabel(), child_props={"expand": True})])
task_scroll = g.make(g.DScrolledWindow(), children=[g.make(g.DViewport(), children=[task_parent_vbox])])
task_scroll.props.update({"hscrollbar-policy": gtk.POLICY_AUTOMATIC, "vscrollbar-policy": gtk.POLICY_AUTOMATIC})
control_panel = g.make(g.DHBox(), child_props={"expand": False}, children=[show_all, add, save])
w.children.append(g.make(g.DVBox(), children=[control_panel, task_scroll]))

def on_close(*args):
    on_save()
    gtk.main_quit()
w.widget.connect("delete-event", on_close)

gtk.main()


