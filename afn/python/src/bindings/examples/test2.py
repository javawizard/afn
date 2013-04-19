
from bindings import bind, gtk2_bind as g

def task_view(task):
    text_box = g.DEntry()
    completed_box = g.make(g.DCheckButton(), child_props={"expand": False})
    bind.key_bind(text_box, "text", task, "text")
    bind.key_bind(completed_box, "active", task, "completed")
    panel = g.make(g.DHBox(), children=[text_box, completed_box])
    visible = bind.BinaryViewer(lambda s, c: (s or (not c)), False, False)
    bind.w_bind_s(visible.a, bind.value_for_weak_dict_key(show_all, "active"))
    # TODO: Binding to task_completed seems not to work, tasks don't disappear
    # when marked completed. Figure out why.
    bind.w_bind_s(visible.b, bind.value_for_weak_dict_key(completed_box, "active"))
    bind.bind(bind.value_for_dict_key(panel, "visible"), visible.output)
    return panel

tasks = bind.PyList()

w = g.DWindow()
w.title = "test2"

show_all = g.DCheckButton()
show_all.label = "Show all tasks"
show_all.active = False
show_all.child_props["expand"] = False

add = g.DButton()
add.child_props["expand"] = False
add.label = "Add"
def on_add(*args):
    t = bind.PyDict()
    t["text"] = ""
    t["completed"] = False
    tasks.append(t)
add.widget.connect("clicked", on_add)

task_vbox = g.make(g.DVBox(), child_props={"expand": False})
task_parent_vbox = g.make(g.DVBox(), children=[task_vbox, g.make(g.DLabel(), child_props={"expand": True})])
task_scroll = g.make(g.DScrolledWindow(), children=[g.make(g.DViewport(), children=[task_parent_vbox])])
w.children.append(g.make(g.DVBox(), children=[show_all, task_scroll, add]))

lt = bind.ListTranslator(task_view, None)
bind.s_bind_s(lt.a, tasks)
bind.s_bind_s(task_vbox.children, lt.b)

