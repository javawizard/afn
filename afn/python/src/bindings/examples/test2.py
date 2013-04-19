
from bindings import bind, gtk2_bind

def task_view(task):
    text_box = gtk2_bind.DEntry()
    completed_box = gtk2_bind.DCheckButton()
    bind.key_bind(text_box, "text", task, "text")
    bind.key_bind(completed_box, "active", task, "completed")
    panel = gtk2_bind.DHBox()
    panel.children.extend([text_box, completed_box])
    visible = bind.BinaryViewer(lambda s, c: (s or (not c)), False, False)
    bind.w_bind_s(visible.a, bind.value_for_weak_dict_key(show_all, "active"))
    # TODO: Binding to task_completed seems not to work, tasks don't disappear
    # when marked completed. Figure out why.
    bind.w_bind_s(visible.b, bind.value_for_weak_dict_key(completed_box, "active"))
    bind.bind(bind.value_for_dict_key(panel, "visible"), visible.output)
    return panel

tasks = bind.PyList()

w = gtk2_bind.DWindow()
w.title = "test2"

show_all = gtk2_bind.DCheckButton()
show_all.label = "Show all tasks"
show_all.active = False

add = gtk2_bind.DButton()
add.label = "Add"
def on_add(*args):
    t = bind.PyDict()
    t["text"] = ""
    t["completed"] = False
    tasks.append(t)
add.widget.connect("clicked", on_add)

task_vbox = gtk2_bind.DVBox()
task_viewport = gtk2_bind.DViewport()
task_viewport.children.append(task_vbox)
task_scroll = gtk2_bind.DScrolledWindow()
task_scroll.children.append(task_viewport)
main_vbox = gtk2_bind.DVBox()
main_vbox.children.extend([show_all, task_scroll, add])
w.children.append(main_vbox)

lt = bind.ListTranslator(task_view, None)
bind.s_bind_s(lt.a, tasks)
bind.s_bind_s(task_vbox.children, lt.b)

