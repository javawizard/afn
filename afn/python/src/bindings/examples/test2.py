
from bindings import bind, gtk2_bind

def task_view(task):
    task_text = bind.value_for_weak_dict_key(task, "text")
    task_completed = bind.value_for_weak_dict_key(task, "completed")
    text_box = gtk2_bind.DEntry()
    completed_box = gtk2_bind.DCheckButton()
    bind.w_bind_s(text_box.text, task_text)
    bind.w_bind_s(completed_box.active, task_completed)
    panel = gtk2_bind.DHBox()
    panel.children.extend([text_box, completed_box])
    visible = bind.BinaryViewer(lambda s, c: (s or (not c)), False, False)
    bind.w_bind_s(visible.a, show_all.active)
    # TODO: Binding to task_completed seems not to work, tasks don't disappear
    # when marked completed. Figure out why.
    bind.w_bind_s(visible.b, completed_box.active)
    bind.w_bind_s(panel.visible, visible.output)
    return panel

tasks = bind.MemoryList()

w = gtk2_bind.DWindow()
w.title.set("test2")

show_all = gtk2_bind.DCheckButton()
show_all.label.set("Show all tasks")
show_all.active.set(False)

add = gtk2_bind.DButton()
add.label.set("Add")
def on_add(*args):
    t = bind.MemoryDict()
    t["text"] = ""
    t["completed"] = False
    tasks.append(t)
add.widget.connect("clicked", on_add)

task_vbox = gtk2_bind.DVBox()
main_vbox = gtk2_bind.DVBox()
main_vbox.children.extend([show_all, task_vbox, add])
w.children.append(main_vbox)

lt = bind.ListTranslator(task_view, None)
bind.s_bind_s(lt.a, tasks)
bind.s_bind_s(task_vbox.children, lt.b)

