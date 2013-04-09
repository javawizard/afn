
from bindings import bind, gtk2_bind

def task_view(task):
    task_text = bind.value_for_weak_dict_key(task, "text")
    task_completed = bind.value_for_weak_dict_key(task, "completed")
    text_box = gtk2_bind.DEntry()
    completed_box = gtk2_bind.DCheckButton()
    bind.s_bind_w(task_text, text_box.text)
    bind.s_bind_w(task_completed, completed_box.active)
    panel = gtk2_bind.DHBox()
    panel.children.extend([text_box, completed_box])
    return panel

tasks = bind.MemoryList()

w = gtk2_bind.DWindow()
w.title.set("test2")

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
main_vbox.children.extend([task_vbox, add])
w.children.append(main_vbox)
lt = bind.ListTranslator(task_view, None)
bind.s_bind_s(tasks, lt.a)
bind.s_bind_s(lt.b, task_vbox.children)

