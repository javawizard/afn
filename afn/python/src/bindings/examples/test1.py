
from bindings import bind, gtk2_bind

def view(v):
    c = gtk2_bind.DCheckButton()
    bind.s_bind_w(v, c.active)
    return c

values = bind.MemoryList()
values.extend([bind.MemoryValue(False), bind.MemoryValue(False), bind.MemoryValue(True)])

w = gtk2_bind.DWindow()
w.title.set("test1")

lt = bind.ListTranslator(view, None)
bind.s_bind_w(values, lt.a)
bind.s_bind_w(lt.b, w.children)

