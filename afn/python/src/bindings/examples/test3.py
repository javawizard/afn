
from bindings import bind, gtk2_bind

w = gtk2_bind.DWindow()
w.title.set("test3")

vbox = gtk2_bind.DVBox()
w.children.append(vbox)

t1, t2 = gtk2_bind.DEntry(), gtk2_bind.DEntry()
b = gtk2_bind.DButton()
cb = gtk2_bind.DCheckButton()
cb.label.set("synchronized")
b.label.set("save")
c = bind.DelayController()
bind.s_bind_s(c.model, t1.text)
bind.s_bind_s(t2.text, c.view)
bind.s_bind_s(cb.active, c.synchronized)
vbox.children.extend([t1, t2, b, cb])
b.widget.connect("clicked", lambda *args: c.save())