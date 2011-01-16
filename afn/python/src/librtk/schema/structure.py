
from collections import namedtuple as _namedtuple

WidgetSchema = _namedtuple("WidgetSchema", "name doc type widget_properties "
        " layout_properties state_properties calls events")
WidgetPropertySchema = _namedtuple("WidgetPropertySchema", "name doc writable default")
LayoutPropertySchema = _namedtuple("LayoutPropertySchema", "name doc writable default")
StatePropertySchema = _namedtuple("StatePropertySchema", "name doc default")
CallSchema = _namedtuple("CallSchema", "name doc")
EventSchema = _namedtuple("EventSchema", "name doc")
