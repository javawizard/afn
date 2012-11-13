
from bindings import ValueTranslator, BindCell, ValidationFailed, value_bind

def i_s(v):
    return str(v)

def s_i(v):
    try:
        return int(v)
    except ValueError:
        raise ValidationFailed("Not a valid int representation: %r" % v)

def s_b(v):
    if v == "True":
        return True
    elif v == "False":
        return False
    else:
        raise ValidationFailed("Not a valid bool representation: %r" % v)

def b_s(v):
    if v is True:
        return "True"
    elif v is False:
        return "False"
    else:
        raise ValidationFailed("Not a bool value: %r" % v)

x = BindCell(1)
y = BindCell(None)
z = BindCell(None)
xy = ValueTranslator(i_s, s_i, 1)
yz = ValueTranslator(s_b, b_s, "1")
value_bind(x, xy.a)
value_bind(xy.b, y)
value_bind(y, yz.a)
value_bind(yz.b, z)



