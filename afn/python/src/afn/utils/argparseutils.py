
try:
    import argparse
except ImportError:
    from afn.backports import argparse


class AppendWithConst(argparse.Action):
    """
    An argparse action that functions like append, but it inserts the value of
    const as the first item in the list.
    """
    def __call__(self, parser, namespace, values, option_string=None):
        items = argparse._ensure_value(namespace, self.dest, [])[:]
        items.append([self.const] + values)
        setattr(namespace, self.dest, items)
