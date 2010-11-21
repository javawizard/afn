__doc__ = """\
Autoconfigure is a library that Python applications connecting to Autobus and
providing interfaces can use. It allows remote editing of configuration files
that a particular application uses. Typically, an application called "example"
will instruct Autoconfigure to register itself as example.configure for the
application's normal configuration file.

If the application is written to support this, changes made by remote
applications to configuration can be applied as soon as they are made.
"""