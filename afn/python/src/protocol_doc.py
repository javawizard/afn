__doc__ = """\
This module is a simple protocol documentation generator. It takes an input
file and converts it to an HTML file. The input file is line-based.

The file is divided into sections, each representing a particular item that
can be sent across the wire. Each section is further divided into a list of
items, each of which has a type and a description. The type is either a
certain number of bytes, the name of another section, or a list of the names
of another section. The description of an item is just information on what
that item is. An item can also be marked as repeating, which means the item
can appear multiple times. The description will specify whether it can appear
zero times or if it has to appear at least once, and the description will
usually explain what governs how many times it can appear. A common practice
is to have a single byte specifying how many instances of some particular item
appear, and then have that repeating item appear immediately following.

So, a section is started with a single word on a single line. This specifies
the name of the section. Individual items within a section are specified as
item: value, where item is a number, another section name, or a space-separated
list of other section names, and value is the textual description of the item.

An item, whatever the type, can also be suffixed with "...", which means that
the item is a repeating item, as discussed above.

Item values are interpreted as HTML.

Any text in the file before the first section (which is a single word on a
single line) is placed at the top of the file before the sections. 
"""
