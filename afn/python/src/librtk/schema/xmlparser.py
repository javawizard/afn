

"""
A module for parsing schemas in XML format. The default widget schema uses
this.
"""

from xml.etree import ElementTree

"""
A module for parsing XML documents specifying widget schemas into the
corresponding schema objects used by librt itself.

WARNING: Parsing a foreign schema is unsafe. Parts of the schems are passed
into Python's eval function, so a schema document could execute arbitrary
code. Do not parse untrusted schemas.
"""

import structure
import categories

def parse_file(filename):
    with open(filename) as file:
        return parse(file.read())

def parse(xml):
    root = ElementTree.fromstring(xml)
    schema = {}
    for element in root:
        widget = parse_widget(element)
        schema[widget.name] = widget
    return schema

def parse_widget(element):
    name = element.attrib["name"]
    doc = get_doc(element)
    type = {"toplevel":categories.TOPLEVEL, "container":categories.CONTAINER,
            "widget":categories.WIDGET}[element.tag]
    widget_props = {}
    layout_props = {}
    state_props = {}
    calls = {}
    events = {}
    for e in element.findall("widget"):
        widget_props[e.attrib["name"]] = structure.WidgetPropertySchema(
                name=e.attrib["name"],
                doc=get_doc(e),
                writable=e.attrib.get("writable", "true") == "true",
                default=eval(e.attrib.get("default", None)))
    for e in element.findall("layout"):
        layout_props[e.attrib["name"]] = structure.LayoutPropertySchema(
                name=e.attrib["name"],
                doc=get_doc(e),
                writable=e.attrib.get("writable", "true") == "true",
                default=eval(e.attrib.get("default", None)))
    for e in element.findall("state"):
        state_props[e.attrib["name"]] = structure.StatePropertySchema(
                name=e.attrib["name"],
                doc=get_doc(e),
                default=eval(e.attrib.get("default", '""')))
    for e in element.findall("call"):
        calls[e.attrib["name"]] = structure.CallSchema(
                name=e.attrib["name"],
                doc=get_doc(e))
    for e in element.findall("event"):
        events[e.attrib["name"]] = structure.EventSchema(
                name=e.attrib["name"],
                doc=get_doc(e))
    return structure.WidgetSchema(name=name, doc=doc, type=type,
            widget_properties=widget_props, layout_properties=layout_props,
            state_properties=state_props, calls=calls, events=events)

def get_doc(element):
    if "doc" in element.attrib:
        return element.attrib["doc"]
    doc_child = element.find("doc")
    if doc_child is not None:
        return doc_child.text
    return ""




















