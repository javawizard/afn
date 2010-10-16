"""
This module is a main module. It provides a Protocol Buffers compiler for Java.
The resulting Java code is much simpler and easier to use than the Java code
that Google's protoc generates. Message classes are mutable, which makes things
a ton easier to use, but they also have setters that return the message, which
allows for chaining (new SomeMessageType().setX(...).setY(...).setZ(...)).

Right now, this only supports really basic protobuf functionality. It does not
check a good majority of stuff that protoc generally should, instead relying on
the Java compiler to do this. It does not require any external libraries to be
on the classpath; the generated code contains everything needed to perform
encoding. (It heavily relies on DataInputStream and DataOutputStream for
encoding and decoding functions.)

Multiple messages in a single file are supported (and they can reference each
other), but nested messages (messages defined within messages) are not. Enums
are not yet supported. Required, optional, and repeated fields are all fully
supported. Defaults are not; fields in the generated classes are stored as
their Java wrapper types, and if they're not present in the message, they'll be
null (unless they're marked as required, in which case the class will throw an
exception when trying to deserialize the message). Unknown fields are not
allowed; if an unknown field is encountered while deserializing, an exception
will be thrown. If someone wants to add support for unknown fields, I'd greatly
appreciate it.

Optional and required fields' classes are their wrapper type or the generated
class of the message for a compound field. Repeated fields are lists of the
corresponding types for that entry. These fields are initialized to contain
ArrayList instances, but any list instance is fine for the serializer; it's
perfectly fine for some user of the class to replace the list with a
LinkedList, for example, if they see fit.

The java_package and java_outer_classname options must be specified in the
protobuf file, and they must be specified before any other messages. All
options, for that matter, must be specified before messages are defined. This
is a limitation I might remove at some point.

The supported primitive types are string, int32, int64, double, and bool.

Only single-line comments (I.E. those starting with "//") are supported.

This module uses pyparsing to parse the protobuf file. You'll need pyparsing
installed in order for this module to work. 
"""

from pyparsing import Literal, Regex, ZeroOrMore, Forward, Optional, Group
from pyparsing import Keyword, Suppress, LineEnd
import sys
import os
import re

class Field(object):
    def __init__(self, tokens):
        tokens = tokens.asList()
        self.mode = tokens[0] # required, optional, repeated
        self.type = tokens[1] # int32, bool, string, etc
        self.name = tokens[2] # name of the field
        self.number = int(tokens[3]) # field index in the protocol
    
    def get_java_type(self):
        type = java_type_map.get(self.type, self.type)
        if self.mode == "repeated":
            type = "List<" + type + ">"
        return type
    
    def get_java_default(self):
        if self.mode == "repeated":
            return "new ArrayList<" + java_type_map.get(self.type, self.type) + ">()"
        return "null"
    
    def __repr__(self):
        return ("<Field " + self.name + " of type " + self.type + ", " + 
                self.mode + ", #" + str(self.number) + ">")
    __str__ = __repr__

class Message(object):
    def __init__(self, tokens):
        tokens = tokens.asList()
        self.name = tokens[0]
        self.fields = tokens[1]
    
    def __repr__(self):
        return "<Message " + self.name + " with fields " + repr(self.fields) + ">"
    __str__ = __repr__

comment = Literal("//") + Regex("[^\\n]*") + LineEnd()
field_type = Regex("[A-Za-z0-9_]+")
field_name = Regex("[A-Za-z0-9_]+")
field_number = Regex("[0-9]+")
message_field = ((Keyword("required") | Keyword("optional") | Keyword("repeated")) 
        + field_type + field_name + Suppress("=") + field_number + Suppress(";"))
message_field.setParseAction(Field)
message_name = Regex("[A-Za-z_]+")
message_spec = (Suppress(Keyword("message")) + message_name + Suppress("{") + 
         Group(ZeroOrMore(message_field)) + Suppress("}"))
message_spec.setParseAction(Message)
option_spec = (Suppress(Keyword("option")) + Regex("[a-z_]+") + Suppress("=") + 
        Regex('"[^"]*"') + Suppress(";"))
option_spec.setParseAction(lambda tokens: (tokens[0], tokens[1][1:-1]))
option_list = ZeroOrMore(option_spec)
option_list.setParseAction(lambda tokens: dict(tokens.asList()))
message_list = Group(ZeroOrMore(message_spec))
proto_file = (option_list + message_list).ignore(comment)

if len(sys.argv) < 3:
    print "usage: simpleproto some_file.proto outputfolder"
    print "The output will be placed in a folder within outputfolder"
    print "appropriate for the package specified in the file's java_package"
    print "option, in the file specified by java_outer_classname."

options, messages = proto_file.parseFile(sys.argv[1], parseAll=True).asList()
output_class = options["java_outer_classname"]
output_package = options["java_package"]
output_file_path = os.path.join(sys.argv[2], output_package.replace(
        ".", os.path.sep), output_class + ".java")
output_file = open(output_file_path, "w")

default_printf_args = {"output_class": output_class, "output_package":
        output_package}

# Maps protobuf types to the corresponding Java field types
java_type_map = {"string": "String", "int32": "Integer", "int64": "Long",
        "double": "Double", "bool": "Boolean"}
# Maps protobuf types to their expected wire type
wire_type_map = {"string": 2, "int32": 0, "int64": 0, "double": 1, "bool": 0}

def write(text, **kwargs):
    args = {}
    args.update(default_printf_args)
    args.update(kwargs)
    result = text % args
    result = re.sub("[\n]+", "\n", result)
    while result.startswith("\n"):
        result = result[1:]
    output_file.write(result)

write("""
package %(output_package)s;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.List;
import java.util.ArrayList;

public class %(output_class)s {
public static interface GeneratedMessage {public void serialize(DataOutputStream
out);public void deserialize(DataInputStream in);}

protected static byte[] reverseBytes(byte[] in){
    byte[] out = new byte[in.length];
    for(int i = 0; i < in.length; i++){
        out[(in.length-i)-1] = in[i];
    }
    return out;
}
protected static byte[] readVarint(DataInputStream in){
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    int value = in.read();
    while((value&0x80)>0){
        out.write(value);
        value = in.read();
    }
    out.write(value);
    return out.toByteArray()
}
protected static long readVarintAsLong(DataInputStream in){
    long result = 0;
    for(byte value : reverseBytes(readVarint(in))){
        result <<= 7;
        result = result | (value & 0x7F);
    }
    return result;
}
protected static int readVarintAsInt(DataInputStream in){
    return (int) readVarintAsLong(in);
}
protected static void writeVarintAsLong(DataOutputStream out, long value){
    for(int i = 0; i < 10; i++){
        out.write((value&0x7F)|0x80);
        out >>= 
    }
} 
""")

for message in messages:
    write("""
    public static class %(name)s implements GeneratedMessage { 
    """, name=message.name)
    for field in message.fields:
        write("""
        public %(type)s %(name)s = %(default)s;
        """, type=field.get_java_type(), name=field.name, default=field.get_java_default())
    write("public void serialize(DataOutputStream out){}public void deserialize(DataInputStream in){}")
    write("}")

write("}\n")

output_file.close()






























