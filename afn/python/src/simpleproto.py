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

The supported primitive types are string, fixed32, fixed64, double, and bool.

Only single-line comments (I.E. those starting with "//") are supported.

Field numbers must be less than 2047.

This module uses pyparsing to parse the protobuf file. You'll need pyparsing
installed in order for this module to work. 
"""

from pyparsing import Literal, Regex, ZeroOrMore, Forward, Optional, Group
from pyparsing import Keyword, Suppress, LineEnd
import sys
import os
import re

def studley(text):
    """
    Converts text containing underscores to studley caps. For example,
    studley("this_that_other") returns "thisThatOther". Text already in studley
    caps will not be modified.
    """
    return re.sub("_[a-zA-Z]", lambda match: match.group()[1:].upper(), text)
def camel(text):
    """
    Same as studley(...), but sets the first character to be upper-case.
    """
    text = studley(text)
    return text[0:1].upper() + text[1:]

# Maps protobuf types to the corresponding Java field types
java_type_map = {"string": "String", "fixed32": "Integer", "fixed64": "Long",
        "double": "Double", "bool": "Boolean"}
# Maps protobuf types to their expected wire type
wire_type_map = {"string": 2, "fixed32": 5, "fixed64": 1, "double": 1, "bool": 0}

class Field(object):
    def __init__(self, tokens):
        tokens = tokens.asList()
        self.mode = tokens[0] # required, optional, repeated
        self.type = tokens[1] # fixed32, bool, string, etc
        self.name = tokens[2] # name of the field
        self.number = int(tokens[3]) # field index in the protocol
        self.camel_name = camel(self.name)
        self.studley_name = studley(self.name)
        self.java_type = self.get_java_type()
        self.java_default = self.get_java_default()
        self.wire_type = wire_type_map.get(self.type, 2)
        self.component_type = java_type_map.get(self.type, self.type)
    
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
// Auto-generated by Alexander Boyd (a.k.a. javawizard, jcp)'s simpleproto.py

package %(output_package)s;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.io.IOException;

public class %(output_class)s {
public static interface GeneratedMessage<E extends GeneratedMessage<E>> 
{public byte[] serialize();public E deserialize(byte[] bytes);
public <T> T get(String name);public void set(String name, Object value);
public Class<?> getInstanceClassForField(String fieldName);
}
protected static int readVarint(DataInputStream in)throws IOException{
    int value = 0;
    int i;
    int shift = 0;
    while(((i = in.read())&0x80)>0){
        value |= (i&0x7F)<<(shift*7);
        shift += 1;
    }
    value |= i<<(shift*7);
    return value;
}
protected static void writeVarint(DataOutputStream out, int value)throws IOException{
    if(value<0)throw new RuntimeException("Negative varints are not allowed.");
    while(value>0){
        int i = value&0x7F;
        value >>= 7;
        if(value>0)
            i |= 0x80;
        out.write(i);
    }
}
protected static byte[] reverseBytes(byte[] in){
    byte[] out = new byte[in.length];
    for(int i = 0; i < in.length; i++){
        out[(in.length-i)-1] = in[i];
    }
    return out;
}
protected static byte[] readReversedBytes(DataInputStream in, int length)throws IOException{
    byte[] bytes = new byte[length];
    in.readFully(bytes);
    return reverseBytes(bytes);
}
protected static void writeReversedBytes(DataOutputStream out, byte[] bytes)throws IOException{
    bytes = reverseBytes(bytes);
    out.write(bytes);
}
protected static DataInputStream readReversedToStream(DataInputStream in, int length)throws IOException{
    return new DataInputStream(new ByteArrayInputStream(readReversedBytes(in, length)));
}
protected static int readInt(DataInputStream in)throws IOException{
    return readReversedToStream(in, 4).readInt();
}
protected static long readLong(DataInputStream in)throws IOException{
    return readReversedToStream(in, 8).readLong();
}
protected static double readDouble(DataInputStream in)throws IOException{
    return readReversedToStream(in, 8).readDouble();
}
protected static void writeInt(DataOutputStream out, int value)throws IOException{
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    new DataOutputStream(baos).writeInt(value);
    writeReversedBytes(out, baos.toByteArray());
}
protected static void writeLong(DataOutputStream out, long value)throws IOException{
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    new DataOutputStream(baos).writeLong(value);
    writeReversedBytes(out, baos.toByteArray());
}
protected static void writeDouble(DataOutputStream out, double value)throws IOException{
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    new DataOutputStream(baos).writeDouble(value);
    writeReversedBytes(out, baos.toByteArray());
}
protected static void writeFieldNumber(DataOutputStream out, int fieldNumber, int wireType)throws IOException{
    writeVarint(out, (fieldNumber << 3) | wireType);
}
protected static int readFieldNumber(DataInputStream in)throws IOException{
    return readVarint(in) >> 3;
}
protected static byte[] readVarBytes(DataInputStream in)throws IOException{
    byte[] bytes = new byte[readVarint(in)];
    in.readFully(bytes);
    return bytes;
}
protected static String readString(DataInputStream in)throws IOException{
    return new String(readVarBytes(in));
}
protected static void writeVarBytes(DataOutputStream out, byte[] bytes)throws IOException{
    writeVarint(out, bytes.length);
    out.write(bytes);
}
protected static void writeString(DataOutputStream out, String string)throws IOException{
    writeVarBytes(out, string.getBytes());
}
""")

for message in messages:
    write("""
    public static class %(name)s implements GeneratedMessage<%(name)s> { 
    """, name=message.name)
    for field in message.fields:
        write("""
        public %(type)s %(name)s = %(default)s;
        """, type=field.java_type, name=field.studley_name, default=field.java_default)
    for field in message.fields:
        write("""
        public %(message_class)s set%(camel_name)s(%(type)s %(name)s){
            this.%(name)s = %(name)s;
            return this;
        }
        """, message_class=message.name, camel_name=field.camel_name,
             type=field.java_type, name=field.studley_name)
    write("public void checkRequired(){")
    for field in message.fields:
        if field.mode != "required":
            continue;
        write("""if(this.%(name)s == null)
        throw new RuntimeException("Field '%(name)s' (protobuf message field "
        + "'%(protoname)s') is required as per the protobuf definition "
        + "but no value for this field seems to be supplied.");
        """, name=field.studley_name, protoname=field.name)
    write("""
    }void readFrom(DataInputStream in)throws IOException{
        while(in.available() > 0){
            int fieldNumber = readFieldNumber(in);
    """)
    for field in message.fields:
        write("if(fieldNumber == %(number)s){%(ctype)s newValue =\n",
                number=field.number, ctype=field.component_type)
        if field.type == "fixed32":
            write("readInt(in);")
        elif field.type == "fixed64":
            write("readLong(in);")
        elif field.type == "double":
            write("readDouble(in);")
        elif field.type == "string":
            write("readString(in);")
        elif field.type == "bool":
            write("in.readBoolean();")
        else:
            write("""
            new %(ctype)s();
            byte[] bytes = readVarBytes(in);
            newValue.deserialize(bytes);
            """, type=field.java_type, ctype=field.component_type)
        if field.mode == "repeated":
            write("this.%(name)s.add(newValue);", name=field.studley_name)
        else:
            write("this.%(name)s=newValue;", name=field.studley_name)
        write("}else\n")
    write("""
            {throw new RuntimeException("Invalid field received: " + fieldNumber);}
        }checkRequired();
    }
    """)
    write("void writeTo(DataOutputStream out)throws IOException{")
    for field in message.fields:
        write("if(this.%(name)s != null){", name=field.studley_name)
        if field.mode == "repeated":
            write("for(%(ctype)s currentValue:%(name)s){",
                    ctype=field.component_type, name=field.studley_name)
        else:
            write("%(ctype)s currentValue = %(name)s;",
                    ctype=field.component_type, name=field.studley_name)
        write("writeFieldNumber(out, %(number)s, %(wire)s);",
                number=field.number, wire=field.wire_type)
        if field.type == "fixed32":
            write("writeInt(out, currentValue);")
        elif field.type == "fixed64":
            write("writeLong(out, currentValue);")
        elif field.type == "double":
            write("writeDouble(out, currentValue);")
        elif field.type == "string":
            write("writeString(out, currentValue);")
        elif field.type == "bool":
            write("out.writeBoolean(currentValue);")
        else:
            write("""
            byte[] bytes = currentValue.serialize();
            writeVarBytes(out, bytes);
            """)
        if field.mode == "repeated":
            write("}")
        write("}")
    write("""
    }
    public %(message_type)s deserialize(byte[] bytes){
        try{readFrom(new DataInputStream(new ByteArrayInputStream(bytes)));}
        catch(IOException e){throw new RuntimeException(e);}return this;
    }
    public byte[] serialize(){
        try{
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            writeTo(new DataOutputStream(out));
            return out.toByteArray();
        }catch(IOException e){throw new RuntimeException(e);}
    }
    private static Map<String,Class<?>> _typeMap = new HashMap<String,Class<?>>();
    public static Set<String> getFieldNames(){
        return _typeMap.keySet();
    }
    public static Class<?> getClassForField(String name){
        return _typeMap.get(name);
    }
    public Class<?> getInstanceClassForField(String name){
        return getClassForField(name);
    }
    public static List<String> getFieldNamesForPrefix(String prefix){
        ArrayList<String> list = new ArrayList<String>();
        for(String name : getFieldNames()){
            if(name.startsWith(prefix))
                list.add(name);
        }
        return list;
    }
    public void set(String name, Object value){
        try{
            getClass().getField(name).set(this, value);
        }catch(Exception e){throw new RuntimeException(e);}
    }
    @SuppressWarnings("unchecked")
    public <T> T get(String name){
        try{
            return (T) getClass().getField(name).get(this);
        }catch(Exception e){throw new RuntimeException(e);}
    }
    static{
    """, message_type=message.name)
    for field in message.fields:
        write("_typeMap.put(\"%(name)s\", %(ctype)s.class);",
                name=field.studley_name, ctype=field.component_type)
    write("}}")

write("}\n")

output_file.close()






























