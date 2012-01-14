"""
Neat little command-line JSON tool written by Alexander Boyd as part of the AFN
project. Copyright Alexander Boyd. Released under the terms of the GNU Lesser
General Public License.
"""

try:
    from argparse import ArgumentParser
except ImportError:
    from afn.backports.argparse import ArgumentParser
from afn.utils.argparseutils import AppendWithConst
from afn.utils.partial import partial
import json
import plistlib
import sys

description = """
A command-line tool for editing JSON data and plist files.
"""

epilog = """
Examples:


"""

parser = ArgumentParser()

parser.add_argument("-S", "--suppress-warnings", dest="warnings", action="store_false")

path_actions = parser.add_argument_group("path-related actions")
add_path_action = partial(path_actions.add_argument, dest="actions", action=AppendWithConst)
add_path_action("-u", "--up", const="up", nargs=0,
        help='Jumps one level up the path. Analogous to "cd ..".')
add_path_action("-k", "--key", const="key", nargs=1, metavar="<key>")
add_path_action("-K", "--key-and-up", const="key-and-up", nargs=1, metavar="<key>",
        help='Adds the specified key onto the end of the path. Analogous to "cd <key>".')
add_path_action("-i", "--index", const="index", nargs=1, metavar="<index>")
add_path_action("-I", "--index-and-up", const="index-and-up", nargs=1, metavar="<index>",
        help='Adds the specified index onto the end of the path. Analogous to "cd <index".')
add_path_action("-m", "--make", const="make", nargs=0,
        help="This action doesn't work yet. In the future, it will function analogously "
        'to "mkdir -p".')

io_actions = parser.add_argument_group("I/O actions")
add_io_action = partial(io_actions.add_argument, dest="actions", action=AppendWithConst)
add_io_action("-r", "--read", const="read", nargs=0,
        help="Reads data from stdin using the current mode, then sets the value at the "
        "current path to contain the data read.")
add_io_action("-R", "--read-file", const="read-file", nargs=1, metavar="<file>",
        help="Reads the specified file using the current mode, then sets the value at the "
        "current path to contain the data read.")
add_io_action("-w", "--write", const="write", nargs=0,
        help="Writes the root value to stdout using the current mode. Note that this writes "
        "the root value, not the value at the current path.")
add_io_action("-W", "--write-file", const="write", nargs=1, metavar="<file>",
        help="Writes the root value to the specified file using the current mode. Note that "
        "this writes the root value, not the value at the current path.")
add_io_action("-c", "--write-current", const="write-current", nargs=0,
        help="Writes the value at the current path to stdout using the current mode.")
add_io_action("-C", "--write-current-to-file", const="write-current-to-file", nargs=1, metavar="<file>",
        help="Writes the value at the current path to the specified file using the current mode.")
add_io_action("-f", "--file", const="file", nargs=1, metavar="<file>",
        help="Same as -R <file>, but adds -W <file> to the end of the command line. "
        "This allows for editing files in place without having to specify -R and -W separately. "
        "Note that even if the mode is changed after -f is specified, the file will still be "
        "written back using the mode that -f used to read the file.")

addition_actions = parser.add_argument_group("data actions")
add_addition_action = partial(addition_actions.add_argument, dest="actions", action=AppendWithConst)
add_addition_action("-o", "--object", const="object", nargs=0,
        help="Sets the value at the current path to be a newly-created object/dictionary.")
add_addition_action("-l", "--list", const="list", nargs=0,
        help="Sets the value at the current path to be a newly-created list/array.")
add_addition_action("-s", "--string", const="string", nargs=1, metavar="<text>",
        help="Sets the value at the current path to be a string containing the specified text.")
add_addition_action("-n", "--number", const="number", nargs=1, metavar="<number>",
        help="Sets the value at the current path to be the specified number.")
add_addition_action("-b", "--bool", "--boolean", const="bool", nargs=1, metavar="<bool>",
        help="Sets the value at the current path to be the specified boolean, which can be "
        "one of true, false, yes, no, on, or off. The value is case-insensitive.")
add_addition_action("-T", "--true", const="true", nargs=0,
        help="Sets the value at the current path to the boolean true.")
add_addition_action("-F", "--false", const="false", nargs=0,
        help="Sets the value at the current path to the boolean false.")
add_addition_action("-N", "--null", const="null", nargs=0,
        help="Sets the value at the current path to null.")
add_addition_action("-v", "--value", const="value", nargs=1, metavar="<value>",
        help="Reads <value> using the current mode and sets the value at the current "
        "path to it.")

mode_actions = parser.add_argument_group("modes")
add_mode_action = partial(mode_actions.add_argument, dest="actions", action=AppendWithConst)
add_mode_action("-j", "--json", const="json", nargs=0,
        help="Changes the current mode to JSON mode. Data will be read and written "
        "as JSON values.")
add_mode_action("-p", "--plist", const="plist", nargs=0,
        help="Changes the current mode to p-list mode. Data will be read and written "
        "using Apple's p-list XML file format.")
add_mode_action("-P", "--python", const="python", nargs=0,
        help="Changes the current mode to Python mode. Data will be read and written "
        "as Python objects. WARNING: This is not safe; reading data in Python mode "
        "allows evaluation of arbitrary Python expressions, so Python mode should "
        "only be used when you know that the data you'll be reading is safe.")
add_mode_action("-t", "--text", const="text", nargs=0,
        help="Changes the current mode to text mode. Data will be read as one "
        "continuous string, and only strings and numbers can be written with this "
        "mode. Strings will be written without quotes or escapes. This is intended "
        "to be used when you want to print out the value of "
        "a string without quotes, escapes, and other format-specific things.")

modify_actions = parser.add_argument_group("modification actions")
add_modify_action = partial(modify_actions.add_argument, dest="actions", action=AppendWithConst)
add_modify_action("-x", "--delete", "--remove", const="delete", nargs=0,
        help="Deletes the value at the current path, then jumps one level up the path. "
        'For example, if the current path points to the key "test" of a particular '
        'object/dictionary, -x will delete that key and whatever its value happens '
        ' to be, and then jump up one path level so that the path then points to the '
        'object/dictionary itself.')
add_modify_action("-a", "--append-and-up", const="append-and-up", nargs=0)
add_modify_action("-A", "--append", const="append", nargs=0,
        help="Appends a new item to the list/array that the current path points to, "
        'then adds the index of the newly-created item to the path. The item will have '
        'the value null; this can then be changed with a command such as -s or -n. For '
        'example, "-a -n 5" will append the number 5 to the list/array at the current path.')
add_modify_action("-g", "--insert-and-up", const="insert-and-up", nargs=1, metavar="<index>")
add_modify_action("-G", "--insert", const="insert", nargs=1, metavar="<index>",
        help="Inserts a new item into the list/array that the current path points to, "
        'then adds the specified index onto the path. Indexes start at 0. For example, running '
        '-g 0 -n 7 into the list [1,2,3] will change the list to [7,1,2,3], and running '
        '-g 1 -n 7 on the list [1,2,3] will change the list to [1,7,2,3]. This is different '
        'from -i, which will just modify an existing item; running -i 1 -n 7 on the list '
        '[1,2,3] will change the list to [1,7,3].')

class JSONFormatter(object):
    def read(self, text):
        return json.loads(text)
    def read_file(self, filename):
        with open(filename, "r") as f:
            return json.load(f) 
    def write(self, value):
        return json.dumps(value)
    def write_file(self, filename, value):
        with open(filename, "w") as f:
            json.dump(value, f)

class PlistFormatter(object):
    def read(self, text):
        return plistlib.readPlistFromString(text)
    def read_file(self, filename):
        return plistlib.readPlist(filename)
    def write(self, value):
        return plistlib.writePlistToString(value)
    def write_file(self, filename, value):
        plistlib.writePlist(value, filename)

class PythonFormatter(object):
    def read(self, text):
        return eval(text)
    def read_file(self, filename):
        with open(filename, "r") as f:
            return eval(f.read())
    def write(self, value):
        return repr(value)
    def write_file(self, filename, value):
        with open(filename, "w") as f:
            f.write(repr(value))

class TextFormatter(object):
    def read(self, text):
        return text
    def read_file(self, filename):
        with open(filename, "r") as f:
            return f.read()
    def write(self, value):
        return str(value)
    def write_file(self, filename, value):
        with open(filename, "w") as f:
            f.write(str(value))

def lookup(parent=False):
    global formatter, path, working
    value = working
    if parent:
        lookup_path = path[:-1]
    else:
        lookup_path = path
    for p in lookup_path:
        value = value[p]
    return value

def set_to(new_value):
    global formatter, path, working
    if len(path) == 0:
        working = new_value
    else:
        target = lookup(True)
        target[path[-1]] = new_value

def parse_bool(value):
    if value.tolower() in ["true", "yes", "on"]:
        return True
    if value.tolower() in ["false", "no", "off"]:
        return False
    raise ValueError("Value was supposed to be true, false, yes, no, on, or off, but was " + str(value))

def read_from_stdin():
    input = ""
    try:
        while True:
            input += raw_input()
    except EOFError: # Read all the input
        pass
    return input

def main():
    global formatter, path, working, warnings
    args = parser.parse_args()
    warnings = args.warnings
    # print args.actions
    formatter = JSONFormatter()
    path = []
    jump_path_next = False
    jump_path_now = False
    working = None
    file_to_write = None
    formatter_to_write_with = None
    for action_info in args.actions:
        if jump_path_next:
            jump_path_next = False
            jump_path_now = True
        elif jump_path_now:
            jump_path_now = False
            # print "Jumping up"
            path = path[:-1]
        # print "Action:", action_info, "Jump:", jump_path_next, jump_path_now, "Working:", working, "Path:", path
        action = action_info[0]
        params = action_info[1:]
        if action.endswith("-and-up"):
            action = action[:-len("-and-up")]
            jump_path_next = True
        # print action, params, path
        if action == "up":
            path = path[:-1]
        elif action == "key":
            path.append(params[0])
        elif action == "index":
            path.append(int(params[0]))
        elif action == "read":
            set_to(formatter.read(read_from_stdin()))
        elif action == "read-file":
            set_to(formatter.read_file(params[0]))
        elif action == "write":
            print formatter.write(working)
        elif action == "write-file":
            formatter.write_file(params[0], working)
        elif action == "write-current":
            print formatter.write(lookup())
        elif action == "write-current-to-file":
            formatter.write_file(params[0], lookup())
        elif action == "file":
            set_to(formatter.read_file(params[0]))
            file_to_write = params[0]
            formatter_to_write_with = formatter
        elif action == "object":
            set_to({})
        elif action == "list":
            set_to([])
        elif action == "string":
            set_to(params[0])
        elif action == "number":
            v = params[0]
            if int(v) == float(v):
                set_to(int(v))
            else:
                set_to(float(v))
        elif action == "bool":
            set_to(parse_bool(params[0]))
        elif action == "true":
            set_to(True)
        elif action == "false":
            set_to(False)
        elif action == "null":
            set_to(None)
        elif action == "value":
            set_to(formatter.read(params[0]))
        elif action == "json":
            formatter = JSONFormatter()
        elif action == "plist":
            formatter = PlistFormatter()
        elif action == "python":
            formatter = PythonFormatter()
        elif action == "text":
            formatter = TextFormatter()
        elif action == "delete":
            v = lookup(True)
            del v[path[-1]]
            path = path[:-1]
            if jump_path_now and warnings:
                print >>sys.stderr, ('WARNING: --delete automatically jumps up;'
                ' putting a "-and-up" action before a --delete will jump twice, '
                'which is probably not what you want. To suppress this warning, use -S.')
        elif action == "append":
            v = lookup()
            v.append(None)
            path.append(len(v) - 1)
        elif action == "insert":
            v = lookup()
            index = int(params[0])
            v.insert(index, None)
            path.append(index)
    if file_to_write is not None:
        formatter_to_write_with.write_file(file_to_write, working)
        






























