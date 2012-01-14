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

description = """
A command-line tool for editing JSON data and plist files.
"""

epilog = """
Examples:


"""

parser = ArgumentParser()

actions = parser.add_argument_group("actions")
add_action = partial(actions.add_argument, dest="actions", action=AppendWithConst)
add_action("-u", "--up", const="up", nargs=0)
add_action("-k", "--key", const="key", nargs=1)
add_action("-K", "--key-and-up", const="key-and-up", nargs=1)
add_action("-i", "--index", const="index", nargs=1)
add_action("-I", "--index-and-up", const="index-and-up", nargs=1)
add_action("-r", "--read", const="read", nargs=0)
add_action("-R", "--read-file", const="read-file", nargs=1)
add_action("-w", "--write", const="write", nargs=0)
add_action("-W", "--write-file", const="write", nargs=1)
add_action("-c", "--write-current", const="write-current", nargs=0)
add_action("-C", "--write-current-to-file", const="write-current-to-file", nargs=1)
add_action("-f", "--file", const="file", nargs=1)
add_action("-o", "--object", const="object", nargs=0)
add_action("-l", "--list", const="list", nargs=0)
add_action("-s", "--string", const="string", nargs=1)
add_action("-n", "--number", const="number", nargs=1)
add_action("-b", "--bool", "--boolean", const="bool", nargs=1)
add_action("-T", "--true", const="true", nargs=0)
add_action("-F", "--false", const="false", nargs=0)
add_action("-N", "--null", const="null", nargs=0)
add_action("-v", "--value", const="value", nargs=1)
add_action("-m", "--make", const="make", nargs=0)
add_action("-j", "--json", const="json", nargs=0)
add_action("-p", "--plist", const="plist", nargs=0)
add_action("-P", "--python", const="python", nargs=0)
add_action("-t", "--text", const="text", nargs=0)
add_action("-x", "--delete", "--remove", const="delete", nargs=0)
add_action("-a", "--append-and-up", const="append-and-up", nargs=0)
add_action("-A", "--append", const="append", nargs=0)
add_action("-g", "--insert-and-up", const="insert-and-up", nargs=0)
add_action("-G", "--insert", const="insert", nargs=0)

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
    global formatter, path, working
    args = parser.parse_args()
    # print args.actions
    formatter = JSONFormatter()
    path = []
    working = None
    file_to_write = None
    formatter_to_write_with = None
    for action_info in args.actions:
        action = action_info[0]
        params = action_info[1:]
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
    if file_to_write is not None:
        formatter_to_write_with.write_file(file_to_write, working)
        






























