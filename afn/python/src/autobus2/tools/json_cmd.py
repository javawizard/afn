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

parser = ArgumentParser()

actions = parser.add_argument_group("actions")
add_action = partial(actions.add_argument, dest="actions", action=AppendWithConst)
add_action("-u", "--up", const="up", nargs=0)
add_action("-k", "--key", const="key", nargs=1)
add_action("-i", "--index", const="index", nargs=1)
add_action("-r", "--read", const="read", nargs="?")
add_action("-w", "--write", const="write", nargs="?")
add_action("-f", "--file", const="file", nargs=1)
add_action("-c", "--write-current", const="write-current", nargs=0)
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

def main():
    global formatter, path, working
    args = parser.parse_args()
    formatter = JSONFormatter()
    path = []
    working = None
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
            input = ""
            try:
                while True:
                    input += raw_input()
            except EOFError: # Read all the input
                pass
            set_to(formatter.read(input))
        elif action == "write":
            print formatter.write(working)
        elif action == "write-current":
            print formatter.write(lookup())
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
        






























