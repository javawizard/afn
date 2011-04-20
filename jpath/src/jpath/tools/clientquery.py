
from jpath.engine import Interpreter
from jpath.errors import EvaluationError
from jpath.errors import ParseException
from jpath.utils.conversion import jpath_to_python
import sys
try:
    import readline
except:
    print "Unable to import readline. History will be disabled."

options = {}

def main():
    print "JPath ClientQuery"
    print "Type :help for help, or :exit to exit."
    interpreter = Interpreter()
    interpreter.parse("1").evaluate(interpreter.new_state().context)
    while True:
        context = interpreter.new_state().context
        context = context.new_with_options(options)
        try:
            input = raw_input("CQ> ")
        except EOFError:
            print
            sys.exit()
        if input.startswith(":"):
            process_special(input)
            continue
        try:
            results = interpreter.parse(input).query(context)
            if len(results) == 0:
                print "-- No results."
            elif len(results) == 1:
                print "-- 1 result:"
            else:
                print "-- " + str(len(results)) + " results:"
            for result in results:
                print repr(jpath_to_python(result))
        except ParseException as e:
            print "Error while parsing: " + str(e)
        except EvaluationError as e:
            print "Error while evaluating: " + str(e)


def process_special(input):
    input = input[1:]
    if " " not in input:
        input += " " # Append an additional space so that split will return
        # at least two items
    command, input = input.split(" ", 1) 
    if command == "option":
        if " " not in input:
            print "Syntax: :option <name> <value>"
            return
        name, value = input.split(" ", 1)
        value = eval(value)
        options[name] = value
        print "Option " + name + " set to " + repr(value) + "."
    elif command in ["exit", "quit"]:
        sys.exit()
    else:
        print "Invalid command: " + command

if __name__ == "__main__":
    main()
