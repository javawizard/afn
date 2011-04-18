
from jpath.engine import Context, parse, jpath_to_python, EvaluationError
from jpath.engine import ParseException
import sys
try:
    import readline
except:
    print "Unable to import readline. History will be disabled."

options = {}

def main():
    print "JPath ClientQuery"
    print "Type :help for help, or :exit to exit."
    parse("1")
    while True:
        context = Context()
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
            results = parse(input).query(context)
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
    elif command == "exit":
        sys.exit()
    else:
        print "Invalid command: " + command

if __name__ == "__main__":
    main()
