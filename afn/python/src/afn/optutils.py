"""
A command-line argument parsing library similar to argparse and optparse.

The main goal of optutils is to provide a library (and a slightly varied
command-line argument syntax) capable of parsing some arguments out of a
command line when the specifications for the rest of the arguments are not yet
known.

Note that this library won't, for now, support arguments that have multiple
parameters, as a result of the fact that, absent information about the layout
of said arguments, it's impossible to tell whether a given value belongs to a
particular flag or the general list of arguments to the program.

Note also that arguments with values must be given as --arg=value, not as
--arg value, as the latter is ambiguous as to whether the value is a value for
arg or a general argument to the program (with --arg a no-value flag).

Which, at present, would mean that arguments can be specified either as
--arg, --arg=value, or value.
"""

