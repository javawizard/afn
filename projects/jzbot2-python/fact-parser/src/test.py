from factparser.parser import parse
from factparser.parser import FactContext
from factparser.sinks import PrintSink

text = "I'd like to print %test%.";
context = FactContext()
context.localVars['test'] = "This is some text";
factoid = parse(text); 
factoid.resolve(PrintSink(), context)

