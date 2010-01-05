from factparser.parser import parse
from factparser.parser import FactContext
from factparser.sinks import PrintSink

text = "You chose {random|first|second|third}.";
context = FactContext()
factoid = parse(text); 
factoid.resolve(PrintSink(), context)

