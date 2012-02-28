package jw.jzbot.fact.functions.list;

import java.util.List;

import jw.jzbot.fact.ArgumentList;
import jw.jzbot.fact.FactContext;
import jw.jzbot.fact.Function;
import jw.jzbot.fact.Sink;
import jw.jzbot.fact.utils.list.ListUtils;

public class LsgetFunction extends Function {
    
    @Override
    public void evaluate(Sink sink, ArgumentList arguments, FactContext context) {
        List<String> list = ListUtils.parseList(arguments.resolveString(0));
        sink.write(list.get(Integer.parseInt(arguments.resolveString(1))));
    }
    
    @Override
    public String getHelp(String topic) {
        return "Syntax: {lsget|<list>|<index>} -- Returns the item in the specified list at the specified index.";
    }
    
}
