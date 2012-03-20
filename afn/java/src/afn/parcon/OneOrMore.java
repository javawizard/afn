package afn.parcon;

import java.util.ArrayList;

public class OneOrMore extends Parser {
    
    private Parser parser;
    
    public OneOrMore(Parser parser) {
        this.parser = parser;
    }
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        ArrayList<Object> result = new ArrayList<Object>();
        Result parseResult = parser.parse(text, position, end, space);
        while (parseResult.matched) {
            result.add(parseResult.value);
            position = parseResult.end;
            parseResult = parser.parse(text, position, end, space);
        }
        if (result.size() < 1)
            return new Result(parseResult.expected);
        else
            return new Result(position, result, parseResult.expected);
    }
    
}
