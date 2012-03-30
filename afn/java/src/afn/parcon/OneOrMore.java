package afn.parcon;

import java.util.ArrayList;

/**
 * A parser that repeatedly attempts to match the parser it's constructed with.
 * Once the parser in question fails, all of the results (even if there is just
 * one result) are collected into a list, and OneOrMore's result value is this
 * list.<br/>
 * <br/>
 * 
 * Unlike {@link ZeroOrMore}, OneOrMore fails if the underlying parser doesn't
 * match at least once.
 * 
 * @author jcp
 * 
 */
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
