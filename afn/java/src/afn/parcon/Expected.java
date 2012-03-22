package afn.parcon;

import afn.parcon.errors.ECustomExpectation;

public class Expected extends Parser {
    
    public Parser parser;
    
    public String expected;
    
    public Expected(Parser parser, String expected) {
        this.parser = parser;
        this.expected = expected;
    }
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        position = space.consume(text, position, end);
        Result result = parser.parse(text, position, end, space);
        if (result.matched)
            return result;
        else
            return new Result(Functions.expectation1(position,
                    new ECustomExpectation(expected)));
    }
    
}
