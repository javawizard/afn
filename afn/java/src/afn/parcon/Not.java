package afn.parcon;

import afn.parcon.errors.ECustomExpectation;

/**
 * A parser that succeeds when its underlying parser fails and vice versa. Its
 * result, when it succeeds (due to its underlying parser failing), is always
 * null, and it consumes no input.
 * 
 * @author jcp
 * 
 */
public class Not extends Parser {
    public Parser parser;
    
    public Not(Parser parser) {
        this.parser = parser;
    }
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        Result result = this.parser.parse(text, position, end, space);
        if (result.matched)
            return new Result(Functions.expectation1(position,
                    new ECustomExpectation("(TODO: Not)")));
        else
            return new Result(position, null, Functions.expectation0(position));
    }
    
}
