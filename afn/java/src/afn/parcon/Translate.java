package afn.parcon;

/**
 * A parser that behaves exactly as the parser it's constructed with, except
 * that if the parser in question succeeds, a particular {@link OneFunction} is
 * called, passing in the result, and the return value of the OneFunction is
 * used as the new result returned by Translate.
 * 
 * @author jcp
 * 
 */
public class Translate extends Parser {
    private Parser parser;
    private OneFunction function;
    
    public Translate(Parser parser, OneFunction function) {
        this.parser = parser;
        this.function = function;
    }
    
    @SuppressWarnings("unchecked")
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        Result result = parser.parse(text, position, end, space);
        if (result.matched)
            return new Result(result.end, function.call(result.value),
                    result.expected);
        else
            return new Result(result.expected);
    }
}
