package afn.parcon;

/**
 * A parser that acts exactly as the parser it's constructed with, except that
 * it discards the parser's output; Discard, when it succeeds, always provides a
 * result value of null.
 * 
 * @author jcp
 * 
 */
public class Discard extends Parser {
    /**
     * The underlying parser that this Discard instance behaves as
     */
    public Parser parser;
    
    /**
     * Creates a new Discard instance that will behave the same as the specified
     * parser, other than always returning null.
     * 
     * @param parser
     */
    public Discard(Parser parser) {
        this.parser = parser;
    }
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        Result result = parser.parse(text, position, end, space);
        if (result.matched)
            return new Result(result.end, null, result.expected);
        else
            return new Result(result.expected);
    }
    
}
