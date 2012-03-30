package afn.parcon;

/**
 * A parser that always succeeds, even if the parser it's constructed with
 * fails. When this happens, Optional consumes no input and returns the default
 * result value it was constructed with (which defaults to null).
 * 
 * @author jcp
 * 
 */
public class Optional extends Parser {
    /**
     * The underlying parser
     */
    public Parser parser;
    /**
     * The result that Optional will return if {@link #parser} fails
     */
    public Object defaultResult;
    
    /**
     * Creates a new Optional with the specified parser and a
     * {@link #defaultResult} of <tt>null</tt>.
     * 
     * @param parser
     */
    public Optional(Parser parser) {
        this(parser, null);
    }
    
    public Optional(Parser parser, Object defaultResult) {
        this.parser = parser;
        this.defaultResult = defaultResult;
    }
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        Result result = parser.parse(text, position, end, space);
        if (result.matched)
            return result;
        else
            return new Result(position, defaultResult, result.expected);
    }
    
}
