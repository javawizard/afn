package afn.parcon;

/**
 * A parser that simply delegates to another parser. This parser exists to allow
 * recursive grammars; one can declare an instance of Forward, create other
 * parsers that use it, and then later fill in what the Forward instance in
 * question should actually behave like.<br/>
 * <br/>
 * 
 * The underlying parser is specified by setting {@link Forward#parser} to the
 * parser that the Forward instance is to behave as.
 * 
 * @author jcp
 * 
 */
public class Forward extends Parser {
    /**
     * The parser that this Forward instance is to behave as
     */
    public Parser parser;
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        return parser.parse(text, position, end, space);
    }
    
}
