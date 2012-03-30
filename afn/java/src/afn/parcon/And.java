package afn.parcon;

/**
 * A parser that matches (and otherwise acts as) the parser it's constructed
 * with as long as the check parser it's constructed with also matches.
 * 
 * @author jcp
 * 
 */
public class And extends Parser {
    /**
     * The parser to use
     */
    public Parser parser;
    /**
     * The check parser to use
     */
    public Parser checkParser;
    
    /**
     * Creates a new And with the specified parser and the specified check
     * parser. This And will only match as long as both parsers match at the
     * same location; the result will be that of <tt>parser</tt>, not
     * <tt>checkParser</tt>.
     * 
     * @param parser
     * @param checkParser
     */
    public And(Parser parser, Parser checkParser) {
        this.parser = parser;
        this.checkParser = checkParser;
    }
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        // TODO: may want to parse space to make sure parsers are in sync
        Result checkResult = checkParser.parse(text, position, end, space);
        if (!checkResult.matched)
            return checkResult;
        return parser.parse(text, position, end, space);
    }
    
}
