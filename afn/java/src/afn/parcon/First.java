package afn.parcon;

import java.util.ArrayList;
import java.util.List;

import afn.parcon.errors.Expectation;
import static afn.parcon.Utils.concat;

/**
 * A parser that attempts to match all of its parsers in order. When it finds
 * one that matches, it returns that parser's result value. If none of its
 * parsers succeed, First fails.
 * 
 * @author jcp
 * 
 */
public class First extends Parser {
    /**
     * The set of parsers that First will try to match
     */
    public Parser[] parsers;
    
    /**
     * Creates a new First instance that will attempt to match the specified
     * parsers, in the specified orde.r
     * 
     * @param parsers
     */
    public First(Parser... parsers) {
        this.parsers = parsers;
    }
    
    @SuppressWarnings("unchecked")
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        List<Expectation> expected = new ArrayList<Expectation>();
        for (Parser parser : parsers) {
            Result result = parser.parse(text, position, end, space);
            if (result.matched)
                return new Result(result.end, result.value, concat(
                        result.expected, expected));
            else
                expected.addAll(result.expected);
        }
        return new Result(expected);
    }
}
