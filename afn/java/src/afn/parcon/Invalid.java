package afn.parcon;

/**
 * A parser that always fails. This usually isn't useful, but can be useful in
 * some situations (for example, as a whitespace parser, which would effectively
 * disable whitespace parsing in a particular grammar).
 * 
 * @author jcp
 * 
 */
public class Invalid extends Parser {
    
    /**
     * A singleton Invalid instance that can be used to avoid the overhead of
     * constructing a new Invalid instance everywhere you're using one.
     */
    public static final Invalid invalid = new Invalid();
    
    public Result parse(String text, int position, int end, Parser space) {
        return new Result(Functions.expectation0(position));
    }
    
}
