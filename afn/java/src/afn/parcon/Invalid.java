package afn.parcon;

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
