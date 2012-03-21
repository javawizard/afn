package afn.parcon;

public class Optional extends Parser {
    
    private Parser parser;
    private Object defaultResult;
    
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
            return new Result(end, defaultResult, result.expected);
    }
    
}
