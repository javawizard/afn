package afn.parcon;

public class Discard extends Parser {
    
    private Parser parser;
    
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
