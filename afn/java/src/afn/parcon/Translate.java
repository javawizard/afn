package afn.parcon;

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
