package afn.parcon;

public class Exact extends Parser {
    
    private Parser parser;
    
    private Parser whitespace;
    
    public Exact(Parser parser) {
        this(parser, Invalid.invalid);
    }
    
    public Exact(Parser parser, Parser whitespace) {
        this.parser = parser;
        this.whitespace = whitespace;
    }
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        position = space.consume(text, position, end);
        return parser.parse(text, position, end, this.whitespace);
    }
}
