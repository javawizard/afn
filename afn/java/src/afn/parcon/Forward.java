package afn.parcon;

public class Forward extends Parser {
    
    public Parser parser;
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        return parser.parse(text, position, end, space);
    }
    
}
