package afn.parcon;

public class And extends Parser {
    public Parser parser;
    public Parser checkParser;
    
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
