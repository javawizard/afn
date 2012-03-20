package afn.parcon;

public class ZeroOrMore extends Parser {
    
    private Parser parser;
    
    public ZeroOrMore(Parser parser) {
        this.parser = parser;
    }
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        // TODO Auto-generated method stub
        return null;
    }
    
}
