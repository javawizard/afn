package afn.parcon;

public class OneOrMore extends Parser {
    
    private Parser parser;
    
    public OneOrMore(Parser parser) {
        this.parser = parser;
    }
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        // TODO Auto-generated method stub
        return null;
    }
    
}
