package afn.parcon;

public class Optional extends Parser {
    
    private Parser parser;
    
    public Optional(Parser parser) {
        this.parser = parser;
    }
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        // TODO Auto-generated method stub
        return null;
    }
    
}
