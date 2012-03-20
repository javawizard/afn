package afn.parcon;

public class Literal extends Parser<Void> {
    private String string;
    
    public Literal(String string) {
        this.string = string;
    }
    
    public Result<Void> parse(String text, int position, int end, Parser space) {
        position = space.consume(text, position, end);
    }
    
}
