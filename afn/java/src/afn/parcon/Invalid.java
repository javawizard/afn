package afn.parcon;

public class Invalid extends Parser<Void> {
    
    public Result<Void> parse(String text, int position, int end, Parser space) {
        return new Result<Void>();
    }
    
}
