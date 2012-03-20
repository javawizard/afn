package afn.parcon;

public class Literal extends Parser {
    private String string;
    private boolean significant;
    
    public Literal(String string) {
        this(string, false);
    }
    
    public Literal(String string, boolean significant) {
        this.string = string;
        this.significant = significant;
    }
    
    public Result parse(String text, int position, int end, Parser space) {
        position = space.consume(text, position, end);
        int expectedEnd = position + string.length();
        if (expectedEnd <= end
                && text.substring(position, expectedEnd).equals(string))
            return new Result(expectedEnd, significant ? string : null);
        else
            return new Result();
    }
}
