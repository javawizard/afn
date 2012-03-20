package afn.parcon;

public class CharIn extends Parser {
    private String chars;
    
    public CharIn(String chars) {
        this.chars = chars;
    }
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        position = space.consume(text, position, end);
        if (position < end && chars.indexOf(text.charAt(position)) != -1)
            return new Result(position + 1, text.charAt(position));
        else
            return new Result();
    }
}
