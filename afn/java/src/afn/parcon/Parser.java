package afn.parcon;

public abstract class Parser<T> {
    public abstract Result<T> parse(String text, int position, int end,
            Parser space);
    
    public T parseString(String text) {
        return null;
    }
    
    public int consume(String text, int position, int end) {
        Result<T> result = parse(text, position, end, Invalid.invalid);
        while (result.matched) {
            position = result.end;
            result = parse(text, position, end, Invalid.invalid);
        }
        return position;
    }
}
