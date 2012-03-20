package afn.parcon;

public abstract class Parser<T> {
    public abstract Result<T> parse(String text, int position, int end,
            Parser space);
    
    public T parseString(String text) {
        return null;
    }
}
