package afn.parcon.errors;

public abstract class ExpectationType {
    public abstract String format();
    
    public String toString() {
        return "<" + getClass().getName() + ": " + format() + ">";
    }
}
