package afn.parcon.errors;

public class EUnsatisfiable extends ExpectationType {
    
    @Override
    public String format() {
        return "EOF";
    }
    
    public boolean equals(Object other) {
        return other instanceof EUnsatisfiable;
    }
    
    public int hashCode() {
        return 0;
    }
    
}
