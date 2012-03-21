package afn.parcon.errors;

public class EAnyChar extends ExpectationType {
    public String format() {
        return "any char";
    }
    
    public boolean equals(Object other) {
        return other instanceof EAnyChar;
    }
    
    public int hashCode() {
        return 0;
    }
}
