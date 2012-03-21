package afn.parcon.errors;

public class EAnyCharIn extends ExpectationType {
    private String chars;
    
    public EAnyCharIn(String chars) {
        this.chars = chars;
    }
    
    public String format() {
        return "any char in \"" + chars + "\"";
    }
    
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((chars == null) ? 0 : chars.hashCode());
        return result;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (!(obj instanceof EAnyCharIn))
            return false;
        EAnyCharIn other = (EAnyCharIn) obj;
        if (chars == null) {
            if (other.chars != null)
                return false;
        } else if (!chars.equals(other.chars))
            return false;
        return true;
    }
}
