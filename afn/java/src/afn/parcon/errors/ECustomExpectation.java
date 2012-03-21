package afn.parcon.errors;

public class ECustomExpectation extends ExpectationType {
    private String text;
    
    public ECustomExpectation(String text) {
        this.text = text;
    }
    
    public String format() {
        return text;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((text == null) ? 0 : text.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (!(obj instanceof ECustomExpectation))
            return false;
        ECustomExpectation other = (ECustomExpectation) obj;
        if (text == null) {
            if (other.text != null)
                return false;
        } else if (!text.equals(other.text))
            return false;
        return true;
    }
    
}
