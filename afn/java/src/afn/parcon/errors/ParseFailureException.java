package afn.parcon.errors;

import java.util.List;

public class ParseFailureException extends ParseException {
    public List<Expectation> expectations;
    
    public ParseFailureException(List<Expectation> expectations) {
        this.expectations = expectations;
    }
    
    public String getMessage() {
        return 
    }
}
