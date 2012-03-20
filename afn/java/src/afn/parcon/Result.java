package afn.parcon;

import java.util.ArrayList;
import java.util.List;

public class Result {
    public boolean matched;
    public int end;
    public Object value;
    public List<Expectation> expected = new ArrayList<Expectation>();
    
    /**
     * Creates a failed result with a blank set of expectations, which should be
     * filled in later.
     */
    public Result() {
        this.matched = false;
    }
    
    /**
     * Creats a failed result with the specified expectations.
     * 
     * @param expected
     */
    public Result(List<Expectation> expected) {
        this.matched = false;
        this.expected.addAll(expected);
    }
    
    /**
     * Creates a successful result with the specified end position and value.
     * The expectation list should be filled in later.
     * 
     * @param end
     * @param value
     */
    public Result(int end, Object value) {
        this.matched = true;
        this.end = end;
        this.value = value;
    }
    
    /**
     * Creates a successful result with the specified end position, value, and
     * expectations.
     * 
     * @param end
     * @param value
     * @param expected
     */
    public Result(int end, Object value, List<Expectation> expected) {
        this.matched = true;
        this.end = end;
        this.value = value;
        this.expected.addAll(expected);
    }
    
    public String toString() {
        return "<afn.parcon.Result, " + (matched ? "matched" : "failed")
                + ", end=" + end + ", value=" + value + ", expected="
                + expected + ">";
    }
}
