package afn.parcon;

import java.util.ArrayList;
import java.util.List;

public class Result<T> {
    public boolean matched;
    public int end;
    public T value;
    public List<Expectation> expected = new ArrayList<Expectation>();
    
    public Result() {
        this.matched = false;
    }
    
    public Result(List<Expectation> expected) {
        this.matched = false;
        this.expected.addAll(expected);
    }
    
    public Result(int end, T value) {
        this.matched = true;
        this.value = value;
    }
    
    public Result(int end, T value, List<Expectation> expected) {
        this.matched = true;
        this.value = value;
        this.expected.addAll(expected);
    }
}
