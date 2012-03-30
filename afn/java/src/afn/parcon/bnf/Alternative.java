package afn.parcon.bnf;

import java.util.ArrayList;
import java.util.List;

public class Alternative {
    public List<Value> values = new ArrayList<Value>();

    public Alternative() {
        super();
    }

    public Alternative(List<Value> values) {
        super();
        this.values = values;
    }
}
