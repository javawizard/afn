package afn.parcon.bnf;

import java.util.ArrayList;
import java.util.List;

public class Alternative {
    public List<Component> values = new ArrayList<Component>();

    public Alternative() {
        super();
    }

    public Alternative(List<Component> values) {
        super();
        this.values = values;
    }
}
