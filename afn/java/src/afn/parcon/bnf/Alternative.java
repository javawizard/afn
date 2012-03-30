package afn.parcon.bnf;

import java.util.ArrayList;
import java.util.List;

public class Alternative {
    public List<Component> components = new ArrayList<Component>();

    public Alternative() {
        super();
    }

    public Alternative(List<Component> components) {
        super();
        this.components = components;
    }
}
