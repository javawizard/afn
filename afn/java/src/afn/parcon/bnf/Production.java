package afn.parcon.bnf;

import java.util.ArrayList;
import java.util.List;

public class Production {
    public Production(String name, List<Alternative> alternatives) {
        super();
        this.name = name;
        this.alternatives = alternatives;
    }
    public String name;
    public List<Alternative> alternatives = new ArrayList<Alternative>();
    public Production() {
        super();
    }
    public Production(String name) {
        super();
        this.name = name;
    }
}
