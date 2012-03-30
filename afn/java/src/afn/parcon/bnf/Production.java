package afn.parcon.bnf;

import java.util.ArrayList;
import java.util.List;

import afn.parcon.ThenList;

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
    
    /**
     * Same as new Production(values.get(0), values.get(1)). This is used in the
     * BNF parser in {@link BNF}.
     * 
     * @param values
     */
    public Production(ThenList values) {
        this((String) values.get(0), (List) values.get(1));
    }
}
