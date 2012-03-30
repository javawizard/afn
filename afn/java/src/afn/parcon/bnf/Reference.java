package afn.parcon.bnf;

public class Reference implements Value {
    public Reference(String name) {
        super();
        this.name = name;
    }

    public String name;

    public Reference() {
        super();
    }
}
