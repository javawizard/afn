package afn.parcon.bnf;

public class Text implements Component {
    public Text(String value) {
        super();
        this.value = value;
    }
    
    public String value;
    
    public Text() {
        super();
    }
}
