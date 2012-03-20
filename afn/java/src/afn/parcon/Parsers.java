package afn.parcon;

public class Parsers {
    public static Literal literal(String text) {
        return new Literal(text);
    }
    
    public static Literal significantLiteral(String text) {
        return new Literal(text, true);
    }
    
    public static Literal significant(String text) {
        return significantLiteral(text);
    }
    
    public static First first(Parser... parsers) {
        return new First(parsers);
    }
}
