package afn.parcon;

public class Parsers {
    public static Literal literal(String text) {
        return new Literal(text);
    }
    
    public static Literal significantLiteral(String text) {
        return new Literal(text, true);
    }
}
