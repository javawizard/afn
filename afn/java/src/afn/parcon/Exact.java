package afn.parcon;

/**
 * A parser that behaves the same as the parser it's constructed with, except
 * that it passes {@link Invalid#invalid} as the whitespace parser, or a custom
 * whitespace parser if the two-argument constructor is used. This is useful
 * when parsing, for example, string literals, as space is significant in string
 * literals and should be preserved. As an example:<br/>
 * <br/>
 * 
 * <tt>System.out.println(literal("\"").then(charNotIn("\"").zeroOrMore()).then(literal("\"")).translate(joinStrings).parseString("\"Hello, great big round world\""));</tt>
 * <br/>
 * <br/>
 * 
 * prints:<br/>
 * <br/>
 * 
 * <tt>Hello,greatbigroundworld</tt><br/>
 * <br/>
 * 
 * which is rarely what you want. If, instead, the parser is wrapped in an
 * instance of <tt>Exact</tt>:<br/>
 * <br/>
 * 
 * <tt>System.out.println(new Exact(literal("\"").then(charNotIn("\"").zeroOrMore()).then(literal("\""))).translate(joinStrings).parseString("\"Hello, great big round world\""));</tt>
 * <br/>
 * <br/>
 * 
 * then the following is printed:<br/>
 * <br/>
 * <tt>Hello, great big round world</tt><br/>
 * <br/>
 * 
 * which is probably more what you'd expect.
 * 
 * @author jcp
 * 
 */
public class Exact extends Parser {
    
    private Parser parser;
    
    private Parser whitespace;
    
    public Exact(Parser parser) {
        this(parser, Invalid.invalid);
    }
    
    public Exact(Parser parser, Parser whitespace) {
        this.parser = parser;
        this.whitespace = whitespace;
    }
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        position = space.consume(text, position, end);
        return parser.parse(text, position, end, this.whitespace);
    }
}
