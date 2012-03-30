package afn.parcon;

import afn.parcon.errors.EStringLiteral;

/**
 * A parser that parses a particular string literal. The parser's result is
 * either <tt>null</tt> if {@link #significant} is <tt>false</tt> or the string
 * literal itself if {@link #significant} is <tt>true</tt>.
 * 
 * @author jcp
 * 
 */
public class Literal extends Parser {
    /**
     * The string literal to parse
     */
    public String string;
    /**
     * True if the result of this Literal should be the parsed string, false if
     * the result should be <tt>null</tt>
     */
    public boolean significant;
    
    /**
     * Creates a new Literal using the specified string. {@link #significant}
     * will be set to <tt>false</tt>.
     * 
     * @param string
     */
    public Literal(String string) {
        this(string, false);
    }
    
    /**
     * Creates a new Literal using the specified string and the specified
     * significance setting.
     * 
     * @param string
     * @param significant
     */
    public Literal(String string, boolean significant) {
        this.string = string;
        this.significant = significant;
    }
    
    public Result parse(String text, int position, int end, Parser space) {
        position = space.consume(text, position, end);
        int expectedEnd = position + string.length();
        if (expectedEnd <= end
                && text.substring(position, expectedEnd).equals(string))
            return new Result(expectedEnd, significant ? string : null,
                    Functions.expectation0(expectedEnd));
        else
            return new Result(Functions.expectation1(position,
                    new EStringLiteral(string)));
    }
    
    public String toString() {
        return "<Literal, " + (significant ? "significant" : "not significant")
                + ": \"" + string + "\">";
    }
}
