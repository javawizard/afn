package afn.parcon;

import afn.parcon.errors.EAnyCharIn;

/**
 * A parser that matches a single character. The character must be a character
 * present in the string passed into CharIn's constructor. The result of this
 * parser is the character matched.
 * 
 * @author jcp
 * 
 */
public class CharIn extends Parser {
    /**
     * The set of characters that this parser will match
     */
    public String chars;
    
    /**
     * Creates a new CharIn that will only match the specified set of
     * characters.
     * 
     * @param chars
     */
    public CharIn(String chars) {
        this.chars = chars;
    }
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        position = space.consume(text, position, end);
        if (position < end && chars.indexOf(text.charAt(position)) != -1)
            return new Result(position + 1, text.charAt(position),
                    Functions.expectation0(position + 1));
        else
            return new Result(Functions.expectation1(position, new EAnyCharIn(
                    chars)));
    }
}
