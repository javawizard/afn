package afn.parcon;

import afn.parcon.errors.EAnyCharIn;
import afn.parcon.errors.EAnyCharNotIn;

/**
 * A parser that matches a single character as long as the character in question
 * is not in the set of characters passed to CharNotIn's constructor. This
 * parser otherwise behaves like {@link CharIn}.
 * 
 * @author jcp
 * 
 */
public class CharNotIn extends Parser {
    /**
     * The set of characters that this CharNotIn refuses to match
     */
    public String chars;
    
    /**
     * Creates a new CharNotIn that refuses to match any character in the
     * specified set of characters.
     * 
     * @param chars The set of characters not to match
     */
    public CharNotIn(String chars) {
        this.chars = chars;
    }
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        position = space.consume(text, position, end);
        if (position < end && chars.indexOf(text.charAt(position)) == -1)
            return new Result(position + 1, text.charAt(position),
                    Functions.expectation0(position + 1));
        else
            return new Result(Functions.expectation1(position,
                    new EAnyCharNotIn(chars)));
    }
}
