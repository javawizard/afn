package afn.parcon;

import afn.parcon.errors.ParseFailureException;

/**
 * This class represents a Parcon parser. All of the various types of parsers
 * subclass from this class.<br/>
 * <br/>
 * 
 * A particular piece of text can be parsed by calling
 * {@link #parseString(String)} or {@link #parseString(String, boolean, Parser)}
 * . The former parses the entire string and uses a whitespace parser that
 * matches carriage returns, newlines, tabs, and spaces; the latter allows these
 * to be specified separately.<br/>
 * <br/>
 * 
 * Usually, you won't need to create a Parser subclass, as the set of subclasses
 * provided with Parcon is extensive; instead, you'll typically just create
 * instances of these classes. If you do want to write your own custom parsing
 * logic, however, you need only subclass Parser and override
 * {@link #parse(String, int, int, Parser)}. A quick look at some of Parcon's
 * built-in Parser subclasses (such as Literal or CharIn) will help you figure
 * out how to go about this.
 * 
 * @author jcp
 * 
 */
public abstract class Parser {
    /**
     * Parses the specified text, starting at the specified position and not
     * going past the specified end position, using the specified whitespace
     * parser, and returns the result.<br/>
     * <br/>
     * 
     * This is the sole method subclasses must override in order to produce a
     * fully-functional parser. Subclasses are responsible for applying the
     * specified whitespace parser as much as possible before doing actual work;
     * this is so that some parsers, such as {@link Exact}, can deliberately
     * avoid parsing whitespace if they wish. A typical way to do this is to add
     * the following line at the top of your implementation of this method:<br/>
     * <br/>
     * 
     * <tt>position = space.consume(text, position, end);</tt>
     * 
     * <br/>
     * <br/>
     * 
     * @param text
     * @param position
     * @param end
     * @param space
     * @return
     */
    public abstract Result parse(String text, int position, int end,
            Parser space);
    
    /**
     * Parses the specified string from start to end and returns the result.
     * This is the same as
     * <tt>parseString(text, true, Functions.whitespace)</tt>.
     * 
     * @param text
     * @return
     * @throws ParseFailureException
     *             If this parser cannot parse the specified input completely,
     *             from start to finish
     * @throws ParseException
     *             If another parse error occurs
     */
    public Object parseString(String text) {
        return parseString(text, true, Functions.whitespace);
    }
    
    public Object parseString(String text, boolean all, Parser whitespace) {
        Result result = parse(text, 0, text.length(), whitespace);
        if (result.matched) {
            if (!all) { // We got a result back and we're not trying to match
                // the entire string, so regardless of how much we parsed, we
                // return.
                return result.value;
            }
            // Result matched and we're trying to match everything, so we ask
            // the whitespace parser to consume everything at the end, then
            // check to see if the end position is equal to the string length,
            // and if it is, we return the value.
            if (whitespace.consume(text, result.end, text.length()) == text
                    .length())
                return result.value;
        }
        throw new ParseFailureException(result.expected);
    }
    
    /**
     * Matches this parser as many times as possible on the specified text at
     * the specified position (but not going past <tt>end</tt>), discarding the
     * result each time and returning the position at which this parser no
     * longer matched.<br/>
     * <br/>
     * 
     * The default implementation of this method calls
     * {@link #parse(String, int, int, Parser)} repeatedly until it fails,
     * passing the resulting end index of one call as the position of the next.
     * The final position, the one at which parsing failed, is then returned.<br/>
     * <br/>
     * 
     * This method's main use is for whitespace parsing: most parsers call the
     * whitespace parser's consume method to filter out whitespace. Parsers
     * should therefore override this if they can provide a more efficient
     * implementation.
     * 
     * @param text
     * @param position
     * @param end
     * @return
     */
    public int consume(String text, int position, int end) {
        Result result = parse(text, position, end, Invalid.invalid);
        while (result.matched) {
            position = result.end;
            result = parse(text, position, end, Invalid.invalid);
        }
        return position;
    }
    
    public Then then(Parser next) {
        return new Then(this, next);
    }
    
    public Discard discard() {
        return new Discard(this);
    }
    
    public Translate translate(OneFunction function) {
        return new Translate(this, function);
    }
    
    public OneOrMore onceOrMore() {
        return new OneOrMore(this);
    }
    
    public ZeroOrMore zeroOrMore() {
        return new ZeroOrMore(this);
    }
    
    public Expected expect(String expected) {
        return new Expected(this, expected);
    }
    
    public And onlyIf(Parser parser) {
        return new And(this, parser);
    }
    
    /**
     * Same as <tt>this.translate(Functions.construct(c))</tt>, i.e. returns a
     * new parser that constructs an instance of the specified class by invoking
     * the class's one-argument constructor passing in this parser's result.
     * 
     * @param c
     * @return
     */
    public Translate construct(Class c) {
        return this.translate(Functions.construct(c));
    }
}
