package afn.parcon;

import java.util.regex.Pattern;

import afn.parcon.errors.ECustomExpectation;

/**
 * A parser that behaves similarly to <tt>new {@link CharIn}(" \r\n\t")</tt>,
 * but is heavily optimized, and in particular, overrides
 * {@link Parser#consume(String, int, int) consume} with an implementation much
 * faster than {@link Parser}'s default implementation. Its result is also
 * always <tt>null</tt>. This parser is therefore used as the default whitespace
 * parser when none is specified.
 * 
 * @author jcp
 * 
 */
public class StandardSpace extends Parser {
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        if (position < end && " \n\r\t".indexOf(text.charAt(position)) != -1)
            return new Result(position + 1, null,
                    Functions.expectation0(position + 1));
        else
            return new Result(Functions.expectation1(position,
                    new ECustomExpectation("space or tab or <CR> or <LF>")));
    }
    
    @Override
    public int consume(String text, int position, int end) {
        for (; " \n\r\t".indexOf(text.charAt(position)) != -1 && position < end; position++)
            ;
        return position;
    }
}
