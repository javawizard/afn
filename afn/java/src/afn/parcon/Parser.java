package afn.parcon;

public abstract class Parser {
    public abstract Result parse(String text, int position, int end,
            Parser space);
    
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
        throw new RuntimeException("Parse failure");
    }
    
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
}
