package afn.parcon;

import static afn.parcon.Functions.literal;

public class InfixExpr extends Parser {
    public static class Operator {
        public Parser parser;
        public TwoFunction function;
        
        public Operator(Parser parser, TwoFunction function) {
            this.parser = parser;
            this.function = function;
        }
        
        public Operator(String text, TwoFunction function) {
            this(literal(text), function);
        }
    }
    
    public static Operator op(Parser parser, TwoFunction function) {
        return new Operator(parser, function);
    }
    
    public static Operator op(String text, TwoFunction function) {
        return new Operator(text, function);
    }
    
    private Parser component;
    private Operator[] operators;
    
    public InfixExpr(Parser component, Operator... operators) {
        this.component = component;
        this.operators = operators;
    }
    
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        // TODO Auto-generated method stub
        return null;
    }
    
}
