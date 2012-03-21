package afn.parcon;

import static afn.parcon.Functions.literal;

import java.util.ArrayList;
import java.util.List;

import afn.parcon.expectations.Expectation;

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
    
    @SuppressWarnings("unchecked")
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        // This is almost a line-for-line port of the Python Parcon library's
        // parcon.InfixExpr.parse method.
        Result componentResult = component.parse(text, position, end, space);
        if (!componentResult.matched)
            return new Result(componentResult.expected);
        // We've matched the component at least once. Now let's try matching
        // operator-component pairs.
        Object value = componentResult.value;
        position = componentResult.end;
        Result opResult = null;
        while (true) {
            boolean foundOp = false;
            TwoFunction opFunction = null;
            List<Expectation> opsExpected = new ArrayList<Expectation>();
            opsExpected.addAll(componentResult.expected);
            for (Operator op : operators) {
                opResult = op.parser.parse(text, position, end, space);
                if (opResult.matched) {
                    foundOp = true;
                    opFunction = op.function;
                    break;
                } else {
                    opsExpected.addAll(opResult.expected);
                }
            }
            if (!foundOp) // No more operators, so return the current value
                return new Result(position, value, opsExpected);
            // Operator matched. Now try to match the component.
            componentResult = component.parse(text, opResult.end, end, space);
            if (!componentResult.matched) {
                // Component didn't match, so we return the current value plus
                // the component's expectations. TODO: should we also return
                // opsExpected as part of the expectations?
                return new Result(position, value, Utils.concat(
                        componentResult.expected, opResult.expected));
            }
            // Component matched. Update the position and the value and continue
            // with attemping to match operators and components.
            position = componentResult.end;
            value = opFunction.call(value, componentResult.value);
        }
    }
    
}
