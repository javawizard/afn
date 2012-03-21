package afn.parcon;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Functions {
    public static final OneFunction<Object, Double> toDouble = new OneFunction<Object, Double>() {
        public Double call(Object value) {
            if (value instanceof String)
                return Double.parseDouble((String) value);
            if (value instanceof Double)
                return (Double) value;
            throw new RuntimeException("Not a string or a double: " + value);
        }
    };
    
    public static final OneFunction<Object, List<Object>> flatten = new OneFunction<Object, List<Object>>() {
        public List<Object> call(Object value) {
            if (!(value instanceof List)) // If we just have one item, wrap it
                // in a list and return it
                return new ArrayList(Collections.singletonList(value));
            List<Object> result = new ArrayList<Object>();
            for (Object o : (List) value) {
                if (o instanceof List)
                    result.addAll(flatten.call(o));
                else
                    result.add(o);
            }
            return result;
        }
    };
    
    public static final OneFunction<List<Object>, String> joinStrings = new OneFunction<List<Object>, String>() {
        public String call(List<Object> value) {
            StringBuilder b = new StringBuilder();
            for (Object o : value)
                b.append(o);
            return b.toString();
        }
    };
    
    public static Literal literal(String text) {
        return new Literal(text);
    }
    
    public static Literal significantLiteral(String text) {
        return new Literal(text, true);
    }
    
    public static Literal significant(String text) {
        return significantLiteral(text);
    }
    
    public static First first(Parser... parsers) {
        return new First(parsers);
    }
    
    public static CharIn charIn(String chars) {
        return new CharIn(chars);
    }
    
    public static Optional optional(Parser parser) {
        return new Optional(parser);
    }
    
    public static Exact exact(Parser parser) {
        return new Exact(parser);
    }
    
    public static <P, R> ReflectiveOneFunction<P, R> method1(
            Object objectOrClass, String method) {
        return new ReflectiveOneFunction<P, R>(objectOrClass, method);
    }
    
    public static <A, B, R> ReflectiveTwoFunction<A, B, R> method2(
            Object objectOrClass, String method) {
        return new ReflectiveTwoFunction<A, B, R>(objectOrClass, method);
    }
    
    public static Parser promote(Object o) {
        if (o instanceof String)
            return literal((String) o);
        return (Parser) o;
    }
    
    public static final Parser whitespace = charIn(" \n\r\t");
    
    public static final Parser digit = charIn("0123456789");
    
    public static final Parser number = exact(
            optional(charIn("-+")).then(digit.onceOrMore()).then(
                    optional(significant(".").then(digit.onceOrMore()))))
            .translate(flatten).translate(joinStrings);
}
