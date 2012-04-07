package afn.parcon;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import afn.parcon.errors.EUnsatisfiable;
import afn.parcon.errors.Expectation;
import afn.parcon.errors.ExpectationType;

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
    /**
     * A OneFunction that flattens out the specified value. If the value is not
     * an instance of {@link java.util.List List}, a new singleton List
     * containing only the value in question is returned. If the value is a
     * list, its values are recursively flattened and concatenated into a single
     * list.
     */
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
    
    public static <T> List<T> concatLists(List<T> first, List<T> second) {
        return Utils.concat(first, second);
    }
    
    public static final TwoFunction<List, List, List> concatLists = method2(
            Functions.class, "concatLists");
    
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
    
    public static CharNotIn charNotIn(String chars) {
        return new CharNotIn(chars);
    }
    
    public static Optional optional(Parser parser) {
        return new Optional(parser);
    }
    
    public static Optional optional(Parser parser, Object defaultValue) {
        return new Optional(parser, defaultValue);
    }
    
    public static Exact exact(Parser parser) {
        return new Exact(parser);
    }
    
    public static Not not(Parser parser) {
        return new Not(parser);
    }
    
    /**
     * Constructs a Then parser that parses multiple parsers sequentially.
     * sequence(a, b, c, d) behaves the same as new Then(new Then(new Then(a,
     * b), c), d).
     * 
     * @param parsers
     * @return
     */
    public static Then sequence(Parser... parsers) {
        if (parsers.length < 2)
            throw new RuntimeException(
                    "At least 2 parsers must be specified, but only "
                            + parsers.length + " were.");
        Then current = new Then(parsers[0], parsers[1]);
        for (int i = 2; i < parsers.length; i++)
            current = new Then(current, parsers[i]);
        return current;
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
    
    /**
     * Returns a list containing a single expectation, an expectation at the
     * specified position and whose type is a newly-created EUnsatisfiable.
     * 
     * @param position
     * @return
     */
    public static List<Expectation> expectation0(int position) {
        return Utils.list(new Expectation(position, new EUnsatisfiable()));
    }
    
    /**
     * Returns a list containing a single expectation, an expectation at the
     * specified position and with the specified type.
     * 
     * @param position
     * @param expectation
     * @return
     */
    public static List<Expectation> expectation1(int position,
            ExpectationType expectation) {
        return Utils.list(new Expectation(position, expectation));
    }
    
    public static ConstructorFunction construct(Class c) {
        return new ConstructorFunction(c);
    }
    
    public static ConstructorFunction construct(Class c, boolean expandThenLists) {
        return new ConstructorFunction(c, expandThenLists);
    }
    
    public static final Parser whitespace = new StandardSpace();
    
    public static final Parser digit = charIn("0123456789").expect(
            "any numerical digit");
    
    public static final Parser number = exact(
            optional(charIn("-+")).then(digit.onceOrMore()).then(
                    optional(significant(".").then(digit.onceOrMore()), "")))
            .translate(flatten).translate(joinStrings);
    
    public static final Parser exponentNumber =
        exact(
            sequence(
                number,
                optional(
                    sequence(
                        charIn("eE"), 
                        optional(charIn("+-"), "+"),
                        digit.onceOrMore()
                    ), 
                    ""
                )
            )
        ).translate(flatten).translate(joinStrings);
}
