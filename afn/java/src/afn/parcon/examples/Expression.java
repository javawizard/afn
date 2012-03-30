package afn.parcon.examples;

import static afn.parcon.Functions.*;

import java.util.Scanner;

import afn.parcon.Forward;
import afn.parcon.InfixExpr;
import afn.parcon.TwoFunction;
import static afn.parcon.InfixExpr.op;
import afn.parcon.Parser;
import afn.parcon.errors.Formatting;
import afn.parcon.errors.ParseFailureException;

/**
 * A simple Parcon example demonstrating how to write a mathematical expression
 * parser.<br/>
 * <br/>
 * 
 * This example demonstrates how the parser and the evaluator can be written as
 * the same thing using Parcon. The result of parsing an expression with the
 * parser provided in this file is the actual numerical result of the
 * expression.
 * 
 * @author jcp
 * 
 */
public class Expression {
    /*
     * First we need to create some methods that will perform the actual
     * operations. We choose to do the parsing and evaluating in the same steps
     * for convenience; we could, instead, have had each of these methods return
     * some sort of abstract syntax tree nodes.
     */
    public static Double add(Double x, Double y) {
        return x + y;
    }
    
    public static Double subtract(Double x, Double y) {
        return x - y;
    }
    
    public static Double multiply(Double x, Double y) {
        return x * y;
    }
    
    public static Double divide(Double x, Double y) {
        return x / y;
    }
    
    public static Parser createParser() {
        /*
         * Now we get to the actual grammar.
         */
        Forward expr = new Forward();
        Parser term = first(number.translate(toDouble), literal("(").then(expr)
                .then(literal(")")));
        term = new InfixExpr(term, op("*",
                method2(Expression.class, "multiply")), op("/",
                method2(Expression.class, "divide")));
        term = new InfixExpr(term, op("+", method2(Expression.class, "add")),
                op("-", method2(Expression.class, "subtract")));
        expr.parser = term;
        return expr;
        /*
         * That's it! Short, wasn't it?
         */
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        Parser expr = createParser();
        System.out
                .println("Type an expression to evaluate. Operations are "
                        + "+, -, *, and /. Parentheses are allowed. Order of precedence "
                        + "is as expected: + and - lower than * and /.");
        Scanner scanner = new Scanner(System.in);
        while (true) {
            String line = scanner.nextLine();
            try {
                System.out.println("Result: " + expr.parseString(line));
                // The result of expr.parseString will be a java.lang.Double
            } catch (ParseFailureException e) {
                int position = Formatting.filter(e.expectations).position;
                for (int i = 0; i < position; i++)
                    System.out.print(" ");
                System.out.println("^");
                System.out.println("The expression you typed has a problem: "
                        + e.getMessage());
            }
            System.out.println("Type another expression.");
        }
    }
}
