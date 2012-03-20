package afn.parcon.examples;

import static afn.parcon.Functions.*;

import java.util.Scanner;

import afn.parcon.Forward;
import afn.parcon.InfixExpr;
import afn.parcon.TwoFunction;
import static afn.parcon.InfixExpr.op;
import afn.parcon.Parser;

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
     * First we need to write a class providing our actual operations. If you
     * wanted to parser but not evaluate an expression, you might write this to
     * return (and accept as parameters) AST nodes of some sort. We, however,
     * want our parser to actually evaluate the expression, so we'll have this
     * class do the actual operations.
     * 
     * Note that we're writing these as one class to make things a bit shorter.
     * The operations could instead have been written as anonymous inner classes
     * at the locations where they're passed into the grammar.
     */
    private static class Op implements TwoFunction<Double, Double, Double> {
        private String op;
        
        public Op(String op) {
            this.op = op;
        }
        
        public Double call(Double a, Double b) {
            if (op.equals("+"))
                return a + b;
            else if (op.equals("-"))
                return a - b;
            else if (op.equals("*"))
                return a * b;
            else if (op.equals("/"))
                return a / b;
            throw new RuntimeException("Invalid op: " + op);
        }
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        /*
         * Now we get to the actual grammar.
         */
        Forward expr = new Forward();
        Parser term = first(number.translate(toDouble), literal("(").then(expr)
                .then(literal(")")));
        term = new InfixExpr(term, op("*", new Op("*")), op("/", new Op("/")));
        term = new InfixExpr(term, op("+", new Op("+")), op("-", new Op("-")));
        expr.parser = term;
        /*
         * That's it! Short, wasn't it? Now we'll use it to ask for an
         * expression from standard in and then print out the result.
         */
        System.out
                .println("Type an expression to evaluate. Operations are "
                        + "+, -, *, and /. Parentheses are allowed. Order of precedence "
                        + "is as expected: + and - lower than * and /.");
        String line = new Scanner(System.in).nextLine();
        System.out.println("Result: " + expr.parseString(line)); // The result
        // of expr.parseString will be a java.lang.Double
    }
}
