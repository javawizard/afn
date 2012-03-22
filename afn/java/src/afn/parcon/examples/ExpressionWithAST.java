package afn.parcon.examples;

import afn.parcon.Forward;
import afn.parcon.InfixExpr;
import afn.parcon.Parser;
import static afn.parcon.Functions.*;
import static afn.parcon.InfixExpr.op;

public class ExpressionWithAST {
    
    public static abstract class AST {
        abstract double evaluate();
    }
    
    public static abstract class BinaryAST extends AST {
        AST left;
        AST right;
        
        public BinaryAST(AST left, AST right) {
            this.left = left;
            this.right = right;
        }
    }
    
    public static class NumberAST extends AST {
        public double value;
        
        public NumberAST(double value) {
            this.value = value;
        }
        
        double evaluate() {
            return this.value;
        }
    }
    
    public static class AddAST extends BinaryAST {
        public AddAST(AST left, AST right) {
            super(left, right);
        }
        
        double evaluate() {
            return left.evaluate() + right.evaluate();
        }
    }
    
    public static class SubtractAST extends BinaryAST {
        public SubtractAST(AST left, AST right) {
            super(left, right);
        }
        
        double evaluate() {
            return left.evaluate() - right.evaluate();
        }
    }
    
    public static class MultiplyAST extends BinaryAST {
        public MultiplyAST(AST left, AST right) {
            super(left, right);
        }
        
        double evaluate() {
            return left.evaluate() * right.evaluate();
        }
    }
    
    public static class DivideAST extends BinaryAST {
        public DivideAST(AST left, AST right) {
            super(left, right);
        }
        
        double evaluate() {
            return left.evaluate() / right.evaluate();
        }
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        Forward expr = new Forward();
        Parser term = first(
                number.translate(toDouble)
                        .translate(construct(NumberAST.class)), literal("(")
                        .then(expr).then(literal(")")));
        term = new InfixExpr(term, op("*", construct(MultiplyAST.class)), op(
                "/", construct(DivideAST.class)));
        term = new InfixExpr(term, op("+", construct(AddAST.class)), op("-",
                construct(SubtractAST.class)));
        expr.parser = term;
        System.out.println(((AST) expr.parseString("(5+3)*4")).evaluate());
    }
}
