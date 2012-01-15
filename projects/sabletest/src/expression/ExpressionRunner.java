package expression;

import java.io.PushbackReader;
import java.io.StringReader;
import java.math.BigInteger;

import expression.lexer.Lexer;
import expression.node.AExpr;
import expression.node.AInAddp;
import expression.node.AInDivp;
import expression.node.AInMulp;
import expression.node.AInSubp;
import expression.node.AInUnmp;
import expression.node.ANextAddp;
import expression.node.ANextDivp;
import expression.node.ANextMulp;
import expression.node.ANextSubp;
import expression.node.ANextUnmp;
import expression.node.ANumberTerm;
import expression.node.AParensTerm;
import expression.node.Node;
import expression.node.Start;
import expression.parser.Parser;

public class ExpressionRunner
{
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        String text = "(5+3)*4";
        Lexer lexer = new Lexer(new PushbackReader(new StringReader(text)));
        Parser parser = new Parser(lexer);
        Start start = parser.parse();
        BigInteger answer = run(start.getPExpr());
        System.out.println("The answer is " + answer);
    }
    
    public static BigInteger run(Node node)
    {
        if (node instanceof AExpr)
            return run(((AExpr) node).getAddp());
        else if (node instanceof AInAddp)
            return run(((AInAddp) node).getFirst()).add(run(((AInAddp) node).getSecond()));
        else if (node instanceof AInSubp)
            return run(((AInSubp) node).getFirst()).subtract(
                    run(((AInSubp) node).getSecond()));
        else if (node instanceof AInMulp)
            return run(((AInMulp) node).getFirst()).multiply(
                    run(((AInMulp) node).getSecond()));
        else if (node instanceof AInDivp)
            return run(((AInDivp) node).getFirst()).divide(
                    run(((AInDivp) node).getSecond()));
        else if (node instanceof AInUnmp)
            return run(((AInUnmp) node).getSecond()).negate();
        else if (node instanceof ANextAddp)
            return run(((ANextAddp) node).getNext());
        else if (node instanceof ANextSubp)
            return run(((ANextSubp) node).getNext());
        else if (node instanceof ANextMulp)
            return run(((ANextMulp) node).getNext());
        else if (node instanceof ANextDivp)
            return run(((ANextDivp) node).getNext());
        else if (node instanceof ANextUnmp)
            return run(((ANextUnmp) node).getNext());
        else if (node instanceof ANumberTerm)
            return new BigInteger(((ANumberTerm) node).getNumber().getText());
        else if (node instanceof AParensTerm)
            return run(((AParensTerm) node).getExpr());
        else
            throw new IllegalArgumentException("Invalid node class: "
                + node.getClass().getName());
    }
}
