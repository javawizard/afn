package afn.parcon.bnf;

import afn.parcon.InfixExpr;
import afn.parcon.Parser;
import static afn.parcon.Functions.*;

public class BNF {
    /**
     * Creates a Parcon parser that can parse the syntax of BNF itself.
     * 
     * @return
     */
    public static Parser createBNFGrammar() {
        Parser equals = literal("::=");
        Parser ref = literal("<").then(
                charNotIn(">").onceOrMore().translate(joinStrings)).then(
                literal(">"));
        Parser productionStart = ref.then(equals);
        Parser string = exact(
                literal("\"").then(charNotIn("\"").zeroOrMore()).then(
                        literal("\""))).translate(joinStrings).construct(
                Text.class);
        Parser component = first(
                ref.onlyIf(not(ref.then(equals))).construct(Reference.class),
                string);
        Parser alternative = component.onceOrMore()
                .construct(Alternative.class);
        Parser production = productionStart.then(new InfixExpr(alternative
                .translate(flatten), InfixExpr.op("|", concatLists)));
        Parser productions = production.onceOrMore();
        return productions;
    }
    
    public static final Parser bnfParser = createBNFGrammar();
}
