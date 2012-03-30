package afn.parcon.bnf;

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
                        literal("\""))).translate(joinStrings).translate(
                construct(Text.class));
    }
    
    public static final Parser bnfParser = createBNFGrammar();
}
