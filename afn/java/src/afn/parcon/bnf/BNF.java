package afn.parcon.bnf;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import afn.parcon.Forward;
import afn.parcon.InfixExpr;
import afn.parcon.Parser;
import afn.parcon.Then;
import static afn.parcon.Functions.*;

/**
 * A class that allows for creating parsers from a description written in BNF.
 * This effectively allows for parsing text that matches a language described by
 * a particular BNF grammar.
 * 
 * The only method you'll probably ever need is
 * {@link #createParserFromBNF(String)}.
 * 
 * @author jcp
 * 
 */
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
    
    public static final Parser bnfGrammar = createBNFGrammar();
    
    public static Map<String, Parser> internalBnfToParcon(
            List<Production> productions) {
        Map<String, Parser> result = new HashMap<String, Parser>();
        for (Production p : productions)
            result.put(p.name, new Forward());
        for (Production p : productions) {
            Parser[] alternativeParsers = new Parser[p.alternatives.size()];
            for (int ai = 0; ai < alternativeParsers.length; ai++) {
                Alternative alternative = p.alternatives.get(ai);
                Parser[] componentParsers = new Parser[alternative.components
                        .size()];
                for (int ci = 0; ci < componentParsers.length; ci++) {
                    Component c = alternative.components.get(ci);
                    if (c instanceof Text) {
                        componentParsers[ci] = significantLiteral(((Text) c).value);
                    } else if (c instanceof Reference) {
                        String name = ((Reference) c).name;
                        if (!result.containsKey(name))
                            throw new RuntimeException("Production <" + p.name
                                    + "> references production <" + name
                                    + "> but that production wasn't "
                                    + "declared in the grammar.");
                        componentParsers[ci] = result.get(name);
                    } else {
                        throw new RuntimeException("Invalid type: "
                                + c.getClass().getName());
                    }
                }
                alternativeParsers[ai] = Then.reduce(componentParsers);
            }
            ((Forward) result.get(p.name)).parser = first(alternativeParsers);
        }
        // Unwrap all of the forwards to speed things up and make the resulting
        // parsers' toString()s a bit clearer
        for (String name : result.keySet())
            result.put(name, ((Forward) result.get(name)).parser);
        return result;
    }
    
    /**
     * Converts a grammar, specified as a string in BNF format, to a set of
     * equivalent Parcon parsers. For every production in the BNF specified, an
     * equivalent Parcon parser is created and put into a {@link java.util.Map
     * Map}, with the production's name as the key. The resulting map is then
     * returned.<br/>
     * <br/>
     * 
     * For example, caling this method with:<br/>
     * <br/>
     * 
     * <pre>
     * &lt;test&gt; ::= "x" | "x" "y" &lt;test&gt;
     * </pre>
     * 
     * <br/>
     * <br/>
     * 
     * would return a map with one key, "test", whose value is the value of the
     * local variable <tt>test</tt> in the following code (and the code assumes
     * that "import static afn.parcon.Functions;" is present):<br/>
     * <br/>
     * 
     * <pre>
     * test = new Forward();
     * test.parser = first(significantLiteral(&quot;x&quot;),
     *         significantLiteral(&quot;x&quot;).then(significantLiteral(&quot;y&quot;)).then(test));
     * </pre>
     * 
     * @param bnf
     * @return
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Parser> createParserFromBNF(String bnf) {
        return internalBnfToParcon((List<Production>) bnfGrammar
                .parseString(bnf));
    }
}
