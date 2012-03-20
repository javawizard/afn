package afn.parcon.examples;

import static afn.parcon.Parsers.*;
import afn.parcon.Parser;

public class Test1 {
    public static void main(String[] args) {
        Parser parser = significant("a").then(significant("b"))
                .then(first(significant("c"),
                        significant("d").then(significant("e"))));
        Parser whitespace = literal(" ");
        String text = "abcdefg";
        System.out.println(parser);
        System.out.println(parser.parse(text, 0, text.length(), whitespace));
    }
}
