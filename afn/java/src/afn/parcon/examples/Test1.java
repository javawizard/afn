package afn.parcon.examples;

import static afn.parcon.Parsers.*;
import afn.parcon.Parser;

public class Test1 {
    public static void main(String[] args) {
        Parser parser = significant("a").then(significant("b")).then(
                significant("c"));
        Parser whitespace = literal(" ");
        String text = "abcde";
        System.out.println(parser);
        System.out.println(parser.parse(text, 0, text.length(), whitespace));
    }
}
