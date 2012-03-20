package afn.parcon.examples;

import static afn.parcon.Functions.*;
import afn.parcon.Parser;

public class Test1 {
    public static void main(String[] args) {
        Parser parser = significant("a").then(significant("b")).then(
                first(significant("c"),
                        significant("d").discard().then(significant("e"))));
        Parser whitespace = literal(" ");
        String text = "a   b  defg";
        System.out.println(parser);
        System.out.println(parser.parse(text, 0, text.length(), whitespace));
    }
}
