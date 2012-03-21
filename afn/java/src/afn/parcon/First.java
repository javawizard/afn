package afn.parcon;

import java.util.ArrayList;
import java.util.List;

import afn.parcon.expectations.Expectation;
import static afn.parcon.Utils.concat;

public class First extends Parser {
    private Parser[] parsers;
    
    public First(Parser... parsers) {
        this.parsers = parsers;
    }
    
    @SuppressWarnings("unchecked")
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        List<Expectation> expected = new ArrayList<Expectation>();
        for (Parser parser : parsers) {
            Result result = parser.parse(text, position, end, space);
            if (result.matched)
                return new Result(result.end, result.value, concat(
                        result.expected, expected));
            else
                expected.addAll(result.expected);
        }
        return new Result(expected);
    }
}
