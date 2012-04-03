package afn.parcon;

/**
 * A parser that takes two parser and parses the first followed by the second.
 * If either parser fails, Then fails. Otherwise, the two results (call them A
 * and B) are then combined to produce Then's result as follows:<br/>
 * <br/>
 * 
 * If A is null, set it to an empty {@link ThenList}. Otherwise, if A is not a
 * ThenList, set it to a ThenList containing one item, the original value of A.<br/>
 * If B is null, set it to an empty {@link ThenList}. Otherwise, if B is not a
 * ThenList, set it to a ThenList containing one item, the original value of B.<br/>
 * Concatenate the resulting lists into a new ThenList (we'll call this list C).<br/>
 * If C contains no items, return null. If C contains one item, return that
 * item. Otherwise, return C.<br/>
 * <br/>
 * 
 * It might be hard to understand what, exactly, the point of the above steps
 * are, and how multiple Thens used in a row act. The idea is that, for a
 * combination such as new Then(new Then(new Then(A, B), C), D), the results of
 * A, B, C, and D are all aggregated into a list. Those results that are null
 * are removed. Those results that are lists (specifically {@link ThenList}s)
 * themselves are expanded, i.e. they are replaced with the items which they
 * contain. Then, if the resulting list is empty, null is returned; if the
 * resulting list contains one item, it is returned; otherwise, the list itself
 * is returned.
 * 
 * @author jcp
 * 
 */
public class Then extends Parser {
    private Parser first;
    private Parser second;
    
    public Then(Parser first, Parser second) {
        this.first = first;
        this.second = second;
    }
    
    /**
     * Same as {@link Functions#sequence(Parser...) sequence)(parsers). This
     * method (Then.reduce) used to be the only implementation of the method,
     * but it has since been moved into {@link Functions}.
     * 
     * @param parser
     * @return
     */
    public static Then reduce(Parser... parsers) {
        return Functions.sequence(parsers);
    }
    
    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Result parse(String text, int position, int end, Parser space) {
        Result firstResult = first.parse(text, position, end, space);
        if (!firstResult.matched)
            return new Result(firstResult.expected);
        Result secondResult = second.parse(text, firstResult.end, end, space);
        if (!secondResult.matched)
            return new Result(Utils.concat(firstResult.expected,
                    secondResult.expected));
        Object a = firstResult.value;
        Object b = secondResult.value;
        boolean combineFirst = a instanceof ThenList;
        boolean combineSecond = b instanceof ThenList;
        ThenList items = new ThenList();
        if (combineFirst) {
            items.addAll((ThenList) a);
        } else if (a != null) {
            items.add(a);
        }
        if (combineSecond) {
            items.addAll((ThenList) b);
        } else if (b != null) {
            items.add(b);
        }
        // For now, we mirror the Python version of Parcon's behavior and avoid
        // wrapping single items. I may change this later.
        Object resultValue = items.size() == 1 ? items.get(0) : items;
        Result result = new Result(secondResult.end, resultValue);
        result.expected.addAll(firstResult.expected);
        result.expected.addAll(secondResult.expected);
        return result;
    }
    
    public String toString() {
        return "<Then: first=" + first + ", second=" + second + ">";
    }
}
