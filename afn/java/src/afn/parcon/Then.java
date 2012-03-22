package afn.parcon;

public class Then extends Parser {
    private Parser first;
    private Parser second;
    
    public Then(Parser first, Parser second) {
        this.first = first;
        this.second = second;
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
