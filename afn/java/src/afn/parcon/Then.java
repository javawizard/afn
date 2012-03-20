package afn.parcon;

public class Then<C> extends Parser<ThenList<C>> {
    private Parser first;
    private Parser second;
    
    public Then(Then<C> first, Then<C> second) {
        this.first = first;
        this.second = second;
    }
    
    public Then(Then<C> first, Parser<C> second) {
        this.first = first;
        this.second = second;
    }
    
    public Then(Parser<C> first, Then<C> second) {
        this.first = first;
        this.second = second;
    }
    
    public Then(Parser<C> first, Parser<C> second) {
        this.first = first;
        this.second = second;
    }
    
    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public Result<ThenList<C>> parse(String text, int position, int end,
            Parser space) {
        Result<Object> firstResult = first.parse(text, position, end, space);
        if (!firstResult.matched)
            return new Result<ThenList<C>>(firstResult.expected);
        Result<Object> secondResult = second.parse(text, firstResult.end, end,
                space);
        if (!secondResult.matched)
            return new Result<ThenList<C>>(secondResult.expected);
        boolean combineFirst = first instanceof Then;
        boolean combineSecond = second instanceof Then;
        Object a = firstResult.value;
        Object b = secondResult.value;
        ThenList<C> items = new ThenList<C>();
        if (combineFirst) {
            items.addAll((ThenList) a);
        } else if (a != null) {
            items.add((C) a);
        }
        if (combineSecond) {
            items.addAll((ThenList) b);
        } else if (b != null) {
            items.add((C) b);
        }
        Result<ThenList<C>> result = new Result<ThenList<C>>(secondResult.end,
                items);
        result.expected.addAll(firstResult.expected);
        result.expected.addAll(secondResult.expected);
        return result;
    }
}
