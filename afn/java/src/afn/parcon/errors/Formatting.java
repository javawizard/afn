package afn.parcon.errors;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Formatting {
    public ExpectationSet filter(List<Expectation> expectations) {
        if (expectations.size() == 0)
            return new ExpectationSet(0);
        boolean onlyUnsatisfiables = true;
        for (Expectation e : expectations) {
            if (!(e.expectation instanceof EUnsatisfiable)) {
                onlyUnsatisfiables = false;
                break;
            }
        }
        if (onlyUnsatisfiables) {
            // This is a bunch of unsatisfiables, so we just pick out the last
            // one and return a singleton list based on it.
            Expectation maxExpectation = Collections.max(expectations);
            return new ExpectationSet(maxExpectation.position,
                    Collections.singletonList(maxExpectation.expectation));
        }
        // This contains things other than unsatisfiables, so we remove all of
        // the unsatisfiables, then get the expectations highest in the list.
        List<Expectation> list1 = new ArrayList<Expectation>();
        for (Expectation e : expectations)
            if (!(e.expectation instanceof EUnsatisfiable))
                list1.add(e);
        int position = Collections.max(list1).position;
        List<ExpectationType> list2 = new ArrayList<ExpectationType>();
        for (Expectation e : list1)
            if (e.position == position)
                list2.add(e.expectation);
        // Now we remove duplicates.
        List<ExpectationType> list3 = new ArrayList<ExpectationType>();
        for (ExpectationType e : list2)
            if (!list3.contains(e))
                list3.add(e);
        // And we're done!
        return new ExpectationSet(position, list3);
    }
    
    public String formatFailure(List<Expectation> expectations) {
        ExpectationSet set = filter(expectations);
        return formatExpectations(set.position, set.expectations);
    }
    
    public String formatExpectations(int position,
            List<ExpectationType> expectations) {
        StringBuilder b = new StringBuilder();
        for (ExpectationType e : expectations) {
            if (b.length() != 0)
                b.append(", ");
            b.append(e.format());
        }
        if (expectations.size() == 1)
            return "At position " + position + ": expected " + b;
        else
            return "At position " + position + ": expected one of " + b;
    }
}
