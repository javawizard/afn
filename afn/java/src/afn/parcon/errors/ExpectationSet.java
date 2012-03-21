package afn.parcon.errors;

import java.util.ArrayList;
import java.util.List;

public class ExpectationSet {
    public int position;
    public List<ExpectationType> expectations = new ArrayList<ExpectationType>();
    
    public ExpectationSet(int position, List<ExpectationType> expectations) {
        this.position = position;
        this.expectations = expectations;
    }
    
    public ExpectationSet(int position) {
        this.position = position;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((expectations == null) ? 0 : expectations.hashCode());
        result = prime * result + position;
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (!(obj instanceof ExpectationSet))
            return false;
        ExpectationSet other = (ExpectationSet) obj;
        if (expectations == null) {
            if (other.expectations != null)
                return false;
        } else if (!expectations.equals(other.expectations))
            return false;
        if (position != other.position)
            return false;
        return true;
    }
    
}
