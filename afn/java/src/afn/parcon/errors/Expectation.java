package afn.parcon.errors;

public class Expectation implements Comparable<Expectation> {
    public int position;
    public ExpectationType expectation;
    
    public Expectation(int position, ExpectationType expectation) {
        super();
        this.position = position;
        this.expectation = expectation;
    }
    
    public int compareTo(Expectation other) {
        return new Integer(position).compareTo(other.position);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((expectation == null) ? 0 : expectation.hashCode());
        result = prime * result + position;
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (!(obj instanceof Expectation))
            return false;
        Expectation other = (Expectation) obj;
        if (expectation == null) {
            if (other.expectation != null)
                return false;
        } else if (!expectation.equals(other.expectation))
            return false;
        if (position != other.position)
            return false;
        return true;
    }
}
