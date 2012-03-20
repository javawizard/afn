package afn.parcon;

/**
 * A two-argument function.
 * 
 * @author jcp
 * 
 * @param <A>
 * @param <B>
 * @param <R>
 */
public interface TwoFunction<A, B, R> {
    public R call(A a, B b);
}
