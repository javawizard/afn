package afn.parcon;

/**
 * A one-argument function.
 * 
 * @author jcp
 * 
 * @param <P>
 * @param <R>
 */
public interface OneFunction<P, R> {
    public R call(P value);
}
