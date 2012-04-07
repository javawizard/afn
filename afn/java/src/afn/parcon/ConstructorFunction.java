package afn.parcon;

import java.lang.reflect.Constructor;
import java.util.Arrays;

public class ConstructorFunction<A, B, R> implements OneFunction<A, R>,
        TwoFunction<A, B, R> {
    public Class<R> c;
    public boolean expandThenLists = false;
    
    public ConstructorFunction(Class<R> c) {
        this.c = c;
    }
    
    /**
     * Creates a new constructor function.
     * 
     * @param c
     *            The class to construct
     * @param expandThenLists
     *            Whether or not to expand a ThenList passed to
     *            {@link #call(Object)}. If this is false, the single-argument
     *            constructor will be invoked no matter what the parameter to
     *            {@link #call(Object)} is. If this is true, a null argument to
     *            <tt>call</tt> will result in the no-argument constructor being
     *            invoked, and a ThenList passed to <tt>call</tt> will result in
     *            the <tt>thenList.size()</tt>-arg constructor being invoked
     *            with the items in the ThenList as arguments.
     */
    public ConstructorFunction(Class<R> c, boolean expandThenLists) {
        this.c = c;
        this.expandThenLists = expandThenLists;
    }
    
    Constructor getConstructor(int args) {
        for (Constructor cs : c.getConstructors()) {
            if (cs.getParameterTypes().length == args)
                return cs;
        }
        throw new RuntimeException("Of " + c.getConstructors().length
                + " *public* constructors, none have the correct number of arguments");
    }
    
    public R call(A a, B b) {
        try {
            return (R) getConstructor(2).newInstance(a, b);
        } catch (Exception e) {
            throw new RuntimeException("class " + c.getName() + " with args "
                    + a + " and " + b, e);
        }
    }
    
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public R call(A a) {
        try {
            if (expandThenLists && (a == null || a instanceof ThenList)) {
                if (a == null)
                    return (R) getConstructor(0).newInstance();
                else {
                    ThenList t = (ThenList) a;
                    return (R) getConstructor(t.size())
                            .newInstance(t.toArray());
                }
            } else {
                return (R) getConstructor(1).newInstance(a);
            }
        } catch (Exception e) {
            throw new RuntimeException("class " + c.getName() + " with arg "
                    + a, e);
        }
    }
    
}
