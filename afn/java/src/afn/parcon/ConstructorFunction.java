package afn.parcon;

import java.lang.reflect.Constructor;
import java.util.Arrays;

public class ConstructorFunction<A, B, R> implements OneFunction<A, R>,
        TwoFunction<A, B, R> {
    private Class<R> c;
    
    public ConstructorFunction(Class<R> c) {
        this.c = c;
    }
    
    Constructor getConstructor(int args) {
        for (Constructor cs : c.getConstructors()) {
            if (cs.getParameterTypes().length == args)
                return cs;
        }
        throw new RuntimeException("Of " + c.getConstructors().length
                + " constructors, none have the correct number of arguments");
    }
    
    public R call(A a, B b) {
        try {
            return (R) getConstructor(2).newInstance(a, b);
        } catch (Exception e) {
            throw new RuntimeException("class " + c.getName() + " with args "
                    + a + " and " + b, e);
        }
    }
    
    public R call(A a) {
        try {
            return (R) getConstructor(1).newInstance(a);
        } catch (Exception e) {
            throw new RuntimeException("class " + c.getName() + " with arg "
                    + a, e);
        }
    }
    
}
