package afn.parcon;

import java.lang.reflect.Method;

public class ReflectiveOneFunction<P, R> implements OneFunction<P, R> {
    private Object object;
    private Method method;
    
    public ReflectiveOneFunction(Object objectOrClass, String method) {
        try {
            Method[] methods;
            if (objectOrClass instanceof Class)
                methods = ((Class) objectOrClass).getMethods();
            else {
                methods = objectOrClass.getClass().getMethods();
                object = objectOrClass;
            }
            for (Method m : methods) {
                if (m.getName().equals(method)) {
                    this.method = m;
                    break;
                }
            }
            if (this.method == null)
                throw new RuntimeException("No such *public* method");
            int argCount = this.method.getParameterTypes().length;
            if (argCount != 1)
                throw new RuntimeException("Method takes " + argCount
                        + " args, not 1 as required");
        } catch (Exception e) {
            throw new RuntimeException(
                    "Exception trying to construct method for class/object "
                            + objectOrClass + " and method " + method, e);
        }
    }
    
    @Override
    public R call(P value) {
        try {
            return (R) method.invoke(object, value);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
    
}
