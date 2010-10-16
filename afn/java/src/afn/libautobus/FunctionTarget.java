package afn.libautobus;

/**
 * The actual implementation of a function. Usually, you'll use an instance of
 * ReflectedFunctionTarget, an implementation of this interface, instead of this interface
 * itself.
 * 
 * @author Alexander Boyd
 * 
 */
public interface FunctionTarget
{
    public Object invoke(Object[] args);
}
