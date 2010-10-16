package afn.libautobus;

public interface ObjectListener
{
    /**
     * Called when the initial value of the object is sent from the server and when the
     * object's value changes. Implementations of this interface must not call any
     * functions on any interfaces on Autobus from this function; since this is called
     * from the thread that reads data from the server, calling such functions will result
     * in deadlock. If you need to call any such functions, you'll need to start a new
     * thread from this function and call functions from there.
     * 
     * @param newValue
     */
    public void changed(Object newValue);
}
