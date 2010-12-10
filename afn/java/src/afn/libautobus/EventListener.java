package afn.libautobus;

/**
 * A listener that can listen for an event to be fired. Implementations of this interface
 * must contain exactly one method named "fired". This method's return value will be
 * ignored (it should typically be void). It will be called when an event is fired. There
 * must only be one implementation of the method right now; an exception will be thrown
 * upon listener registration if the method has been overridden in the implementing class.
 * I hope to raise this restriction soon.
 * 
 * @author Alexander Boyd
 * 
 */
public interface EventListener
{
}
