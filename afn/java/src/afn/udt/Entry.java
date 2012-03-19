package afn.udt;

public interface Entry<I, V> {
    public I getItem();
    
    public I getKey();
    
    public V getValue();
    
    // TODO: add methods for changing the item and the value
}
