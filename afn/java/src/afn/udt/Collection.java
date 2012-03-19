package afn.udt;

public interface Collection<I, V> {
    public int size();
    
    public boolean append(I item);
    
    public boolean append(I item, V value);
    
    public boolean replace(I item);
    
    public boolean replace(I item, V value);
}
