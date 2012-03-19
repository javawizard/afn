package afn.udt;

public interface Collection<I, V> {
    public int size();
    
    public void add(I item);
    
    public void add(I item, V value);
    
    public void add(int index, I item);
    
    public void add(int index, I item, V value);
    
    public void set(int index, I item);
    
    public void set(int index, I item, V value);
    
    public void put(I item);
    
    public void put(I item, V value);
    
    public void put(int index, I item);
    
    public void put(int index, I item, V value);
    
    public void replace(int index, I item);
    
    public void replace(int index, I item, V value);
    
}
