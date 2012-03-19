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
    
    public void removeFirst(I item);
    
    public void removeFirst();
    
    public void removeLast(I item);
    
    public void removeLast();
    
    public void removeAll(I item);
    
    public void removeAll();
    
    public void removeFirstValue(V value);
    
    public void removeLastValue(V value);
    
    public void removeAllValues(V value);
    
    public void remove(int index);
    
}