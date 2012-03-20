package afn.udt;

public abstract class AbstractCollection<I, V> implements Collection<I, V> {
    public void add(I item) {
        add(item, null);
    }
    
    public void add(I item, V value) {
        add(size(), item, value);
    }
    
    public void add(int index, I item) {
        add(index, item, null);
    }
    
    public void set(int index, I item) {
        set(index, item, null);
    }
    
    public void put(I item) {
        put(item, null);
    }
    
    public void put(I item, V value) {
        put(size(), item, value);
    }
    
    public void put(int index, I item) {
        put(index, item, null);
    }
    
    public void remove(I item) {
        remove(getIndex(item));
    }
    
    public void removeLast(I item) {
        remove(getLastIndex(item));
    }
    
    public void removeAll(I item) {
        
    }
    
    public void removeValue(V value) {
        
    }
    
    public void removeLastValue(V value) {
    }
    
    public void removeAllValues(V value) {
    }
    
    public void removeAll() {
        int size = size();
        for (int i = size - 1; i >= 0; i--)
            remove(i);
    }
    
}
