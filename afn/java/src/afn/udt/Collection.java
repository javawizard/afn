package afn.udt;

/**
 * A general collection type that can represent a list, a set, or a map, as well
 * as a variety of other types such as a multimap, a multiset, an ordered map,
 * an ordered set, and an ordered multimap.
 * 
 * add and set do not replace any existing items; put and replace do.
 * 
 * @author jcp
 * 
 * @param <I>
 * @param <V>
 */
public interface Collection<I, V> extends Iterable<I> {
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
    
    public void remove(I item);
    
    public void removeLast(I item);
    
    public void removeAll(I item);
    
    public void removeValue(V value);
    
    public void removeLastValue(V value);
    
    public void removeAllValues(V value);
    
    public void remove();
    
    public void removeLast();
    
    public void removeAll();
    
    public void remove(int index);
    
    public int size();
    
    public Iterable<Entry<I, V>> entries();
    
    public I getItem(int index);
    
    public V get(int index);
    
    public int getIndex(I item);
    
    public V get(I item); // TODO: consider aliasing to get(), or
    // change naming convention from getXValue to getX, then change getFirstX
    // and getLastX to getX and getLastX
    
    public V getLastIndex(I item);
    
    public V getLast(I item);
    
    public Collection<Integer, Void> getIndexes(I item);
    
    public Collection<V, Void> getValues(I item);
    
    public boolean isOrdered();
    
    public boolean allowsDuplicates();
    
    public boolean acceptsValues();
    
    public boolean hasItem(I item);
    
    public boolean hasValue(V value);
}