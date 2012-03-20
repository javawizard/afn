package afn.udt;

/**
 * A general collection type that can represent a list, a set, or a map, as well
 * as a variety of other types such as a multimap, a multiset, an ordered map,
 * an ordered set, and an ordered multimap.
 * 
 * add does not replace existing items; put does. This is irrelevant for set,
 * which always sets the item at the index to the specified item.
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
    
    public void remove(I item);
    
    public void removeLast(I item);
    
    public void removeAll(I item);
    
    public void removeValue(V value);
    
    public void removeLastValue(V value);
    
    public void removeAllValues(V value);
    
    public void removeAll();
    
    public void remove(int index);
    
    public int size();
    
    public Iterable<Entry<I, V>> entries();
    
    public I getItem(int index);
    
    public V get(int index);
    
    public int getIndex(I item);
    
    public V get(I item);
    
    public int getLastIndex(I item);
    
    public V getLast(I item);
    
    public Collection<Integer, Void> getIndexes(I item);
    
    public Collection<V, Void> getValues(I item);
    
    public I getItemForValue(V value);
    
    public int getIndexForValue(V value);
    
    public I getLastItemForValue(V value);
    
    public int getLastIndexForValue(V value);
    
    public Collection<I, Void> getItemsForValue(V value);
    
    public Collection<Integer, Void> getIndexesForValue(V value);
    
    public boolean isOrdered();
    
    public boolean allowsDuplicates();
    
    public boolean acceptsValues();
    
    public boolean hasItem(I item);
    
    public boolean hasValue(V value);
}