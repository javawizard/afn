package afn.stm;

public class TVar<V> {
	long lastModified = 0;
	V value;

	public V getValue() {
		Transaction transaction = TransactionManager.currentTransaction();
		transaction.loadTVar(this);
		return (V) transaction.values.get(this);
	}

	public void setValue(V value) {
		Transaction transaction = TransactionManager.currentTransaction();
		transaction.loadTVar(this);
		transaction.values.put(this, value);
	}
	
	void setActualValue(V value) {
		this.value = value;
	}
	
	V getActualValue() {
		return this.value;
	}
	
}
