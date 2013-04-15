package afn.stm;

import java.util.HashMap;
import java.util.Map;

public class Transaction {
	long startedAt;

	final Map<TVar, Object> values = new HashMap<TVar, Object>();

	Transaction(long startedAt) {
		this.startedAt = startedAt;
	}

	<V> void loadTVar(TVar<V> var) {
		if (!values.containsKey(var)) {
			synchronized (TransactionManager.globalLock) {
				if(var.lastModified > startedAt)
					// Var was modified since this transaction started, so retry immediately
					throw new RetryImmediatelyException();
				// Var wasn't modified. Load its value.
				values.put(var, var.getActualValue());
			}
		}
	}
}
