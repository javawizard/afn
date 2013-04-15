package afn.stm;

import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.Callable;

public class TransactionManager {
	private static final ThreadLocal<Transaction> currentTransactionLocal = new ThreadLocal<Transaction>();

	private static long lastTransactionId = 1;

	static final Object globalLock = new Object();

	static Transaction currentTransaction() {
		return currentTransactionLocal.get();
	}

	static void setCurrentTransaction(Transaction t) {
		if (currentTransactionLocal.get() != null && t != null)
			throw new RuntimeException("Already in a transaction");
		currentTransactionLocal.set(t);
	}

	public static <R> R atomically(Callable<R> c) {
		outer: while (true) {
			Transaction transaction = new Transaction(lastTransactionId);
			R result = null;
			synchronized (globalLock) {
				setCurrentTransaction(transaction);
			}
			try {
				result = c.call();
			} catch (RetryImmediatelyException e) {
				continue;
			} catch (Exception e) {
				throw new RuntimeException(e);
			} finally {
				setCurrentTransaction(null);
			}
			synchronized (globalLock) {
				for (TVar var : transaction.values.keySet()) {
					if (var.lastModified > transaction.startedAt)
						// Var was modified since we started, so retry
						// immediately
						continue outer;
				}
				// Vars haven't been modified, so we're good to commit. Get
				// ourselves a new id and then update all of the vars.
				long id = ++lastTransactionId;
				for (Entry<TVar, Object> entry : transaction.values.entrySet()) {
					entry.getKey().setActualValue(entry.getValue());
					entry.getKey().lastModified = id;
				}
			}
		}
	}
}
