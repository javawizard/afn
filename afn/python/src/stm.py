
from threading import local as Local, Lock

class RetryImmediately(BaseException):
    pass

class RetryLater(BaseException):
    pass


class State(Local):
    def __init__(self):
        self.stack = []
    
    @property
    def current(self):
        if not self.stack:
            raise Exception("No current transaction. The function you're "
                            "calling most likely needs to be run inside a "
                            "call to atomically().")
        return self.stack[-1]
    
    def push(self, transaction):
        self.stack.append(transaction)
    
    def pop(self):
        self.stack.pop()

stm_state = State()
global_lock = Lock()
last_transaction = 0


class Transaction(object):
    def __init__(self):
        self.parent = None
        self.vars = {}
    
    def get_real_value(self, var):
        raise NotImplementedError
    
    def get_value(self, var):
        try:
            return self.vars[var]
        except KeyError:
            value = self.get_real_value(var)
            self.vars[var] = value
            return value
    
    def try_commit(self):
        raise NotImplementedError


class BaseTransaction(Transaction):
    def __init__(self):
        Transaction.__init__(self)
        with global_lock:
            self.start = last_transaction
    
    def get_real_value(self, var):
        with global_lock:
            if var.modified > self.start:
                raise RetryImmediately
            return var.real_value


class NestedTransaction(Transaction):












































