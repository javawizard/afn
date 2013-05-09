
from distutils.core import setup

info = """
This package provides a software transactional memory system implemented in
pure Python.

`Software transactional memory <http://en.wikipedia.org/wiki/Software_transactional_temory>`_
is a mechanism that replaces locks, conditions, and such for writing concurrent
programs.

...TODO: introduction to STM classes/functions, the atomically function, the
TObject class, etc...

Let's take the usual example of a bank account system. A simple transfer
function could look like::

    def transfer(source, target, amount):
        if source.balance < amount:
            raise NotEnoughMoney
        else:
            source.balance -= amount
            target.balance += amount

That does, indeed, work, and guarantees that one will never accidentally
overdraw one's account by attempting to make two transfers at the same time.
But given STM's ability to revert the effects of transactions that throw
exceptions, we can do even better::

    def transfer(source, target, amount):
        source.balance -= amount
        target.balance += amount
        if source.balance < 0:
            raise NotEnoughMoney

This new version of transfer behaves exactly the same as our old one, but is
shorter and makes its point more clearly.

Now let's look at a different example: queues. For brevity, we'll just use a
TList for our queue instead of wrapping it with a Queue class. So, we can
implement a get function of sorts like so::

def get(tlist):
    if tlist:
        value = tlist[0]
        del tlist[0]
        return value
    else:
        raise Empty

And that will guarantee that no two threads will read the same value from any
given queue due to interleaving of operations. And then our put function is
simply the TList's append function.

Now we come to a new feature of the STM system: blocking. What happens if we
want to read an item from our queue-list, and block if an item isn't yet
available?

STM solves this particularly elegantly with a function called retry. Retry
indicates to the STM system that the current transaction has seen state that it
can't yet continue with, and so it needs to be restarted. It would be pointless
to just run the transaction over and over again in a busy wait loop, however,
so the STM system blocks until *at least one of the variables accessed during
the transaction has been modified* before starting the transaction over again.

So, with the benefit of ``retry``, we can implement our blocking ``get`` as
follows::

    def blocking_get(tlist):
        try:
            return get(tlist)
        except Empty:
            retry()

And then any calls to ``blocking_get`` will block, or *retry*, if the queue
doesn't currently have any items available.

There's an additional function, ``or_else``, that allows multiplexing several
different actions that might retry in a similar manner as the Unix ``select``
system call. I'll write up some examples for ``or_else`` later.
"""

setup(name="stm",
      version="0.1.1",
      description="A pure-Python software transactional memory system",
      long_description=info,
      author="Alexander Boyd",
      author_email="alex@opengroove.org",
      license="GNU LGPLv3",
      packages=["stm"],
#      script_name="setup.py",
     )
