[A BF interpreter. The interpreter reads the first line of text from stdin and evaluates that as the BF program. The rest of the input is used as the input to the BF program. An example BF program that prints "Hello, World!" is:

++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.

Good luck programming! ]ev

NOTE TO SELF: consider having either a fixed set of memory positions, or have the bottom stack item be the number of positions present, or derive the number of positions off of the number of stack items. At the end of each loop, the bottom (or 2nd bottom item, if the botton item is something else) item could be the index that the BF program is at. Also consider having instructions that are like move, copy, and delete, but that operate with an index relative to the bottom of the stack instead of an index relative to the top of the stack.
