[A BF interpreter. The interpreter reads the first line of text from stdin and evaluates that as the BF program. The rest of the input is used as the input to the BF program. An example BF program that prints "Hello, World!" is:

++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.

Good luck programming! ]ev

K so, the bf loop goes over and over again. The lowest item in the stack is the program to run, the next item is the index we're currently at, and the next item is the data bank, stored as a string where each value is a position. The next item up is the pointer into the data bank that we're at.

This makes things easy, since we can manipulate the pointers, code, and data bank by copying/moving relative to the bottom of the stack with gp and gk.

The fifth and sixth items are strings containing "[" and "]", respectively. The seventh item is the number of levels deep we are currently nested within a BF loop.

Every item above the seventh item, with the exception of the working items at the top of the stack, is a position marking an open bracket encountered while parsing. These are stored in reverse order, IE the 8th item is the innermost bracket encountered. When the interpreter encounters a close bracket, it decrements the seventh item, pops the 8th item from the stack, and sets the program counter to the 8th item's value.

So anyway, our interpreter first reads in the program and stores it on the stack. It then pushes 0, which is our position in the program. It then pushes a string with a single byte, char 0, which is our data. It then pushes 0, for the data pointer position. It then pushes 



[Enter a BF program to run, then press enter. Everything after that is input to your BF program.]
eo

[First, we read in the program and set up the default variables on the stack.]ev
l00gt0[[]]eu01hgb12eh















