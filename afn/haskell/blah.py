
import sys
for i in range(1000, 1000000, 1000):
    print i
    sys.setrecursionlimit(i)
    try:
        def example():
            example()
        example()
    except:
        pass


import sys
sys.setrecursionlimit(1000000000)
def example(i):
 x = 4
 y = 5
 y = y + i
 x = x + i
 print i
 print " -- " + str(x+y)
 example(i+1)

example(1)

