import code, readline
import librtk
import signal

def main():
    print "Listening on port 6785 for a RTK connection."
    c=librtk.listen(6785)
    print "Connection received, and declared as `c`"
    
    try:
        code.InteractiveConsole({"c": c}).interact()
    finally:
        c.close()

if __name__ == "__main__":
    main()
