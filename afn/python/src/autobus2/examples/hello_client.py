from autobus2 import Bus
import sys
from time import sleep

def main():
    with Bus() as bus:
        with bus.connect(sys.argv[1], int(sys.argv[2]), sys.argv[3]) as connection:
            connection["hi"]()
            connection["hi"]("and hello again!")
            connection["hi"]("what's your name?")
            sleep(2)
            connection["hi"]("good-bye!")
