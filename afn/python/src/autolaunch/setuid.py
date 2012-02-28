"""
A module used by Autolaunch to launch processes under different effective user ids.
"""
import os

def run(program, args, uid=None, gid=None, add_first_arg=True):
    args = args[:]
    if add_first_arg:
        args[0:0] = [program]
    if gid is not None:
        os.setgid(gid)
    if uid is not None:
        os.setuid(uid)
    os.execv(program, args)


