#!/usr/bin/env python

import sys
from os.path import join, dirname, realpath
afn_path = dirname(realpath(__file__))
sys.path.append(join(afn_path, "src"))
if len(sys.argv) <= 1:
    print 'usage: run python_module_name'
    print 'adds "src" to the path, then imports the specified module and calls'
    print 'its main function with no arguments. The argument representing the'
    print 'module name will be removed from sys.argv first so that additional'
    print 'arguments can be provided to the module.'
    sys.exit(0)
module_name = sys.argv[1]
del sys.argv[1]
module = __import__(module_name)
if "." in module_name:
    subnames = module_name.split(".")[1:]
    for subname in subnames:
        module = getattr(module, subname)
module.main()
