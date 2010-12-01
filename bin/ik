#!/usr/bin/python
from external import optfunc
from subprocess import Popen

def call(cmd, verbose=False, allow_fail=False):
    if verbose:
        print cmd
    p = Popen(cmd, shell=True)
    p.communicate()
    status = p.returncode
    if status != 0 and not allow_fail:
        print "command failed:\n%s" % cmd
        exit()
    else:
        return status

@optfunc.main
@optfunc.arghelp('debug', 'run ikarus in debug mode')
def main(fn, debug=False):
    """Usage: %prog <file> [options]"""
    debug = debug and "--debug" or ""
    settings = {
        "debug" : debug,
        "fn" : fn
    }
    call("ikarus %(debug)s --r6rs-script %(fn)s" % settings)
