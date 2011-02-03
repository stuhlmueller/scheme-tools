#!/usr/bin/python
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
