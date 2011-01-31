from itertools import cycle,imap
import collections
import re
import datetime
import time
import logging

LOGFILE = '/tmp/polysolve.log'
logging.basicConfig(filename=LOGFILE, level=logging.DEBUG)

def log(s):
    logging.debug("\n(%s) %s" % (str(datetime.datetime.now()), s))

def fromeach(*iters):
    return (x.next() for x in cycle(imap(iter,iters)))

def infix(lst):
    if (type(lst) != type([])) or (len(lst) == 1):
        return lst
    else:
        return [infix(j) for j in list(fromeach(lst[1:], [lst[0]]*len(lst[1:])))[:-1]]

def flatten(l):
    for el in l:
        if isinstance(el, collections.Iterable) and not isinstance(el, basestring):
            for sub in flatten(el):
                yield sub
        else:
            yield el

def unknowns(eqns):
    return set(i for i in flatten(eqns) if re.sub("[-<>,.=+*0-9 \n()]+", "", str(i)))

def timestamp():
    return time.mktime(datetime.datetime.now().timetuple())

