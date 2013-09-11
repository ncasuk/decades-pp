import os.path
import fnmatch
import sys
import inspect
from ppodd.cal_base import *
from data import *
""" Imports all c_*.py modules """
def getmodules():
    """ Gets a list of all the c_*.py files in this folder """
    ldir=sorted(os.listdir(os.path.dirname(__file__)))
    for fil in ldir:
        if fnmatch.fnmatch(fil,'c_*.py'):
            yield (os.path.basename(fil))[:-3]

def is_calmodule(mod):
    """ Checks whether it is a calibration module, by making sure it has a 'process' member """
    ans=False
    if inspect.isclass(mod):
        if issubclass(mod,cal_base):
            for n,typ in inspect.getmembers(mod):
                if n=='process':
                    ans=True
    return ans


""" Import all the calibration modules
The names are listed in calnames
"""
version='v004'
calnames=[]
for m in getmodules():
    #print 'from %s import %s' % (m,m)
    exec 'from %s import %s' % (m,m)
    if is_calmodule(eval('%s' % m)):
        calnames.append(m[2:].upper())
del m
