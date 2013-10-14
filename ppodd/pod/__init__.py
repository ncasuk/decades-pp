"""**pod** is the package containing all the standard processing routines.  Routines are assumed to be in modules named p_* with * being the class name of the routine.

All classes are added to a dictionary called modules.  If ppodd.pod.modules is altered before a decades_dataset is created then this altered set of modules is what will be used.


@author Dave Tiddeman
""" 
import os,fnmatch,sys,inspect
import ppodd
from ppodd.core import cal_base

def is_calmodule(mod):
    """ Checks whether it is a calibration module, by making sure it has a 'run' member """
    import inspect
    ans=False
    if inspect.isclass(mod):
        for n,typ in inspect.getmembers(mod):
            if n=='run':
                ans=True
    return ans

def getstandardmodule():
    for f in os.listdir(os.path.dirname(__file__)):
        if(f.startswith('p_') and f.endswith('.py')):
            m=f[:-3]
            try:
                mod=__import__('ppodd.pod.'+m,globals(),locals(),fromlist=[m[2:]])
                cls=getattr(mod,m[2:])
                yield cls
            except ImportError:
                ppodd.logger.warning('No module %s in %s' % (m[2:],m))

def addmodule(cls):
    if(is_calmodule(cls)):
        modules[cls.__name__.upper()]=cls
    else:
        ppodd.logger.warning('%s is not a calibration module (no run member) - not added' % cls.__name__.upper())

modules={}
for m in getstandardmodule():
    addmodule(m)

