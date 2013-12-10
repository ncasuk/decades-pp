"""**pod** is the package containing all the standard processing routines.  Routines are assumed to be in modules named p_* with * being the class name of the routine.

All classes are added to a dictionary called modules.  If ppodd.pod.modules is altered before a decades_dataset is created then this altered set of modules is what will be used.


@author Dave Tiddeman
""" 
import os,fnmatch,sys,inspect
import ppodd
from ppodd.core import cal_base
import inspect

def is_calmodule(mod):
    """ Checks whether it is a calibration module, by making sure it has a 'run' member """
    ans=False
    if inspect.isclass(mod):
        members=dict(inspect.getmembers(mod))
        if(('run' in members) and ('process' in members)):
            ans=True
    return ans

def addmodule(cls):
    if(is_calmodule(cls)):
        modname=cls.__name__.upper()
        if(modname not in modules):
            modules[modname]=cls
            ppodd.logger.info('Added module '+modname)

def import_modules(mods):
    try:
        mod=__import__(mods,globals(),locals(),fromlist=[''])
        for pod in dir(mod):
            c=getattr(mod,pod)
            if(inspect.isclass(c)):
                if(mod.__name__==c.__module__):
                    addmodule(c)
    except ImportError:
        ppodd.logger.warning("Can't import %s" % str(mods))
    

def getstandardmodules():
    for f in os.listdir(os.path.dirname(__file__)):
        if(f.startswith('p_') and f.endswith('.py')):
            m=f[:-3]
            import_modules('ppodd.pod.'+m)


modules={}
getstandardmodules()

