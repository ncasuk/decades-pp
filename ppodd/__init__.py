import os.path
import fnmatch
import sys
import inspect
import cal_base
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
        if issubclass(mod,cal_base.cal_base):
            for n,f in inspect.getmembers(mod):
                if n=='process':
                    ans=True
    return ans

def calibrate(dataset):
    """ Sorts calibrate modules - so they are run in order of availability of their inputs """
    notadded=cals
    callist=[]
    notlist=[]
    written=False
    while(len(notadded)>0):
        paras=dataset.para_names()
        q=0
        for cal in notadded:
            q+=1
            c=cal(dataset)
            inp=c.input_names
            oup=c.outputs
            ok=True
            if c.name in nocals:
                ok=False
            for i in inp:
                if(i not in paras):
                    ok=False
            if(ok):
                print 'PROCESSING '+c.name
                callist.append(c)
                c.process()
                dataset+=oup
                if c.name=='WRITE_NC':
                   written=True
            else:
                notlist.append(cal)
        if((sorted(notlist)==sorted(notadded)) | written):    # probably need some sort of loop
            for cal in notlist:
               c=cal(dataset)
               if(not(written) and c.name=='WRITE_NC'):
                   callist.append(c)
                   print 'PROCESSING '+c.name
                   c.process()
                   written=True
               elif(c.name not in nocals):
                   nocals.append(c.name)
            for c in nocals:
               if (c in calnames): calnames.remove(c)            
            break
        notadded=notlist
        notlist=[]
    return callist

version='V004'
cals=[]
calnames=[]
nocals=[]
for m in getmodules():
    print 'from %s import %s' % (m,m)
    exec 'from %s import %s' % (m,m)
    if is_calmodule(eval('%s' % m)):
        cals.append(eval('%s' % m))
        calnames.append(m[2:].upper())
del m
