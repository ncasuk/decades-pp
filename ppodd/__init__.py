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
            for n,typ in inspect.getmembers(mod):
                if n=='process':
                    ans=True
    return ans

def calibrate(dataset,write=True,calmods=[],nocals=[],outparas=None,start=None,end=None):
    """ Sorts calibrate modules - so they are run in order of availability of their inputs """
    dataset.start=start
    dataset.end=end
    cals=[]
    if(len(calmods)==0):
        calmods=calnames
    for c in calmods:
        if(c in calnames):
            cals.append(eval('c_%s' % c.lower()))
        else:
            print 'Warning:Module C_%s not available' % c
    notadded=cals            
    callist=[]
    notlist=[]
    finished=False
    dataset.nocals=set(nocals)
    while(len(notadded)>0):
        for cal in notadded:
            paras=dataset.para_names()
            if (outparas is not None):
                finished=True
                for i in outparas:
                    if(i not in paras):
                        finished=False
            ok=False
            if(not(finished)):
                c=cal(dataset)
                if(c.name=='WRITE_NC'):
                    if (outparas is None):
                        outparas=c.input_names
                        outparas.remove('DATE')
                        outparas.remove('FLIGHT')
                    else:
                        c.input_names=['DATE','FLIGHT']+outparas
                    print c.name,c.input_names
                inp=c.input_names
                oup=c.outputs
                ok=True
                if c.name in dataset.nocals:
                    ok=False
                for i in inp:
                    if(i not in paras):
                        ok=False
                
                if(c.name=='WRITE_NC'):
                    print 'WRITE_NC -> ',ok
                    print dataset.nocals
                    print paras
                    print inp    
            if(ok):
                print 'PROCESSING .. '+c.name
                if ((c.name=='WRITE_NC') & write) | (c.name!='WRITE_NC'):                        
                    callist.append(c)
                    c.process()
                    dataset+=oup
            else:
                notlist.append(cal)             
        if((sorted(notlist)==sorted(notadded)) | finished):    # probably need some sort of loop
            for cal in notlist:
                print 'not list ',cal
                c=cal(dataset)
                if(c.name=='WRITE_NC' and write):
                    if (outparas is not None):
                        c.input_names=['DATE','FLIGHT']+outparas
                    callist.append(c)
                    print 'PROCESSING '+c.name
                    c.process()
                    finished=True
                else:
                    dataset.nocals.update([c.name])
            break
        notadded=notlist
        notlist=[]
    return callist

version='V004'
calnames=[]
for m in getmodules():
    print 'from %s import %s' % (m,m)
    exec 'from %s import %s' % (m,m)
    if is_calmodule(eval('%s' % m)):
        calnames.append(m[2:].upper())
del m
