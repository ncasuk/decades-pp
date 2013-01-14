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
    """ Sorts calibrate modules - so they are run in order of availability of their inputs 
    and do the processing """
    dataset.starttime=start
    dataset.endtime=end
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
        """ Keep going while more modules to add """
        for cal in notadded:
            """ For each unrun module """
            paras=dataset.para_names()
            if (outparas is not None):
                """ If all output parameters are present then we have finished """
                finished=True
                for i in outparas:
                    if(i not in paras):
                        finished=False
            ok=False
            if(not(finished)):
                """ Load the modules """
                c=cal(dataset)
                if(c.name=='WRITE_NC'):
                    """ If it is the writing module sync the output parameters with it """
                    if (outparas is None):
                        outparas=c.input_names
                        outparas.remove('DATE')
                        outparas.remove('FLIGHT')
                    else:
                        c.input_names=['DATE','FLIGHT']+outparas
                inp=c.input_names
                ok=True
                """ Run the module if not listed in dataset.nocals and have all the inputs """
                if c.name in dataset.nocals:
                    ok=False
                for i in inp:
                    if(i not in paras):
                        ok=False                
            if(ok):
                """ Run the module... Only run WRITE_NC if write is True """
                print 'PROCESSING .. '+c.name
                if ((c.name=='WRITE_NC') & write) | (c.name!='WRITE_NC'):                        
                    callist.append(c)
                    c.run()
                    dataset+=c.outputs
            else:
                """ Not running the module this time add to notlist """
                notlist.append(cal)             
        if((sorted(notlist)==sorted(notadded)) | finished):    # probably need some sort of loop
            """ If we have failed to run anything in the last loop, or we know we have finished """
            for cal in notlist:
                c=cal(dataset)
                if(c.name=='WRITE_NC' and write):
                    """ If we still haven't written anything despite the write flag then we should try even though we 
                    don't have all the parameters requested 
                    If all parameters requested output everything """
                    if (outparas is not None):
                        if(outparas==['all']):
                            c.input_names=paras
                        else:
                            c.input_names=['DATE','FLIGHT']+outparas
                    callist.append(c)
                    print 'PROCESSING '+c.name
                    c.run()
                    finished=True
                else:
                    """ If we haven't run the module add it to the nocals list """
                    dataset.nocals.update([c.name])
            break # Break out of the loop as we have finished
        notadded=notlist
        notlist=[]
    return callist

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
