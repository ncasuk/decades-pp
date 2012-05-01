#!/usr/bin/env python
from optparse import OptionParser
import time
import re

if __name__=="__main__":    
    usage="usage: %prog [options] constants_file input_file1:type1 input_file2:type2 .. input_filen:typen"
    parser=OptionParser(usage=usage)
    parser.add_option("-o","--output",dest="output",
           help="Output folder",metavar="OUTPUT FOLDER",default="")
    # Nothing is done with the start and end options yet - not sure if they are needed..
    parser.add_option("-s","--start",dest="start",
           help="Start time (HH:MM:SS)",metavar="START TIME",default="")
    parser.add_option("-e","--end",dest="end",
           help="End time (HH:MM:SS)",metavar="END TIME",default="")
    (options,args)=parser.parse_args()
    foundout=False
    foundconst=None
    for i in range(len(args)):
        opt=args[i].split(':')
        if (len(opt)<2):
            if i==0:
                opt.append('CONST')
                foundconst=opt[0]
            else:
                opt.append('RIO')
        else:
            if(opt[1].startswith('OUTPUT')):
                foundout=True
            if(opt[1]=='CONST'):
                foundconst=opt[0]
        args[i]=tuple(opt)
    if(foundconst!=None):
        import ppodd
        d=ppodd.decades_dataset()
        for oi in args:
            d.add_file(*oi)
        if(not(foundout)):
            if(options.output==""):
                options.output=foundconst.replace('.txt','.nc')
                options.output=options.output.replace('flight-cst','core')
            opt=options.output.split(':')
            if (len(opt)<2):
                opt.append('OUTPUT')
            else:
                opt[1]='OUTPUT_'+opt[1]
            d.add_file(*opt)
            if(options.start!=""):
                start=re.sub(':','',options.start)
                start=time.mktime(time.strptime(start,'%H%M%S')) % 86400
            if(options.end!=""):
                end=re.sub(':','',options.end)
                end=time.mktime(time.strptime(end,'%H%M%S')) % 86400
        callist=ppodd.calibrate(d)
