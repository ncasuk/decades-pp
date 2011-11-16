#!/usr/bin/env python
from optparse import OptionParser
import ppodd

if __name__=="__main__":    
    usage="usage: %prog constants_file [options]"
    parser=OptionParser(usage=usage)
    parser.add_option("-i","--inputs",dest="inputs",default="",
           help="Raw inputs files comma separated",metavar="INPUT1:TYPE1,INPUT2:TYPE2,..INPUTn:TYPE")
    parser.add_option("-o","--output",dest="output",
           help="Output folder",metavar="OUTPUT FOLDER",default="")
    parser.add_option("-s","--start",dest="start",
           help="Start time (HH:MM:SS)",metavar="START TIME",default="")
    parser.add_option("-e","--end",dest="end",
           help="End time (HH:MM:SS)",metavar="END TIME",default="")
    (options,args)=parser.parse_args()
    options.inputs=options.inputs.split(',')
    for i in range(len(options.inputs)):
        opt=options.inputs[i].split(':')
        if (len(opt)<2):
           opt.append('RIO')
        options.inputs[i]=tuple(opt)
    d=ppodd.decades_dataset()
    for oi in options.inputs:
        d.add_file(*oi)
    print options.output
    opt=options.output.split(':')
    print opt
    if (len(opt)<2):
        opt.append('OUTPUT')
    else:
        opt[1]='OUTPUT_'+opt[1]
    print len(opt)
    print opt
    d.add_file(*opt)
    print d.files
    callist=ppodd.calibrate(d)
