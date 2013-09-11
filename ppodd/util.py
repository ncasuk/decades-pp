import time
import fnmatch
import os.path
import re
import datetime

class DecadesFile(object):
    def __init__(self,dataset,filename,filetype=None):
        self.filename=filename
        self.filetypes=dataset.filetypes
        if(filetype):
            self.filetype=filetype
        else:
            self.filetype=self.guesstype(self.__file__)
        
    def __getfilename__(self):
        return self.filetypes[self.filetype].fixfilename(self.__file__)

    def __setfilename__(self,val):
        self.__file__=val
    
    filename=property(__getfilename__,__setfilename__)

    def guesstype(self,filen):
        ans='CRIOS'
        for n,o in self.filetypes.items():
            for patt in o.patterns:
                if(fnmatch.fnmatch(os.path.basename(filen),patt)):
                    ans=n
        return ans

    def astuple(self):
        return (self.filename,self.filetype)    

def fltno_date(names,fltno='X000',date='19001225',rev=0):
    for n in names:
        bn=os.path.basename(n)
        sr=re.search('_\d{8}_|_\d{8}$',bn)
        if(sr):
            date=sr.group(0)[1:9]
        sr=re.search('^[a-uw-z]\d{3}_|_[a-uw-z]\d{3}$|_[a-uw-z]\d{3}[_.]',bn.lower())
        if(sr):
            fltno=re.search('[a-z]\d{3}',sr.group(0)).group(0)
        sr=re.search('_r\d_',bn.lower())
        if(sr):
            rev=int(sr.group(0)[2])
    return fltno,date,rev

def create_new_fltcons(fltno,date):
    print 'Create'
    print 'create new constants flight-cst_faam_%s_r0_%s.txt' % (date,fltno)
    const=os.path.expandvars('$FLTCONS/flight-cst_faam_%s_r0_%s.txt' % (date,fltno))
    dnow=datetime.datetime.strptime(date,'%Y%m%d')
    dt=datetime.timedelta(365*100)
    match=''
    for l in os.listdir(os.path.expandvars('$FLTCONS/')):
        res=re.search('flight-cst_faam_\d{8}',l)
        if(res):
            d1=datetime.datetime.strptime(res.group(0)[16:24],'%Y%m%d')
            if(abs(d1-dnow)<=dt):
                dt=abs(d1-dnow)
                match=l
    print 'Closest constants %s ' % match
    fout=open(const,'w')
    with open(os.path.expandvars('$FLTCONS/%s' % match)) as f:
        for line in f:
            if(line.startswith('! FLIGHT')):
                line='! FLIGHT %s\n' % fltno
            if(line.startswith('! DATE')):
                line='! DATE %s\n' % dnow.strftime('%d %b %Y')
            if(line.startswith('! Revision')):
                line='! Revision %d - %s\n' % (0,time.strftime('%d %b %Y'))
            fout.write(line)
    fout.close()
    return const


def find_file(fltno,date,ftype='const'):
    d={'const':('flight-cst_faam_%s_r?_%s.txt',os.path.expandvars('$FLTCONS')),
       'nc':('core_faam_%s_v???_r?_%s.nc',os.path.expandvars('$NCDATA')),
       'data':('core_faam_%s_r?_%s_rawdlu',os.path.expandvars('$RAWDATA')),
       'zip':('core_faam_%s_r?_%s_rawdlu.zip',os.path.expandvars('$RAWDATA'))}
    dx=d[ftype]
    if(date>'20000101'):
        pattern=dx[0]  % (date,fltno)
    else:
        pattern=dx[0]  % ('????????',fltno)
    ans=''
    for f in os.listdir(dx[1]):
        if fnmatch.fnmatch(f,pattern):
            ans=f
    return ans



   
