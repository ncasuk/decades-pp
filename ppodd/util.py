import time
import fnmatch
import os.path
import re
import datetime
import zipfile

class DecadesFile(object):
    def __init__(self,dataset,filename,filetype=None):
        self.filetypes=dataset.filetypes
        if(filetype):
            self.filetype=filetype
        else:
            self.filetype=self.guesstype(filename)
        self.filename=filename
        
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

def fltno_date(names,fltno='????',date='????????'):
    for n in names:
        bn=os.path.basename(n)
        sr=re.search('_\d{8}_|_\d{8}$',bn)
        if(sr):
            date=sr.group(0)[1:9]
        sr=re.search('^[a-uw-z]\d{3}_|_[a-uw-z]\d{3}$|_[a-uw-z]\d{3}[_.]',bn.lower())
        if(sr):
            fltno=re.search('[a-z]\d{3}',sr.group(0)).group(0)
    return fltno,date

def parse_filenames(dataset,**kwargs):
    if('files' in kwargs):
        fs=kwargs.pop('files')
    else:
        fs=dataset.files
    files=[]
    for fi in fs:
        df=DecadesFile(dataset,*fi)
        files.append(df.filename)
        if(df.filetype=='ZIP'):
            if(os.path.isfile(df.filename)):
                z=zipfile.ZipFile(df.filename)
                files+=z.namelist()
                z.close()
        elif(os.path.isdir(df.filename)):
            files+=os.listdir(df.filename)
        
    return fltno_date(files,**kwargs)
    

def create_new_fltcons(const):
    fltcons=os.path.dirname(const)
    res=re.search('flight-cst_faam_\d{8}_r0_....',const)
    if(res):
        dnow=datetime.datetime.strptime(res.group(0)[16:24],'%Y%m%d')
        fltno=res.group(0)[28:32]
        dt=datetime.timedelta(365*100)
        match=''
        for l in os.listdir(fltcons):
            res=re.search('flight-cst_faam_\d{8}',l)
            if(res):
                d1=datetime.datetime.strptime(res.group(0)[16:24],'%Y%m%d')
                if(abs(d1-dnow)<=dt):
                    dt=abs(d1-dnow)
                    match=l
        if(match):
            fout=open(const,'w')
            with open(os.path.join(fltcons,match)) as f:
                for line in f:
                    if(line.startswith('! FLIGHT')):
                        line='! FLIGHT %s\n' % fltno
                    if(line.startswith('! DATE')):
                        line='! DATE %s\n' % dnow.strftime('%d %b %Y')
                    if(line.startswith('! Revision')):
                        line=''
                    fout.write(line)
            fout.write('! Revision 0 - %s\n' % time.strftime('%d %b %Y'))
            fout.close()
            return const

def revise_fltcons(filename):
    fn=os.path.basename(filename)
    fltcons=os.path.dirname(filename)
    sr=re.search('_r\d+_',fn)
    if(sr):
        rev=int(sr.group(0)[2:-1])
        const=os.path.join(fltcons,fn.replace('_r%d_' % rev,'_r%d_' % (rev+1)))
        rev+=1
        fout=open(const,'w')
        with open(filename) as f:
            for line in f:
                fout.write(line)
        line='! Revision %d - %s\n' % (rev,time.strftime('%d %b %Y'))
        fout.write(line)
        fout.close()
        return const


def find_file(fltno='????',date='????????',rev='*',ftype='CONST'):
    d={'CONST':('flight-cst_faam_%s_r%s_%s.txt',os.path.expandvars('$FLTCONS')),
       'NC':('core_faam_%s_v???_r%s_%s.nc',os.path.expandvars('$NCDATA')),
       'NC1HZ':('core_faam_%s_v???_r%s_%s_1?z.nc',os.path.expandvars('$NCDATA')),
       'CRIOS':('core_faam_%s_r%s_%s_rawdlu',os.path.expandvars('$RAWDATA')),
       'ZIP':('core_faam_%s_r%s_%s_rawdlu.zip',os.path.expandvars('$RAWCORE'))}
    dx=d[ftype]
    pattern=dx[0]  % (date,rev,fltno)
    ans=''
    for f in os.listdir(dx[1]):
        if fnmatch.fnmatch(f,pattern):
            if f>ans: 
                ans=f
    if(ans):
        ans=os.path.join(dx[1],ans)
    if(fltno=='????' and date=='????????'):
        if(ftype=='CONST'):
            flt,dat=fltno_date([ans])
            flt=flt[0]+'%3.3d' % (int(flt[1:])+1)
            ans=os.path.join(dx[1],dx[0] % (time.strftime('%Y%m%d'),'0',flt))
        else:
            ans=''
    if(not(ans)):
        ans=os.path.join(dx[1],dx[0] % (date,'0',fltno))
    return ans



   
