'''
Created on 8 Oct 2013

ppodd.util includes some useful functions for dealing with standard DECADES files
        

@author: Dave Tiddeman
'''
import time
import fnmatch
import os
import re
import datetime
import zipfile
import ftplib
import netrc
import hashlib
import threading
import StringIO
import ppodd



def fltno_date(names,fltno='????',date='????????'):
    """ Find flight number and date from a list of file names """
    for n in names:
        bn=os.path.basename(n)
        sr=re.search('_\d{8}_|_\d{8}$',bn)
        if(sr):
            date=sr.group(0)[1:9]
        sr=re.search('^[a-uw-z]\d{3}_|_[a-uw-z]\d{3}$|_[a-uw-z]\d{3}[_.]',bn.lower())
        if(sr):
            fltno=re.search('[a-z]\d{3}',sr.group(0)).group(0)
    return fltno,date
    

def create_new_fltcons(const):
    """ Create a new flight constants file from the closest one found"""
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
    """ Create a revised flight constants file with an incremented revision"""
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
    """ Find a standard file in a standard location """
    d={'CONST':('flight-cst_faam_%s_r%s_%s.txt',os.path.expandvars('$FLTCONS')),
       'NETCDF':('core_faam_%s_v???_r%s_%s.nc',os.path.expandvars('$NCDATA')),
       'NC1HZ':('core_faam_%s_v???_r%s_%s_1?z.nc',os.path.expandvars('$NCDATA')),
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


class ftpjob(threading.Thread):
    """ FTP data and MD5 file, in a background threaded process """
    def __init__(self,filename,host='ftp.badc.rl.ac.uk',login='',passwd='',account='',
                to='/incoming/faam',**kwargs):
        self.filename=filename
        self.text=False
        self.host=host
        if(self.filename.endswith('.txt')):
            self.text=True
        self.init_macro=[]
        self.login=login
        self.passwd=passwd
        self.account=account
        if(not(self.login)):
            try:
                ne=netrc.netrc()
                auth = ne.authenticators(host)
                if auth is not None:
                    self.login, self.account, self.passwd = auth
                if('init' in ne.macros):
                    self.init_macro=ne.macros['init']                
            except (netrc.NetrcParseError, IOError):
                pass
        self.to=to
        threading.Thread.__init__(self) 
             
    def run(self):
        ppodd.logger.info("ftp to %s:%s " % (self.host,self.to))
        ftp=ftplib.FTP(self.host)
        ftp.login(self.login, self.passwd, self.account)
        for m in self.init_macro:
            if m.startswith('quote '):
                m=m[6:].strip('\n')
                ftp.sendcmd(m)
        ftp.cwd(self.to)
        mode='rb'
        if(self.text):
            mode='r'
        bn=os.path.basename(self.filename)
        with open(self.filename,mode) as f:
            if(self.text):
                ftp.storlines('STOR '+bn,f)
            else:
                ftp.storbinary('STOR '+bn,f)
        md=self.make_md5()
        mdfile=bn[:bn.rfind('.')]+'.md5'
        ftp.storlines('STOR '+mdfile,md)
        ftp.quit()
        ppodd.logger.info("FTP %s successful" % bn)
        
        
        

    def make_md5(self):
        md5 = hashlib.md5()
        chunksize=2**20
        with open(self.filename) as f:
            while(True):
                chunk=f.read(chunksize)
                if(chunk):
                    md5.update(chunk)
                else:
                    break
        return StringIO.StringIO(md5.hexdigest()+'  '+os.path.basename(self.filename)+'\n')


    

   
