
"""

Make up a script for the ftp command based on what uploading

create md5sum for data

spawn job to do the ftp

"""
import ftplib
import netrc
import hashlib
import os
import threading
import StringIO

class ftpjob(threading.Thread):
    def __init__(self,filename,host='ftp.badc.rl.ac.uk',login='',passwd='',account='',
                to='/incoming/faam',**kwargs):
        if('printer' in kwargs):
            self.printer=kwargs.pop('printer')
        else:
            self.printer=sys.stdout
        self.filename=filename
        self.text=False
        self.host=host
        if(self.filename.endswith('.txt')):
            self.text=True
        self.init_macro=[]
        if(not(login)):
            try:
                ne=netrc.netrc()
                auth = ne.authenticators(host)
                if auth is not None:
                    self.login, self.account, self.passwd = auth
                if('init' in ne.macros):
                    self.init_macro=ne.macros['init']                
            except (netrc.NetrcParseError, IOError):
                pass
        else:
            self.login=login
            self.passwd=passwd
            self.account=account
        self.to=to
        threading.Thread.__init__(self) 
             
    def run(self):
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
        self.printer.write("FTP %s successful" % bn)
        
        
        

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


    

