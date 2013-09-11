from ppodd.core import *
from ppodd.util import *
import os
import zipfile
import dateutil.parser

class c_readconst(file_reader):
    def __init__(self,dataset):
        self.name='READCONST'
        self.input_names=[]
        self.filetype='CONST'
        self.outputs=[]
        file_reader.__init__(self,dataset)
        self.patterns=('flight-cst*.txt',)

    def process(self):
        file_reader.process(self)
        if not self.files:
            self.parse_filenames()
              
    def parse_filenames(self):
        print 'PARSE'
        print self.dataset.files
        print '********'
        files=[]
        for fi in self.dataset.files:
            print fi
            if(fi[0].endswith('.zip')):
                z=zipfile.ZipFile(fi[0])
                files+=z.namelist()
                z.close()
            else:
                df=DecadesFile(self.dataset,*fi)
                if(os.path.isdir(df.filename)):
                    files+=os.listdir(df.filename)
                else:
                    files.append(df.filename)
            
        fltno,date,rev=fltno_date(files)
        if('FLIGHT' not in self.dataset and fltno!='X000'):
            self.outputs.append(constants_parameter('FLIGHT',fltno))
        if('DATE' not in self.dataset and date>'19700101'):
            dt=dateutil.parser.parse(date)
            self.outputs.append(constants_parameter('DATE',[dt.day,dt.month,dt.year]))
        if(rev>self.dataset.atts['revision']):
            self.dataset.atts['revision']=rev
        
    def readfile(self,filename):
        f=open(filename)
        self.dataset.flight_constants=f.readlines()
        f.close()
        self.outputs=[]
        for l in self.dataset.flight_constants:
            l=l.strip()
            if(l.startswith('!')):
                if(l.startswith('! FLIGHT ')):
                    flight=l.replace('! FLIGHT ','',1).strip()
                    self.outputs.append(constants_parameter('FLIGHT',flight))
                elif(l.startswith('! DATE ')):
                    cdate=l.replace('! DATE ','',1).strip()
                    try:
                        dt=dateutil.parser.parse(cdate)
                        self.outputs.append(constants_parameter('DATE',[dt.day,dt.month,dt.year]))
                    except:
                        print "Can't parse Date %s" % cdate
                elif(l.startswith('! Revision ')):
                    rev=l.replace('! Revision ','',1).split(' - ')
                    self.revision={'number':int(rev[0]),'date':rev[1]}
            else:
                if('!' in l):
                    l=l[0:l.index('!')].strip()
                if(l.startswith('NO')):
                    mod=l.replace('NO','',1)
                    print 'NO "%s"' % mod
                    self.dataset.nocals.update([mod])                 
                else:
                    values=l.split()
                    name=values[0]
                    values=[float(x) for x in values[1:]]
                    self.outputs.append(constants_parameter(name,values))
 
