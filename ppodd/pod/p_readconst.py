from ppodd.core import file_read,constants_parameter
import ppodd
import dateutil.parser

class readconst(file_read):
    """
Routine for reading in CONST data
"""
    def __init__(self,dataset):
        #self.name='READCONST'
        self.input_names=['CONST']
        #self.filetype='CONST'
        self.outputs=[]
        self.patterns=('flight-cst*.txt',)
        file_read.__init__(self,dataset)
        
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
                        ppodd.logger.warning("READCONST:Can't parse Date %s" % cdate)
                elif(l.startswith('! Revision ')):
                    rev=l.replace('! Revision ','',1).split(' - ')
                    self.revision={'number':int(rev[0]),'date':rev[1]}
                    #self.dataset.attributes['revision']=int(rev[0])
                    self.dataset.add_para('Attribute','revision',int(rev[0]))
            else:
                if('!' in l):
                    l=l[0:l.index('!')].strip()
                if(l.startswith('NO')):
                    mod=l.replace('NO','',1)
                    self.dataset.nocals=self.dataset.nocals+[mod]                 
                else:
                    values=l.split()
                    name=values[0]
                    values=[float(x) for x in values[1:]]
                    self.outputs.append(constants_parameter(name,values))
 
