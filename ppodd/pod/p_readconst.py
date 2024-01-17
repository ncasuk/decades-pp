from ppodd.core import file_read,constants_parameter
import ppodd
import dateutil.parser
from datetime import date

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
        self.outputs.append(constants_parameter('Flight_Constants', ''.join(self.dataset.flight_constants)))
        dt=date.today()
        self.revision={'number':0,'date':dt.strftime("%d %b %Y"),'comment':''}
        for l in self.dataset.flight_constants:
            l=l.strip()
            if not l:  # if line is empty move on
                continue
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
                    try:
                        self.revision['number']=int(rev[0])
                        self.revision['date']=rev[1]
                        self.revision['comment']=rev[2]
                    except:
                        pass
                    
            else:
                if('!' in l):
                    l=l[0:l.index('!')].strip()
                if(l.startswith('NO')):
                    mod=l.replace('NO','',1)
                    self.dataset.nocals=self.dataset.nocals+[mod]                 
                else:
                    _values=l.split()
                    name=_values[0]
                    values=[]
                    for val in _values[1:]:
                        try:
                            values.append(float(val))
                        except:
                            values.append(str(val).strip())
                    self.outputs.append(constants_parameter(name,values))
        self.dataset.add_para('Attribute','revision_number',self.revision['number'])
    
