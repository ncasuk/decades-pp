from ppodd.pod import *
from ppodd.core import *

class potential_temp(cal_base):
    def __init__(self,dataset):
        self.input_names=['PS_RVSM','TAT_DI_R']
        self.outputs=[parameter('POT_TEMP',units='K',frequency=32,long_name='Potential Temperature')]
        #self.name='POTENTIAL_TEMP'
        self.version=1.00
        cal_base.__init__(self,dataset)
        
    def process(self):
        d=self.dataset
        match=d.matchtimes(['PS_RVSM','TAT_DI_R'])
        p1=d['PS_RVSM'].data.ismatch(match)
        t1=d['TAT_DI_R'].data.ismatch(match)
        pote=flagged_data(t1*(1000.0/p1)**(2.0/7.0),p1.times,p1.flag) #!Potential temp (K)
        pote.flag[t1.flag>p1.flag]=t1.flag[t1.flag>p1.flag]
        self.outputs[0].data=pote

