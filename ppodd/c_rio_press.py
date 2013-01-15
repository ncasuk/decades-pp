from ppodd.cal_base import *
class c_rio_press(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALCABP', 'CALS9SP', 'CORCON_cabin_p', 'CORCON_s9_press']
        self.outputs=[parameter('CAB_PRES',units='hPa',frequency=1,number=579,long_name='Cabin pressure')
                     ,parameter('P9_STAT',units='hPa',frequency=32,number=778,long_name='Static pressure from S9 fuselage ports')]
        self.name='RIO_PRESS'
        self.fortname='PRESS1'
        self.version=1.00
        fort_cal.__init__(self,dataset)
        
    def process(self): 
        self.dataset[self.input_names[1]].number=14
        self.dataset[self.input_names[2]].number=221
        fort_cal.process(self)
