from ppodd.pod import *
from ppodd.core import *
class gwinds(fort_cal):
    def __init__(self,dataset):
        self.input_names=['INSPOSN', 'SECS_GIN', 
        'TAS', 'VELN_GIN', 'VELE_GIN', 'VELD_GIN', 'ROLL_GIN', 'PTCH_GIN', 'HDG_GIN', 'ROLR_GIN', 'PITR_GIN', 'HDGR_GIN', 
        'AOA', 'AOSS']
        self.outputs=[parameter('V_C',units='m s-1',frequency=32,number=714,long_name='Northward wind component from turbulence probe and GIN')
                     ,parameter('U_C',units='m s-1',frequency=32,number=715,long_name='Eastward wind component from turbulence probe and GIN')
                     ,parameter('W_C',units='m s-1',frequency=32,number=716,long_name='Vertical wind component from turbulence probe and GIN')]
        #self.name='GWINDS'
        self.version=1.00
        fort_cal.__init__(self,dataset)
