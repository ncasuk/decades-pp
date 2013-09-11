from ppodd.core import *
class c_press1(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALCABP', 'CALS9SP', 'Horace_CABP', 'Horace_S9SP']
        self.outputs=[parameter('CAB_PRES',units='hPa',frequency=1,number=579,long_name='Cabin pressure')
                     ,parameter('P9_STAT',units='hPa',frequency=32,number=778,long_name='Static pressure from S9 fuselage ports')]
        self.name='PRESS1'
        self.version=1.00
        fort_cal.__init__(self,dataset)
