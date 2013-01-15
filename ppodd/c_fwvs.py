from ppodd.cal_base import *
class c_fwvs(fort_cal):
    def __init__(self,dataset):
        self.input_names=['Horace_FWVS']
        self.outputs=[parameter('FLDP',units='DEG K',frequency=1,number=573,long_name='FWVS DEW POINT')]
        self.name='FWVS'
        self.version=1.00
        fort_cal.__init__(self,dataset)
