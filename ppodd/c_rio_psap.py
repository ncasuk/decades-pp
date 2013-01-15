from ppodd.cal_base import *
class c_rio_psap(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALPLIN', 'CALPLOG', 'psap_lin', 'psap_log', 'psap_transmission', 'psap_flow']
        self.outputs=[parameter('PSAP_LIN',units='m-1',frequency=1,number=648,long_name='Uncorrected absorption coefficient at 565nm, linear, from PSAP.')
                     ,parameter('PSAP_LOG',units='*',frequency=1,number=649,long_name='Uncorrected absorption coefficient at 565nm, log, from PSAP.')]
        self.name='RIO_PSAP'
        self.fortname='PSAP'
        self.version=1.00
        fort_cal.__init__(self,dataset)


    def process(self): 
        for i in range(4):
            self.dataset[self.input_names[i+2]].number=i+185
        fort_cal.process(self)
