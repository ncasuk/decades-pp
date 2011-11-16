from cal_base import *
class c_psap(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALPLIN', 'CALPLOG', 'PLIN', 'PLOG', 'PTRN', 'PFLO']
        self.outputs=[parameter('PSAP_LIN',units='m-1',frequency=1,number=648,description='Uncorrected absorption coefficient at 565nm, linear, from PSAP.')
                     ,parameter('PSAP_LOG',units='*',frequency=1,number=649,description='Uncorrected absorption coefficient at 565nm, log, from PSAP.')]
        self.name='PSAP'
        self.version=1.00
        fort_cal.__init__(self,dataset)
