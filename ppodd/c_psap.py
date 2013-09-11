from ppodd.core import *
class c_psap(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALPLIN', 'CALPLOG', 'Horace_PLIN', 'Horace_PLOG', 'Horace_PTRN', 'Horace_PFLO']
        self.outputs=[parameter('PSAP_LIN',units='m-1',frequency=1,number=648,long_name='Uncorrected absorption coefficient at 565nm, linear, from PSAP.')
                     ,parameter('PSAP_LOG',units='*',frequency=1,number=649,long_name='Uncorrected absorption coefficient at 565nm, log, from PSAP.')]
        self.name='PSAP'
        self.version=1.00
        fort_cal.__init__(self,dataset)
