from ppodd.core import *
class c_nevz(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALNVLW', 'CALNVLR', 'CALNVLC', 'CALNVTW', 'CALNVTR', 'CALNVTC', 'CALRSL', 'CALRST', 
                          'Horace_SREG', 'Horace_NVLW', 'Horace_NVLR', 'Horace_NVLC', 
                          'Horace_NVTW', 'Horace_NVTR', 'Horace_NVTC', 'TAS_RVSM']
        self.outputs=[parameter('NV_LWC_U',units='gram m-3',frequency=8,number=602,long_name='Uncorrected liquid water content from the Nevzorov probe')
                     ,parameter('NVLR_CAL',units='V',frequency=8,number=603,long_name='NEVZOROV LIQUID REFE')
                     ,parameter('NVLC_CAL',units='V',frequency=8,number=604,long_name='NEVZOROV LIQUID COLL')
                     ,parameter('NV_TCW_U',units='gram m-3',frequency=8,number=605,long_name='Uncorrected total condensed water content from the Nevzorov probe.')
                     ,parameter('NVTR_CAL',units='V',frequency=8,number=606,long_name='NEVZOROV TOTAL REFER')
                     ,parameter('NVTC_CAL',units='V',frequency=8,number=607,long_name='NEVZOROV TOTAL COLLE')]
        self.name='NEVZ'
        self.version=1.00
        fort_cal.__init__(self,dataset)
