from cal_base import *
class c_nevz(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALNVLW', 'CALNVLR', 'CALNVLC', 'CALNVTW', 'CALNVTR', 'CALNVTC', 'CALRSL', 'CALRST', 'SREG', 'NVLW', 'NVLR', 'NVLC', 'NVTW', 'NVTR', 'NVTC', 'TAS_RVSM']
        self.outputs=[parameter('NV_LWC_U',units='gram m-3',frequency=8,number=602,description='Uncorrected liquid water content from the Nevzorov probe')
                     ,parameter('NVLR_CAL',units='V',frequency=8,number=603,description='NEVZOROV LIQUID REFE')
                     ,parameter('NVLC_CAL',units='V',frequency=8,number=604,description='NEVZOROV LIQUID COLL')
                     ,parameter('NV_TCW_U',units='gram m-3',frequency=8,number=605,description='Uncorrected total condensed water content from the Nevzorov probe.')
                     ,parameter('NVTR_CAL',units='V',frequency=8,number=606,description='NEVZOROV TOTAL REFER')
                     ,parameter('NVTC_CAL',units='V',frequency=8,number=607,description='NEVZOROV TOTAL COLLE')]
        self.name='NEVZ'
        self.version=1.00
        fort_cal.__init__(self,dataset)
