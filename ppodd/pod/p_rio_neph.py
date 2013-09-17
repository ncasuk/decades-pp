from ppodd.pod import *
from ppodd.core import *
class rio_neph(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALNPRS', 'CALNTMP', 'CALNBTS', 'CALNGTS', 'CALNRTS', 'CALNBBS', 'CALNGBS', 'CALNRBS', 'CALNHUM', 'CALNSTS', 
                          'AERACK_neph_pressure', 'AERACK_neph_temp', 
                          'AERACK_neph_total_blue', 'AERACK_neph_total_green', 'AERACK_neph_total_red', 
                          'AERACK_neph_backscatter_blue', 'AERACK_neph_backscatter_red', 'AERACK_neph_backscatter_green', 
                          'AERACK_neph_humidity', 'AERACK_neph_status'] # Why are the backscatter ones in a funny order ?
        self.outputs=[parameter('NEPH_PR',units='hPa',frequency=1,number=760,long_name='Internal sample pressure of the Nephelometer')
                     ,parameter('NEPH_T',units='K',frequency=1,number=761,long_name='Internal sample temperature of the Nephelometer')
                     ,parameter('TSC_BLUU',units='m-1',frequency=1,number=762,long_name='Uncorrected blue total scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('TSC_GRNU',units='m-1',frequency=1,number=763,long_name='Uncorrected green total scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('TSC_REDU',units='m-1',frequency=1,number=764,long_name='Uncorrected red total scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('BSC_BLUU',units='m-1',frequency=1,number=765,long_name='Uncorrected blue back scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('BSC_GRNU',units='m-1',frequency=1,number=766,long_name='Uncorrected green back scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('BSC_REDU',units='m-1',frequency=1,number=767,long_name='Uncorrected red back scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('NHUM_CAL',units='%',frequency=1,number=768,long_name='NEPH HUMIDITY')
                     ,parameter('NSTS_CAL',units='RBITS',frequency=1,number=769,long_name='NEPH STATUS')]
        #self.name='RIO_NEPH'
        self.fortname='NEPHL1'
        self.version=1.00
        fort_cal.__init__(self,dataset)
        
    def process(self): 
        for i in range(10):
            self.dataset[self.input_names[i+10]].number=i+175
        fort_cal.process(self)
