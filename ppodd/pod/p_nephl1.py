from ppodd.pod import *
from ppodd.core import *
class nephl1(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALNPRS', 'CALNTMP', 'CALNBTS', 'CALNGTS', 'CALNRTS', 'CALNBBS', 'CALNGBS', 'CALNRBS', 'CALNHUM', 'CALNSTS', 
                          'Horace_NPRS', 'Horace_NTMP', 
                          'Horace_NBTS', 'Horace_NGTS', 'Horace_NRTS', 
                          'Horace_NBBS', 'Horace_NRBS', 'Horace_NGBS', 
                          'Horace_NHUM', 'Horace_NSTS']
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
        #self.name='NEPHL1'
        self.version=1.00
        fort_cal.__init__(self,dataset)
