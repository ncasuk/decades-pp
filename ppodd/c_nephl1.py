from cal_base import *
class c_nephl1(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALNPRS', 'CALNTMP', 'CALNBTS', 'CALNGTS', 'CALNRTS', 'CALNBBS', 'CALNGBS', 'CALNRBS', 'CALNHUM', 'CALNSTS', 'NPRS', 'NTMP', 'NBTS', 'NGTS', 'NRTS', 'NBBS', 'NRBS', 'NGBS', 'NHUM', 'NSTS']
        self.outputs=[parameter('NEPH_PR',units='hPa',frequency=1,number=760,description='Internal sample pressure of the Nephelometer')
                     ,parameter('NEPH_T',units='K',frequency=1,number=761,description='Internal sample temperature of the Nephelometer')
                     ,parameter('TSC_BLUU',units='m-1',frequency=1,number=762,description='Uncorrected blue total scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('TSC_GRNU',units='m-1',frequency=1,number=763,description='Uncorrected green total scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('TSC_REDU',units='m-1',frequency=1,number=764,description='Uncorrected red total scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('BSC_BLUU',units='m-1',frequency=1,number=765,description='Uncorrected blue back scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('BSC_GRNU',units='m-1',frequency=1,number=766,description='Uncorrected green back scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('BSC_REDU',units='m-1',frequency=1,number=767,description='Uncorrected red back scattering coefficient from TSI 3563 nephelometer.')
                     ,parameter('NHUM_CAL',units='%',frequency=1,number=768,description='NEPH HUMIDITY')
                     ,parameter('NSTS_CAL',units='RBITS',frequency=1,number=769,description='NEPH STATUS')]
        self.name='NEPHL1'
        self.version=1.00
        fort_cal.__init__(self,dataset)
