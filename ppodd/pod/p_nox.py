from ppodd.pod import *
from ppodd.core import *
class nox(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALNO', 'CALNO2', 'CALNOX', 'CALO3F', 'CALNOMX',  
                          'Horace_NOX1', 'Horace_NOX2', 'Horace_NOX3', 'Horace_O3F', 'Horace_RVAS']
        self.outputs=[parameter('NO_TECO',units='ppb',frequency=1,number=770,long_name='Mole fraction of Nitrogen Monoxide (NO) in air from the TECO 42 instrument')
                     ,parameter('NO2_TECO',units='ppb',frequency=1,number=771,long_name='Mole fraction of Nitrogen Dioxide (NO2) in air from the TECO 42 instrument')
                     ,parameter('NOX_TECO',units='ppb',frequency=1,number=772,long_name='Mole fraction of NOx in air from the TECO 42 instrument')]
        #self.name='NOX'
        self.version=1.00
        fort_cal.__init__(self,dataset)
