from cal_base import *
class c_temps2(fort_cal):
    def __init__(self,dataset):
        self.input_names=['TRFCTR', 'CALDIT', 'CALNDT', 'Horace_DITM', 'Horace_SREG', 'Horace_NDTM', 'SECS','PS_RVSM', 'Q_RVSM']
        self.outputs=[parameter('ITDI',units='DEG K',frequency=32,number=519,long_name='DEICED IND TEMP')
                     ,parameter('TAT_DI_R',units='degK',frequency=32,number=520,long_name='True air temperature from the Rosemount deiced temperature sensor.')
                     ,parameter('NDTI',units='DEG K',frequency=32,number=524,long_name='NONDEICED IND TEMP')
                     ,parameter('TAT_ND_R',units='degK',frequency=32,number=525,long_name='True air temperature from the Rosemeount non-deiced temperature sensor.')]
        self.name='TEMPS2'
        self.version=1.00
        fort_cal.__init__(self,dataset)
