from ppodd.core import *
class c_so2(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALSO2', 'Horace_SO2']
        self.outputs=[parameter('SO2_TECO',units='ppb',frequency=1,number=740,
                      long_name='Mole Fraction of Sulphur Dioxide in air from TECO 43 instrument')]
        self.name='SO2'
        self.version=1.00
        fort_cal.__init__(self,dataset)
