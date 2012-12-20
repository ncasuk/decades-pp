from cal_base import *
class c_ozone1(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALO3', 'CALO3P', 'CALO3T', 'CALO3F', 'CALO3MX', 'Horace_O3', 'Horace_RVAS']
        self.outputs=[parameter('O3_TECO',units='ppb',frequency=1,number=574,long_name='Mole fraction of ozone in air from the TECO 49 instrument')]
        self.name='OZONE1'
        self.version=1.00
        fort_cal.__init__(self,dataset)
