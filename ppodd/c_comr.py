from cal_base import *
class c_comr(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALCOMR', 'CALCOMX', 'Horace_CO', 'Horace_RVAS']
        self.outputs=[parameter('CO_AERO',units='ppb',frequency=1,number=782,long_name='Mole fraction of Carbon Monoxide in air from the AERO AL5002 instrument')]
        self.name='COMR'
        self.version=1.00
        fort_cal.__init__(self,dataset)
