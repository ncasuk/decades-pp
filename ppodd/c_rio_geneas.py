from cal_base import *
class c_rio_geneas(fort_cal):
    def __init__(self,dataset):
        self.input_names=['GELIMS', 'CALGE', 'CORCON_utc_time','CORCON_ge_dew','CORCON_ge_cont']
        self.outputs=[parameter('TDEW_GE',units='degK',frequency=4,number=529,long_name='Dew point from the General Eastern instrument.')]
        self.name='GENEAS_RIO'
        self.fortname='GENEAS'
        self.version=1.00
        fort_cal.__init__(self,dataset)

    def process(self): 
        self.dataset[self.input_names[3]].number=58
        self.dataset[self.input_names[4]].number=59
        fort_cal.process(self)
