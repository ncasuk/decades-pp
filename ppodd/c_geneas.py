from cal_base import *
class c_geneas(fort_cal):
    def __init__(self,dataset):
        self.input_names=['GELIMS', 'CALGE', 'SECS','HYGR','HYCS']
        self.outputs=[parameter('TDEW_GE',units='degK',frequency=4,number=529,description='Dew point from the General Eastern instrument.')]
        self.name='GENEAS'
        self.version=1.00
        fort_cal.__init__(self,dataset)
