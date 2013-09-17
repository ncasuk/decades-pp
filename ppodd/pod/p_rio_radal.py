from ppodd.pod import *
from ppodd.core import *
class rio_radal(fort_cal):
    def __init__(self,dataset):
        self.input_names=['PRTAFT_rad_alt']
        self.outputs=[parameter('HGT_RADR',units='m',frequency=2,number=575,long_name='Radar height from the aircraft radar altimeter.')]
        #self.name='RIO_RADAL'
        self.fortname='RADAL1'
        self.version=1.00
        fort_cal.__init__(self,dataset)


    def process(self): 
        self.dataset[self.input_names[0]].number=37
        fort_cal.process(self)
