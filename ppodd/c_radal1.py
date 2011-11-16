from cal_base import *
class c_radal1(fort_cal):
    def __init__(self,dataset):
        self.input_names=['RDHT']
        self.outputs=[parameter('HGT_RADR',units='m',frequency=2,number=575,description='Radar height from the aircraft radar altimeter.')]
        self.name='RADAL1'
        self.version=1.00
        fort_cal.__init__(self,dataset)
