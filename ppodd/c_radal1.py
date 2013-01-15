from ppodd.cal_base import *
class c_radal1(fort_cal):
    def __init__(self,dataset):
        self.input_names=['Horace_RDHT']
        self.outputs=[parameter('HGT_RADR',units='m',frequency=2,number=575,long_name='Radar height from the aircraft radar altimeter.')]
        self.name='RADAL1'
        self.version=1.00
        fort_cal.__init__(self,dataset)
