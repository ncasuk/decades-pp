from cal_base import *
class c_so2(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALSO2', 'SO2']
        self.outputs=[parameter('SOMR',units='PPB',frequency=1,number=740,description='SO2 MIXING RATIO')]
        self.name='SO2'
        self.version=1.00
        fort_cal.__init__(self,dataset)
