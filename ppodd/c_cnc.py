from cal_base import *
class c_cnc(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CNC']
        self.outputs=[parameter('CPC_CONC',units='cm-3',frequency=1,number=568,description='Total condensation particle concentration from TSI 3025A')]
        self.name='CNC'
        self.version=1.00
        fort_cal.__init__(self,dataset)
