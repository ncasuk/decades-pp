from cal_base import *
class c_sols(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALUP1S', 'CALUP2S', 'CALUIRS', 'CALLP1S', 'CALLP2S', 'CALLIRS', 'SECS','UPOS', 'UPIS', 'USIS', 'UPOZ', 'UPIZ', 'USIZ', 'UPOT', 'UPIT', 'USIT', 'LS1S', 'LS2S', 'LS3S', 'LS1Z', 'LS2Z', 'LS3Z', 'LS1T', 'LS2T', 'LS3T']
        self.outputs=[parameter('UP1S',units='W M-2',frequency=1,number=673,description='UPP VIS CLR SIG')
                     ,parameter('UP2S',units='W M-2',frequency=1,number=674,description='UPP VIS RED SIG')
                     ,parameter('UIRS',units='W M-2',frequency=1,number=675,description='UPP I/R SIGNAL')
                     ,parameter('UP1Z',units='W M-2',frequency=1,number=676,description='UPP VIS CLR ZERO')
                     ,parameter('UP2Z',units='W M-2',frequency=1,number=677,description='UPP VIS RED ZERO')
                     ,parameter('UIRZ',units='W M-2',frequency=1,number=678,description='UPP I/R ZERO')
                     ,parameter('UP1T',units='DEG C',frequency=1,number=679,description='UPP VIS CLR TEMP')
                     ,parameter('UP2T',units='DEG C',frequency=1,number=680,description='UPP VIS RED TEMP')
                     ,parameter('UIRT',units='DEG C',frequency=1,number=681,description='UPP I/R TEMP')
                     ,parameter('LP1S',units='WM-2',frequency=1,number=682,description='LWR VIS CLR SIG')
                     ,parameter('LP2S',units='WM-2',frequency=1,number=683,description='LWR VIS RED SIG')
                     ,parameter('LIRS',units='WM-2',frequency=1,number=684,description='LWR I/R SIGNAL')
                     ,parameter('LP1Z',units='WM-2',frequency=1,number=685,description='LWR VIS CLR ZERO')
                     ,parameter('LP2Z',units='WM-2',frequency=1,number=686,description='LWR VIS RED ZERO')
                     ,parameter('LIRZ',units='WM-2',frequency=1,number=687,description='LWR I/R ZERO')
                     ,parameter('LP1T',units='DEG C',frequency=1,number=688,description='LWR VIS CLR TEMP')
                     ,parameter('LP2T',units='DEG C',frequency=1,number=689,description='LWR VIS RED TEMP')
                     ,parameter('LIRT',units='DEG C',frequency=1,number=690,description='LWR I/R TEMP')]
        self.name='SOLS'
        self.version=1.00
        fort_cal.__init__(self,dataset)
