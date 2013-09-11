from ppodd.core import *
class c_sols(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALUP1S', 'CALUP2S', 'CALUIRS', 'CALLP1S', 'CALLP2S', 'CALLIRS', 
                          'SECS',
                          'Horace_UPOS', 'Horace_UPIS', 'Horace_USIS', 
                          'Horace_UPOZ', 'Horace_UPIZ', 'Horace_USIZ', 
                          'Horace_UPOT', 'Horace_UPIT', 'Horace_USIT', 
                          'Horace_LS1S', 'Horace_LS2S', 'Horace_LS3S', 
                          'Horace_LS1Z', 'Horace_LS2Z', 'Horace_LS3Z', 
                          'Horace_LS1T', 'Horace_LS2T', 'Horace_LS3T']
        self.outputs=[parameter('UP1S',units='W M-2',frequency=1,number=673,long_name='UPP VIS CLR SIG')
                     ,parameter('UP2S',units='W M-2',frequency=1,number=674,long_name='UPP VIS RED SIG')
                     ,parameter('UIRS',units='W M-2',frequency=1,number=675,long_name='UPP I/R SIGNAL')
                     ,parameter('UP1Z',units='W M-2',frequency=1,number=676,long_name='UPP VIS CLR ZERO')
                     ,parameter('UP2Z',units='W M-2',frequency=1,number=677,long_name='UPP VIS RED ZERO')
                     ,parameter('UIRZ',units='W M-2',frequency=1,number=678,long_name='UPP I/R ZERO')
                     ,parameter('UP1T',units='DEG C',frequency=1,number=679,long_name='UPP VIS CLR TEMP')
                     ,parameter('UP2T',units='DEG C',frequency=1,number=680,long_name='UPP VIS RED TEMP')
                     ,parameter('UIRT',units='DEG C',frequency=1,number=681,long_name='UPP I/R TEMP')
                     ,parameter('LP1S',units='WM-2',frequency=1,number=682,long_name='LWR VIS CLR SIG')
                     ,parameter('LP2S',units='WM-2',frequency=1,number=683,long_name='LWR VIS RED SIG')
                     ,parameter('LIRS',units='WM-2',frequency=1,number=684,long_name='LWR I/R SIGNAL')
                     ,parameter('LP1Z',units='WM-2',frequency=1,number=685,long_name='LWR VIS CLR ZERO')
                     ,parameter('LP2Z',units='WM-2',frequency=1,number=686,long_name='LWR VIS RED ZERO')
                     ,parameter('LIRZ',units='WM-2',frequency=1,number=687,long_name='LWR I/R ZERO')
                     ,parameter('LP1T',units='DEG C',frequency=1,number=688,long_name='LWR VIS CLR TEMP')
                     ,parameter('LP2T',units='DEG C',frequency=1,number=689,long_name='LWR VIS RED TEMP')
                     ,parameter('LIRT',units='DEG C',frequency=1,number=690,long_name='LWR I/R TEMP')]
        self.name='SOLS'
        self.version=1.00
        fort_cal.__init__(self,dataset)
