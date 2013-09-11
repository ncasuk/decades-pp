from ppodd.core import *
class c_rio_tpress(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALTP1', 'CALTP2', 'CALTP3', 'CALTP4', 'CALTP5', 
                          'CORCON_tp_p0_s10', 'CORCON_tp_up_down', 'CORCON_tp_left_right', 'CORCON_tp_top_s10', 'CORCON_tp_right_s10']
        self.outputs=[parameter('P0_S10',units='hPa',frequency=32,number=773,long_name='Calibrated differential pressure between centre(P0) port and S10 static')
                     ,parameter('PA_TURB',units='hPa',frequency=32,number=774,long_name='Calibrated differential pressure between turbulence probe vertical ports')
                     ,parameter('PB_TURB',units='hPa',frequency=32,number=775,long_name='Calibrated differential pressure between turbulence probe horizontal ports')
                     ,parameter('TBPC',units='hPa',frequency=32,number=776,long_name='TURB PROBE Ca')
                     ,parameter('TBPD',units='hPa',frequency=32,number=777,long_name='TURB PROBE Cb')]
        self.name='RIO_TPRESS'
        self.fortname='TPRESS'
        self.version=1.00
        fort_cal.__init__(self,dataset)

    def process(self): 
        self.dataset[self.input_names[5]].number=215
        self.dataset[self.input_names[6]].number=216
        self.dataset[self.input_names[7]].number=217
        self.dataset[self.input_names[8]].number=218
        self.dataset[self.input_names[9]].number=219
        fort_cal.process(self)
