from ppodd.cal_base import *
class c_tpress(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALTP1', 'CALTP2', 'CALTP3', 'CALTP4', 'CALTP5', 'Horace_TBP1', 'Horace_TBP2', 'Horace_TBP3', 'Horace_TBP4', 'Horace_TBP5']
        self.outputs=[parameter('P0_S10',units='hPa',frequency=32,number=773,long_name='Calibrated differential pressure between centre(P0) port and S10 static')
                     ,parameter('PA_TURB',units='hPa',frequency=32,number=774,long_name='Calibrated differential pressure between turbulence probe vertical ports')
                     ,parameter('PB_TURB',units='hPa',frequency=32,number=775,long_name='Calibrated differential pressure between turbulence probe horizontal ports')
                     ,parameter('TBPC',units='MB',frequency=32,number=776,long_name='TURB PROBE Ca')
                     ,parameter('TBPD',units='MB',frequency=32,number=777,long_name='TURB PROBE Cb')]
        self.name='TPRESS'
        self.version=1.00
        fort_cal.__init__(self,dataset)
