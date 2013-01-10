from cal_base import *
class c_rio_turb(fort_cal):
    def __init__(self,dataset):
        self.input_names=['AOA_A0', 'AOA_A1', 'AOSS_B0', 'AOSS_B1', 'TOLER', 'TASCOR1', 'ALPH0', 'ALPH1', 'BET0', 'BET1', 
                          'IAS_RVSM-rio', 'TAT_DI_R-rio', 'TAT_ND_R-rio', 'PS_RVSM-rio', 'Q_RVSM-rio', 'PALT_RVS-rio', 
                          'P0_S10-rio', 'PA_TURB-rio', 'PB_TURB-rio', 'TBPC-rio', 'TBPD-rio']
        self.outputs=[parameter('AOA-rio',units='degree',frequency=32,number=548,long_name='Angle of attack from the turbulence probe (positive, flow upwards wrt a/c axes)')
                     ,parameter('AOSS-rio',units='degree',frequency=32,number=549,long_name='Angle of sideslip from the turbulence probe (positive, flow from left)')
                     ,parameter('TAS-rio',units='m s-1',frequency=32,number=779,long_name='True airspeed (dry-air) from turbulence probe')
                     ,parameter('TASW-rio',units='MS-1',frequency=32,number=780,long_name='TURB PROBE WET TAS')
                     ,parameter('PSP_TURB-rio',units='hPa',frequency=32,number=781,long_name='Pitot-static pressure from centre-port measurements corrrected for AoA and AoSS')]
        self.name='RIO_TURB'
        self.fortname='TURB'
        self.version=1.00
        fort_cal.__init__(self,dataset)
