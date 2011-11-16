from cal_base import *
class c_turb(fort_cal):
    def __init__(self,dataset):
        self.input_names=['AOA_A0', 'AOA_A1', 'AOSS_B0', 'AOSS_B1', 'TOLER', 'TASCOR1', 'ALPH0', 'ALPH1', 'BET0', 'BET1', 'IAS_RVSM', 'TAT_DI_R', 'TAT_ND_R', 'PS_RVSM', 'Q_RVSM', 'PALT_RVS', 'P0_S10', 'PA_TURB', 'PB_TURB', 'TBPC', 'TBPD']
        self.outputs=[parameter('AOA',units='degree',frequency=32,number=548,description='Angle of attack from the turbulence probe (positive, flow upwards wrt a/c axes)')
                     ,parameter('AOSS',units='degree',frequency=32,number=549,description='Angle of sideslip from the turbulence probe (positive, flow from left)')
                     ,parameter('TAS',units='m s-1',frequency=32,number=779,description='True airspeed (dry-air) from turbulence probe')
                     ,parameter('TASW',units='MS-1',frequency=32,number=780,description='TURB PROBE WET TAS')
                     ,parameter('PSP_TURB',units='hPa',frequency=32,number=781,description='Pitot-static pressure from centre-port measurements corrrected for AoA and AoSS')]
        self.name='TURB'
        self.version=1.00
        fort_cal.__init__(self,dataset)
