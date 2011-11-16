from cal_base import *
class c_grflux(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALCUCF', 'CALCURF', 'CALCUIF', 'CALCLCF', 'CALCLRF', 'CALCLIF', 'UP1S', 'UP2S', 'UIRS', 'UP1Z', 'UP2Z', 'UIRZ', 'UP1T', 'UP2T', 'UIRT', 'LP1S', 'LP2S', 'LIRS', 'LP1Z', 'LP2Z', 'LIRZ', 'LP1T', 'LP2T', 'LIRT', 'SOL_AZIM', 'SOL_ZEN', 'ROLL_GIN', 'PTCH_GIN', 'HDG_GIN']
        self.outputs=[parameter('SW_DN_C',units='W m-2',frequency=1,number=1019,description='Corrected downward short wave irradiance, clear dome')
                     ,parameter('RED_DN_C',units='W m-2',frequency=1,number=1020,description='Corrected downward short wave irradiance, red dome')
                     ,parameter('IR_DN_C',units='W m-2',frequency=1,number=1021,description='Corrected downward long wave irradiance.')
                     ,parameter('SW_UP_C',units='W m-2',frequency=1,number=1022,description='Corrected upward short wave irradiance, clear dome')
                     ,parameter('RED_UP_C',units='W m-2',frequency=1,number=1023,description='Corrected upward short wave irradiance, red dome')
                     ,parameter('IR_UP_C',units='W m-2',frequency=1,number=1024,description='Corrected upward long wave irradiance')]
        self.name='GRFLUX'
        self.version=1.00
        fort_cal.__init__(self,dataset)
