from cal_base import *
class c_rio_rflux(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALCUCF', 'CALCURF', 'CALCUIF', 'CALCLCF', 'CALCLRF', 'CALCLIF', 
        'UP1S-rio', 'UP2S-rio', 'UIRS-rio', 'UP1Z-rio', 'UP2Z-rio', 'UIRZ-rio', 'UP1T-rio', 'UP2T-rio', 'UIRT-rio', 
        'LP1S-rio', 'LP2S-rio', 'LIRS-rio', 'LP1Z-rio', 'LP2Z-rio', 'LIRZ-rio', 'LP1T-rio', 'LP2T-rio', 'LIRT-rio', 
        'SOL_AZIM-rio', 'SOL_ZEN-rio', 'ROLL_GIN', 'PTCH_GIN', 'HDG_GIN']
        self.outputs=[parameter('SW_DN_C-rio',units='W m-2',frequency=1,number=1019,long_name='Corrected downward short wave irradiance, clear dome')
                     ,parameter('RED_DN_C-rio',units='W m-2',frequency=1,number=1020,long_name='Corrected downward short wave irradiance, red dome')
                     ,parameter('IR_DN_C-rio',units='W m-2',frequency=1,number=1021,long_name='Corrected downward long wave irradiance.')
                     ,parameter('SW_UP_C-rio',units='W m-2',frequency=1,number=1022,long_name='Corrected upward short wave irradiance, clear dome')
                     ,parameter('RED_UP_C-rio',units='W m-2',frequency=1,number=1023,long_name='Corrected upward short wave irradiance, red dome')
                     ,parameter('IR_UP_C-rio',units='W m-2',frequency=1,number=1024,long_name='Corrected upward long wave irradiance')]
        self.name='RIO_RFLUX'
        self.fortname='GRFLUX'
        self.version=1.00
        fort_cal.__init__(self,dataset)

