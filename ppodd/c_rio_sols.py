from cal_base import *
class c_rio_sols(fort_cal):
    def __init__(self,dataset):
        self.input_names=['CALUP1S', 'CALUP2S', 'CALUIRS', 'CALLP1S', 'CALLP2S', 'CALLIRS', 
                          'LOWBBR_utc_time', 
                          'UPPBBR_radiometer_1_sig', 'UPPBBR_radiometer_2_sig', 'UPPBBR_radiometer_3_sig', 
                          'UPPBBR_radiometer_1_zero', 'UPPBBR_radiometer_2_zero', 'UPPBBR_radiometer_3_zero', 
                          'UPPBBR_radiometer_1_temp', 'UPPBBR_radiometer_2_temp', 'UPPBBR_radiometer_3_temp',
                          'LOWBBR_radiometer_1_sig', 'LOWBBR_radiometer_2_sig', 'LOWBBR_radiometer_3_sig', 
                          'LOWBBR_radiometer_1_zero', 'LOWBBR_radiometer_2_zero', 'LOWBBR_radiometer_3_zero', 
                          'LOWBBR_radiometer_1_temp', 'LOWBBR_radiometer_2_temp', 'LOWBBR_radiometer_3_temp']
        self.outputs=[parameter('UP1S-rio',units='W M-2',frequency=1,number=673,long_name='UPP VIS CLR SIG')
                     ,parameter('UP2S-rio',units='W M-2',frequency=1,number=674,long_name='UPP VIS RED SIG')
                     ,parameter('UIRS-rio',units='W M-2',frequency=1,number=675,long_name='UPP I/R SIGNAL')
                     ,parameter('UP1Z-rio',units='W M-2',frequency=1,number=676,long_name='UPP VIS CLR ZERO')
                     ,parameter('UP2Z-rio',units='W M-2',frequency=1,number=677,long_name='UPP VIS RED ZERO')
                     ,parameter('UIRZ-rio',units='W M-2',frequency=1,number=678,long_name='UPP I/R ZERO')
                     ,parameter('UP1T-rio',units='DEG C',frequency=1,number=679,long_name='UPP VIS CLR TEMP')
                     ,parameter('UP2T-rio',units='DEG C',frequency=1,number=680,long_name='UPP VIS RED TEMP')
                     ,parameter('UIRT-rio',units='DEG C',frequency=1,number=681,long_name='UPP I/R TEMP')
                     ,parameter('LP1S-rio',units='WM-2',frequency=1,number=682,long_name='LWR VIS CLR SIG')
                     ,parameter('LP2S-rio',units='WM-2',frequency=1,number=683,long_name='LWR VIS RED SIG')
                     ,parameter('LIRS-rio',units='WM-2',frequency=1,number=684,long_name='LWR I/R SIGNAL')
                     ,parameter('LP1Z-rio',units='WM-2',frequency=1,number=685,long_name='LWR VIS CLR ZERO')
                     ,parameter('LP2Z-rio',units='WM-2',frequency=1,number=686,long_name='LWR VIS RED ZERO')
                     ,parameter('LIRZ-rio',units='WM-2',frequency=1,number=687,long_name='LWR I/R ZERO')
                     ,parameter('LP1T-rio',units='DEG C',frequency=1,number=688,long_name='LWR VIS CLR TEMP')
                     ,parameter('LP2T-rio',units='DEG C',frequency=1,number=689,long_name='LWR VIS RED TEMP')
                     ,parameter('LIRT-rio',units='DEG C',frequency=1,number=690,long_name='LWR I/R TEMP')]
        self.name='C_RIO_SOLS'
        self.fortname='SOLS'
        self.version=1.00
        fort_cal.__init__(self,dataset)
        
    def process(self):
        rawnumbers=[ 81, 82, 83, 84, 85, 86, 87, 88, 89, 91, 92, 93, 94, 95, 96, 97, 98, 99]
        for i,n in enumerate(self.input_names[7:]):
            self.dataset[n].number=rawnumbers[i]
        fort_cal.process(self)
