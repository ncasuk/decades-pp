from cal_base import *
class c_rio_airspd(fort_cal):
    def __init__(self,dataset):
        self.input_names=['TASCORR', 'PRTAFT_utc_time','PS_RVSM-rio', 'Q_RVSM-rio', 'TAT_DI_R-rio']
        self.outputs=[parameter('IAS_RVSM-rio',units='m s-1',frequency=32,number=516,long_name='Indicated air speed from the aircraft RVSM (air data) system.')
                     ,parameter('TAS_RVSM-rio',units='m s-1',frequency=32,number=517,long_name='True air speed from the aircraft RVSM (air data) system and deiced temperature.')]
        self.fortname='AIRSPD'
        self.name='RIO_AIRSPD'
        self.version=1.00
        fort_cal.__init__(self,dataset)
