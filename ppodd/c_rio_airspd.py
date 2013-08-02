from ppodd.cal_base import *
class c_rio_airspd(fort_cal):
    def __init__(self,dataset):
        self.input_names=['TASCORR', 'PRTAFT_utc_time','PS_RVSM', 'Q_RVSM', 'TAT_DI_R']
        self.outputs=[parameter('IAS_RVSM',units='m s-1',frequency=32,number=516,long_name='Indicated air speed from the aircraft RVSM (air data) system.')
                     ,parameter('TAS_RVSM',units='m s-1',frequency=32,number=517,long_name='True air speed from the aircraft RVSM (air data) system and deiced temperature.')]
        self.name='RIO_AIRSPD'
        self.version=1.00
        self.fortname='AIRSPD'
        fort_cal.__init__(self,dataset)
