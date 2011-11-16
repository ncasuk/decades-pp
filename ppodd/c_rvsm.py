from cal_base import *
class c_rvsm(fort_cal):
    def __init__(self,dataset):
        self.input_names=['RVAL', 'RVAS']
        self.outputs=[parameter('PS_RVSM',units='hPa',frequency=32,number=576,description='Static pressure from the aircraft RVSM (air data) system')
                     ,parameter('Q_RVSM',units='hPa',frequency=32,number=577,description='Pitot static pressure inverted from RVSM (air data) system indicated airspeed')
                     ,parameter('PALT_RVS',units='m',frequency=32,number=578,description='Pressure altitude from the aircraft RVSM (air data) system')]
        self.name='RVSM'
        self.version=1.00
        fort_cal.__init__(self,dataset)
