from ppodd.core import *
class c_gsun(fort_cal):
    def __init__(self,dataset):
        self.input_names=['DATE', 'SECS', 'LAT_GIN', 'LON_GIN']
        self.outputs=[parameter('SOL_AZIM',units='degree',frequency=1,number=642,long_name='Solar azimuth derived from aircraft position and time.')
                     ,parameter('SOL_ZEN',units='degree',frequency=1,number=643,long_name='Solar zenith derived from aircraft position and time.')]
        self.name='GSUN'
        self.version=1.00
        fort_cal.__init__(self,dataset)
