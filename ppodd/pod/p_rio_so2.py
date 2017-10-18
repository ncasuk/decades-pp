from ppodd.core import flagged_data, parameter

import numpy as np

class rio_so2_mixingratio(cal_base):
    """
    Routine to extract the SO2 concentration value from the SO2-TECO analyser
    TCP data package. The CALSO2 calibration coefficients fromt he Flight
    Constant file are used for scaling the concentration.

    This routine is currenlty not in use, because of instrument drift. The data
    will needs more processing before they can be added to the FAAM core
    netCDF.
    """

    def __init__(self,dataset):
        self.input_names = ['CALSO2',
                            'CHTSOO_conc',
                            'CHTSOO_flow',
                            'CHTSOO_flags',
                            'CHTSOO_utc_time']
                          
        self.outputs = [parameter('SO2_TECO',
                                  units='ppb',
                                  frequency=1,
                                  number=740,
                                  long_name='Mole fraction of Sulphur Dioxide in air from TECO 43 instrument',
                                  standard_name='mole_fraction_of_sulphur_dioxide_in_air')]
        self.version = 1.00
        cal_base.__init__(self,dataset)

    def process(self):
        match = self.dataset.matchtimes(self.input_names)
        so2_mr = self.dataset['CHTSOO_conc'].data.ismatch(match)

        # apply calibration coefficients
        so2_mr = so2_mr*self.dataset['CALSO2'].data[0] + \
                 self.dataset['CALSO2'].data[1]

        # initialize empty flag array, with all flags set to zero
        flag = np.array([0]*so2_mr.size, dtype=np.int8) 
        so2_teco = flagged_data(so2_mr, match, flag)
        self.outputs[0].data = so2_teco
