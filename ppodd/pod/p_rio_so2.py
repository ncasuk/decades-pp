# -*- coding: utf-8 -*-
"""
The raw SO2 concentrations are scaled using the sensitivity and zero values, 
that are available from the TCP package. No calibration coefficients from the
flight constant file are used for the processing.

Routine for processing the data from the CPC (Condensation Particle Counter)
instrument TSI 3786.

:FLAGGING:

  0. Data OK
  1. Not used
  2. Not used
  3. Aircraft either on the ground or instrument is calibrating

:OUTPUT:
  SO2_TECO

"""

from ppodd.core import flagged_data, parameter, cal_base

import numpy as np

class rio_so2_mixingratio(cal_base):
    """
    Routine to extract the SO2 concentration value from the SO2-TECO analyser
    TCP data package.

    """

    def __init__(self,dataset):
        self.input_names = ['CHTSOO_conc',
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
        sens = self.dataset['CHTSOO_sensitivity'].data.ismatch(match)
        wow_ind = self.dataset['WOW_IND'].data.ismatch(match)
        
        # TODO: Current fix for the sensitivity until the Labview bug is fixed
        sens = sens*0.0 + 1.1
                
        zero = self.dataset['CHTSOO_zero'].data.ismatch(match)
        v8 = self.dataset['CHTSOO_V8'].data.ismatch(match)

        cal_status = np.array([0]*so2_mr.size, dtype=np.bool)
        cal_status[v8 != 0] = 1
        cal_status_ix = np.where(cal_status != 0)
        # add time buffer to cal_status
        cal_status_buffer = (3, 5)
        for i in range(cal_status_buffer[0]*-1, cal_status_buffer[1]+1):
            cal_status_ix = list(set(list(np.concatenate((np.array(cal_status_ix), np.array(cal_status_ix)+i)))))
        cal_status_ix = np.array(cal_status_ix)
        cal_status_ix = cal_status_ix[cal_status_ix < len(cal_status)]
        cal_status_ix = list(cal_status_ix)
        cal_status[cal_status_ix] = 1
                
        # apply calibration coefficients
        so2_mr = so2_mr*sens+zero

        # initialize empty flag array, with all flags set to zero
        flag = np.array([0]*so2_mr.size, dtype=np.int8)
        flag[wow_ind != 0] = 3      # flag periods when the aircraft is on the ground
        flag[cal_status != 0] = 3   # flag calibration periods
        so2_teco = flagged_data(so2_mr, match, flag)
        self.outputs[0].data = so2_teco
