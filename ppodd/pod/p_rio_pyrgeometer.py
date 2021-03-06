# -*- coding: utf-8 -*-

import numpy as np
from ppodd.core import flagged_data, parameter, cal_base

def thermistor(resistance):
    """The thermistor is a YSI-44031. Formula is taken from the spec sheet
    supplied by Kipp & Zonen

    :param resistance: measured resistance in Ohm
    :type resistance: numpy.array    
    :result: temperature in Kelvin
    :type result: numpy.array
    """
    
    alpha = 1.0295*(10**-3)
    beta = 2.391*(10**-4)
    gamma = 1.568*(10**-7)
    T = (alpha+(beta*np.log(resistance)+gamma*np.log(resistance)**3))**-1
    return T


def crg4(ampage, temperature):
    """Formula for the Kipp & Zonen CRG4 Pyranometer for calculating the long
    wave flux using the ampage and temperature output.
    No calibration coefficients are needed, because the Ampbox carries the
    sensor specific calibration.

    :param ampage: in milliAmps
    :type ampage: numpy.array
    :param temperature: body temperature of the Pyrgeometer in Kelvin
    :type temperature: numpy.array
    :result: Radiation flux in Wm-2
    :type result: numpy.array
    """
    
    Ioset = 4.0
    gain = 50.0
    Eoset = 600.0
    L_d = (ampage-Ioset)*gain+(5.67e-8*(temperature**4))-Eoset
    return L_d


class rio_pyrgeometer(cal_base):
    """
Calculating of the upward and downward long wave fluxes from the
Kipp & Zonen CR4 Pyrgeometers. Those pyrgeometers were fitted for the first
time ahead of the CLARIFY campaign (August 2017).

:INPUTS:
  | LOWBBR_radiometer_3_sig
  | LOWBBR_radiometer_3_temp
  | UPPBBR_radiometer_3_sig
  | UPPBBR_radiometer_3_temp
                            

:OUTPUTS:
  | IR_DN_C: Downward long wave flux
  | IR_UP_C: Upwards  long wave flux

:FLAGGING:
  Not don eyet

"""

    def __init__(self, dataset):
        """
        :param dataset: dataset for flight
        :type dataset: ppodd.core.decades_dataset
        """
        
        # TODO: Add linear scaling coefficients; requested by Ian Rule
        self.input_names = ['LOWBBR_radiometer_3_sig',
                            'LOWBBR_radiometer_3_temp',
                            'UPPBBR_radiometer_3_sig',
                            'UPPBBR_radiometer_3_temp',
                            'WOW_IND']

        self.outputs = [parameter('IR_DN_C',
                                  units='W m-2',
                                  frequency=1,
                                  number=1021,
                                  long_name='Corrected downward long wave irradiance'),
                       parameter('IR_UP_C',
                                 units='W m-2',
                                 frequency=1,
                                 number=1024,
                                 long_name='Corrected upward long wave irradiance')]

        self.version = 1.00
        cal_base.__init__(self,dataset)

    def process(self):
        match = self.dataset.matchtimes(self.input_names)

        low_sig = self.dataset['LOWBBR_radiometer_3_sig'].ismatch(match)
        low_temp = self.dataset['LOWBBR_radiometer_3_temp'].ismatch(match)

        upp_sig = self.dataset['UPPBBR_radiometer_3_sig'].ismatch(match)
        upp_temp = self.dataset['UPPBBR_radiometer_3_temp'].ismatch(match)

        wow_ind = self.dataset['WOW_IND'].ismatch(match)

        # CRIO DLU specific characteristics
        dlu_range = 20       # -+10 Range Volt
        resolution = 2**16   # bit

        # convert DLU raw counts to Voltage
        low_sig_v = low_sig*(float(dlu_range)/resolution)
        upp_sig_v = upp_sig*(float(dlu_range)/resolution)

        # convert DLU raw counts to Kelvin
        low_temp_v = low_temp*(float(dlu_range)/resolution)
        upp_temp_v = upp_temp*(float(dlu_range)/resolution)

        # temperature
        low_temp_tot_ohm = low_temp_v/(100.E-6)
        low_temp_ohm = 1.0/((1.0/low_temp_tot_ohm)-(1.E-5))

        upp_temp_tot_ohm = upp_temp_v/(100.e-6)
        upp_temp_ohm = 1.0/((1.0/upp_temp_tot_ohm)-(1.E-5))

        # Calculate instrument body temperature
        upp_cr4_temp = thermistor(upp_temp_ohm)
        low_cr4_temp = thermistor(low_temp_ohm)

        # Ampbox
        low_ampbox_output = (low_sig_v/350.)*1000.
        upp_ampbox_output = (upp_sig_v/350.)*1000.

        # Calculate longwave radiation
        low_l_d = crg4(low_ampbox_output, low_cr4_temp)
        upp_l_d = crg4(upp_ampbox_output, upp_cr4_temp)

        n = low_l_d.size
        # create default flag array set to 0
        flag = np.array([0]*n, dtype=np.int8)
        flag[wow_ind != 0] == 1

        # Define output parameters
        self.outputs[0].data = flagged_data(upp_l_d, match, flag)
        self.outputs[1].data = flagged_data(low_l_d, match, flag)
        result = {}
        result['upp_cr4_temp'] = upp_cr4_temp
        result['low_cr4_temp'] = low_cr4_temp
        result['low_l_d'] = low_l_d
        result['upp_l_d'] = upp_l_d
        result['low_ampbox_output'] = low_ampbox_output
        result['upp_ampbox_output'] = upp_ampbox_output
        return result
