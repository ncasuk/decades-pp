from ppodd.core import flagged_data, parameter, cal_base

# -*- coding: utf-8 -*-
"""
In normal operation mode many zeros are done for the SO2 TECO instrument. The
zero concentrations are calculated for each zero calibration period and
linearly interpolated over the whole time period. The interpolated zeros are
removed from the raw SO2 concentration values.
>>>>>>> master

:FLAGGING:
  0. Data OK
  1. Not used
  2. Any of the alarm raised
  3. Aircraft either on the ground or instrument is calibrating

:OUTPUT:
  SO2_TECO: SO2 concentration in ppb

:REFERENCES:
  https://assets.thermofisher.com/TFS-Assets/LSG/manuals/EPM-manual-Model%2043i-hl.pdf

"""

from ppodd.core import flagged_data, parameter, cal_base
import numpy as np
import sys
from datetime import datetime


def get_cals(utc_time, cal_status, conc):
    """

    result is a list of calibration details like
        [[(start_time, end_time), average concentration, cal_type],
          ...]

    :param utc_time: timestamp in unix format
    :param cal_status: calibration status (`1`: calibration mode; `0`: measuring mode)
    :param conc: raw SO2 concentration
    """

    cal_status[0] = 0
    cal_status[-1] = 0
    absdiff = np.abs(np.diff(np.array(cal_status, dtype=np.int8)))
    cal_periods  = np.where(absdiff == 1)[0].reshape(-1, 2)

    x, y, high_or_low = [], [], []
    for i, cal_p in enumerate(cal_periods):
        # skip zero cal periods that are shorter than 10 seconds
        if (cal_p[1]-cal_p[0]) < 10:
            continue

        ix1 = cal_p[0]+5
        ix2 = cal_p[1]-2
        if np.mean(conc[ix1:ix2]) > 10:
            cal_type = 'high'
        else:
            cal_type = 'low'
        x.append([utc_time[ix1], utc_time[ix2]])
        y.append(np.mean(conc[ix1:ix2]))
        high_or_low.append(cal_type)

        # write calibration information to stdout
        timestamp = datetime.utcfromtimestamp(utc_time[cal_p[0]]).strftime('%Y-%m-%d %H:%M:%S')

        if i == 0:
            sys.stdout.write('\n    TECO SO2 Zero Calibrations\n')
            sys.stdout.write('    '+32*'-'+'\n')
            sys.stdout.write('    | time                |   zero |\n')
            sys.stdout.write('    |'+30*'-'+'|\n')
        if np.isnan(y[-1]):
            zero_string = '   nan'
        else:
            zero_string = '%6.3f' % (y[-1],)
        if cal_type == 'low':
            sys.stdout.write('    | %s | %s |\n' % (timestamp, zero_string))

    sys.stdout.write('    '+32*'-'+'\n')

    result = zip(x, y, high_or_low)
    return result


def interpolate_zero_cal_coefficient(utc_time, cal_array):
    """
    The zero offset is interpolated over many zero calibration periods.

    :param utc_time: timestamp in unix format
    :param cal_status: calibration status (`1`: calibration mode; `0`: measuring mode)
    :param conc: raw SO2 concentration
    """

    # interpolate the zero using only calibration periods labelled as 'low'
    _x = [float(c[0][0]+c[0][1])/2. for c in cal_array if c[2] == 'low']
    _y = [c[1] for c in cal_array if c[2] == 'low']
    zero_new = np.interp(utc_time, _x, _y)
    zero_new[zero_new == 0] = np.nan
    return zero_new


class rio_so2_mixingratio(cal_base):
    """
    Routine to process the SO2 measurements from the SO2-TECO analyser.

    """

    def __init__(self, dataset):
        self.input_names = ['CHTSOO_conc',
                            'CHTSOO_flags',
                            'CHTSOO_V6',
                            'CHTSOO_V8',
                            'WOW_IND']

        self.outputs = [parameter('SO2_TECO',
                                  units='ppb',
                                  frequency=1,
                                  number=740,
                                  long_name='Mole fraction of Sulphur Dioxide in air from TECO 43 instrument',
                                  standard_name='mole_fraction_of_sulphur_dioxide_in_air')]
        self.version = 1.00
        cal_base.__init__(self, dataset)

    def process(self):
        match = self.dataset.matchtimes(self.input_names)
        so2_mr_raw = self.dataset['CHTSOO_conc'].data.ismatch(match)
        sens = self.dataset['CHTSOO_sensitivity'].data.ismatch(match)
        v6 = self.dataset['CHTSOO_V6'].data.ismatch(match)
        v8 = self.dataset['CHTSOO_V8'].data.ismatch(match)
        hexflag = self.dataset['CHTSOO_flags'].data.ismatch(match)
        wow_ind = self.dataset['WOW_IND'].data.ismatch(match)

        cal_status = np.zeros(match.size, dtype=np.int8)
        cal_status[v8 != 0] = 1
        cal_status[v6 != 0] = 1

        # convert to unix time integer if necessary
        if np.issubdtype(match.dtype, np.datetime64):
            utc_time = match.astype('datetime64[s]').astype('int')
        else:
            utc_time = match

        cals = get_cals(utc_time,
                        cal_status,
                        np.array(so2_mr_raw))

        # interpolate the zero calibrations
        zero = interpolate_zero_cal_coefficient(utc_time, cals)

        # apply scaling factors
        so2_mr = (so2_mr_raw-zero)/sens

        cal_status = np.zeros(match.size, dtype=np.int8)
        # add time buffer to cal_status; the post cal time buffer for a high
        # cal to be quite extensive, because of tailing
        low_cal_buffer = (3, 5)
        high_cal_buffer = (3, 55)

        for cal in cals:
            _ix = np.where((utc_time > cal[0][0]) & (utc_time < cal[0][1]))[0]
            if cal[2] == 'low':
                _buffer = low_cal_buffer
            elif cal[2] == 'high':
                _buffer = high_cal_buffer

            for i in range(_buffer[0]*-1, _buffer[1]+1):
                _ix = list(set(list(np.concatenate((np.array(_ix), np.array(_ix)+i)))))
                _ix = np.clip(_ix, 0, len(cal_status)-1)
            cal_status[_ix] = 1

        # See manual page B-10
        # https://assets.thermofisher.com/TFS-Assets/LSG/manuals/EPM-manual-Model%2043i-hl.pdf
        alarm = np.array([1 if f[-3:] != '000' else 0 for f in hexflag])
        # initialize empty flag array, with all flags set to zero
        flag = np.zeros(so2_mr.size, dtype=np.int8)
        flag[wow_ind != 0] = 3      # flag periods when the aircraft is on the ground
        flag[alarm != 0] = 2        # flag times when any alarm is raised
        flag[cal_status != 0] = 3   # flag calibration periods
        so2 = flagged_data(so2_mr, match, flag)
        self.outputs[0].data = so2
