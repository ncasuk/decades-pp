# -*- coding: utf-8 -*-
"""
Processing module for the Nevzorov instrument. The module handles the data from
both vane types: The old original vane with two references and the new design
that has only one reference collector. The distinction is done in the flight
constant file using the variable ``VANETYPE`` which can be set to either
``1T1L2R`` or ``1T2L1R``.

:FLAGGING:

  0. Data OK
  1. Not used
  2. Not used
  3. Aircraft on the ground

:OUTPUT:

  Voltages of collectors
  Amparage of collectors
  Power of collectors
  Voltage of reference
  Amparag of reference
  Power of reference
  Uncalibrated water contents
  Calibrated water contents

"""

import sys
from ppodd.core import cal_base, flagged_data, parameter

import numpy as np


def get_no_cloud_mask(twc_col_p, wow, _buffer=3):
    """
    Create a mask where times in cloud are indicated by zero
    and time outside of cloud (*no* total water) are indicated by a
    one.
    The way we determine whether cloud or not is present is looking at the
    range (max-min) of the power reading of the total water collector from the
    Nevzorov instrument. The range inside a cloud should be much higher than
    outside.

    :param twc_col_p: Total water Collector power (W)
    :type twc_col_p: numpy.array of floats
    :param wow: Weight on wheels indicator ('1' aircraft on the ground)
    :type wow: numpy.array
    :param int _buffer: add time buffer to the cloud detection (in secs)
    :returns: maske ('0' cloud; '1' no cloud
    :rtype: numpy.array

    """
    # set range limits for a one second measurement
    # interval (=64 single measurements)
    rng_limits = (1E-12, 0.1)
    mask = np.zeros(twc_col_p.shape[0], dtype=np.int8)

    rng = np.max(twc_col_p, axis=1)-np.min(twc_col_p, axis=1)
    mask[(rng > rng_limits[0]) &
         (rng < rng_limits[1]) &
         np.max(wow == 0, axis=1)] = 1
    # add two second time buffer, so that all data two seconds before
    # and after the estimated mask are also flagged as in cloud
    ix = np.where(mask == 0)[0]
    for i in range(_buffer*-1, _buffer+1):
        ix = list(set(list(np.concatenate((np.array(ix), np.array(ix)+i)))))
    # make sure that the indices do not exceed array dimensions
    ix = np.clip(ix, 0, len(mask)-1)
    mask[ix] = 0
    return mask


def get_fitted_k(col_p, ref_p, ias, ps, no_cloud_mask, k):
    """
    The Nevzorov baseline is not constant, but varies as a function of
    indicated air speed (IAS_RVSM) and static air pressure (PS_RVSM).
    Abel et al. (2014) provide a fitting formula in Appendix A to correct
    the K value (ratio between collector and reference power, when outside of
    clouds) to remove the zero offset of the liquid and total water
    measurements.

    :param col_p: collector power (W)
    :param ref_p: Reference power (W)
    :param ias: Indicated airspeed (m s-1)
    :param ps: Static pressure (mb)
    :param no_cloud_mask: array indicating if in (0) or out (1) of cloud
    :param k: K value which is defined in the flight-cst file
    :return: fitted values

    :Reference:
      S J Abel, R J Cotton, P A Barrett and A K Vance. A comparison of ice
      water content measurement techniques on the FAAM BAe-146 aircraft.
      Atmospheric Measurement Techniques 7(5):4815--4857, 2014.

    """

    from scipy.optimize import curve_fit

    # fitting function
    def func(x, a, b):
        return x[0, :]/x[1, :]-k-(a*(1.0/x[2, :])+b*np.log10(x[3, :]))

    ix = np.where(no_cloud_mask == 1)[0]
    xdata = np.vstack([np.array(col_p)[ix, :].ravel(),
                       np.array(ref_p)[ix, :].ravel(),
                       np.array(ias)[ix, :].ravel(),
                       np.array(ps)[ix, :].ravel()])
    popt, pcov = curve_fit(func, xdata, xdata[0, :]*0.0)
    return (k+(popt[0]*(1.0/ias)+popt[1]*np.log10(ps)), popt)


class rio_nevzorov_1t2l1r(cal_base):
    """
    Processing module for the  Nevzorov vane that has:
      | 1x Total Water sensor (TWC)
      | 2x Liquid Water sensors (LWC1 & LWC2)
      | 1x reference

    """

    def __init__(self, dataset):
        """
        Defining of the input and output parameters that are necessary for
        processing.

        :param dataset: dataset for flight
        :type dataset: ppodd.core.decades_dataset
        """

        self.input_names = ['CORCON_nv_lwc_vcol',
                            'CORCON_nv_lwc_icol',
                            'CORCON_nv_lwc_vref',
                            'CORCON_nv_lwc_iref',
                            'CORCON_nv_twc_vcol',
                            'CORCON_nv_twc_icol',
                            'CORCON_nv_twc_vref',
                            'CORCON_nv_twc_iref',
                            'TAS_RVSM',
                            'IAS_RVSM',
                            'PS_RVSM',
                            'WOW_IND',
                            'CLWCIREF', 'CLWCVREF', 'CLWCICOL', 'CLWCVCOL',
                            'CTWCIREF', 'CTWCVREF', 'CTWCICOL', 'CTWCVCOL',
                            'CALNVTWC',
                            'CALNVLWC1',
                            'CALNVLWC2',
                            'CALNVL']

        self.outputs = [parameter('NV_TWC_U',
                                  units='gram m-3',
                                  frequency=64,
                                  long_name='Uncorrected total condensed water content from the Nevzorov probe'),
                        parameter('NV_LWC1_U',
                                  units='gram m-3',
                                  frequency=64,
                                  long_name='Uncorrected liquid water content from the Nevzorov probe (1st collector)',
                                  standard_name='mass_concentration_of_liquid_water_in_air'),
                        parameter('NV_LWC2_U',
                                  units='gram m-3',
                                  frequency=64,
                                  long_name='Uncorrected liquid water content from the Nevzorov probe (2nd collector)',
                                  standard_name='mass_concentration_of_liquid_water_in_air'),
                        parameter('NV_TWC_C',
                                  units='gram m-3',
                                  frequency=64,
                                  long_name='Corrected total condensed water content from the Nevzorov probe'),
                        parameter('NV_LWC1_C',
                                  units='gram m-3',
                                  frequency=64,
                                  long_name='Corrected liquid water content from the Nevzorov probe (1st collector)',
                                  standard_name='mass_concentration_of_liquid_water_in_air'),
                        parameter('NV_LWC2_C',
                                  units='gram m-3',
                                  frequency=64,
                                  long_name='Corrected liquid water content from the Nevzorov probe (2nd collector)',
                                  standard_name='mass_concentration_of_liquid_water_in_air'),
                        parameter('NV_TWC_P',
                                  units='W',
                                  frequency=64,
                                  long_name='TWC collector power'),
                        parameter('NV_LWC1_P',
                                  units='W',
                                  frequency=64,
                                  long_name='LWC1 collector power'),
                        parameter('NV_LWC2_P',
                                  units='W',
                                  frequency=64,
                                  long_name='LWC2 collector power'),
                        parameter('NV_REF_P',
                                  units='W',
                                  frequency=64,
                                  long_name='Reference power')]

        self.version = 1.00
        cal_base.__init__(self, dataset)

    def process(self):
        np.seterr(divide='ignore')  # suppress divide by zero messages
        t = self.dataset.matchtimes(self.input_names)

        # Dictionary that is used for translating the variable
        # names in the dataset; the original DLU variable names were defined
        # for the 'old' vane type with two reference sensors. Therefore the
        # variable names need to be adapted for the new vane design
        dictionary = [('CORCON_nv_lwc1_vcol', 'CORCON_nv_lwc_vcol'),
                      ('CORCON_nv_lwc1_icol', 'CORCON_nv_lwc_icol'),
                      ('CORCON_nv_lwc1_vref', 'CORCON_nv_lwc_vref'),
                      ('CORCON_nv_lwc1_iref', 'CORCON_nv_lwc_iref'),
                      ('CORCON_nv_lwc2_vcol', 'CORCON_nv_twc_vref'),
                      ('CORCON_nv_lwc2_icol', 'CORCON_nv_twc_iref'),
                      ('CORCON_nv_lwc2_vref', 'CORCON_nv_lwc_vref'),
                      ('CORCON_nv_lwc2_iref', 'CORCON_nv_lwc_iref'),
                      ('CORCON_nv_twc_vref',  'CORCON_nv_lwc_vref'),
                      ('CORCON_nv_twc_iref',  'CORCON_nv_lwc_iref'),
                      ('CLWC1ICOL', 'CLWCICOL'),
                      ('CLWC1VCOL', 'CLWCVCOL'),
                      ('CLWC1IREF', 'CLWCIREF'),
                      ('CLWC1VREF', 'CLWCVREF'),
                      ('CLWC2ICOL', 'CTWCIREF'),
                      ('CLWC2VCOL', 'CTWCVREF'),
                      ('CLWC2IREF', 'CLWCIREF'),
                      ('CLWC2VREF', 'CLWCVREF'),
                      ('CTWCICOL',  'CTWCICOL'),
                      ('CTWCVCOL',  'CTWCVCOL'),
                      ('CTWCIREF',  'CLWCIREF'),
                      ('CTWCVREF',  'CLWCVREF')]

        # translating decades_dataset dictionary keys
        for d in dictionary:
            self.dataset[d[0]] = self.dataset[d[1]]

        insts = ['twc', 'lwc1', 'lwc2']
        measurements = ['icol', 'vcol', 'iref', 'vref']
        cal = {}
        times = self.dataset['CORCON_nv_lwc_vcol'].ismatch(t).times2d
        sh = times.shape
        times = times.ravel()
        nvl = self.dataset['CALNVL'][0]

        tas = self.dataset['TAS_RVSM'].data.ismatch(t).ravel()
        tas = tas.interp(times=times).reshape(sh)

        ias = self.dataset['IAS_RVSM'].data.ismatch(t).ravel()
        ias = ias.interp(times=times).reshape(sh)

        ps = self.dataset['PS_RVSM'].data.ismatch(t).ravel()
        ps = ps.interp(times=times).reshape(sh)

        wow_ind = self.dataset['WOW_IND'].data.ismatch(t).ravel()
        wow_ind = wow_ind.interp(times=times).reshape(sh)

        for n, i in enumerate(insts):
            area = self.dataset[('calnv%s' % i).upper()][1]
            K = self.dataset[('calnv%s' % i).upper()][0]
            for m in measurements:
                raw = self.dataset['CORCON_nv_%s_%s' % (i, m)].ismatch(t)
                cons = self.dataset[('c%s%s' % (i, m)).upper()]
                # Calibrate to volts or current
                cal['%s%s' % (i, m)] = (cons[0]+cons[1]*raw)*cons[2]

            # Sensor power (in Watts); 1 Watt == 1 J s-1
            col_p = cal['%sicol' % i]*cal['%svcol' % i]  # V*I; collector
            ref_p = cal['%siref' % i]*cal['%svref' % i]  # V*I; reference

            if i.lower() == 'twc':
                no_cloud_mask = get_no_cloud_mask(col_p, wow_ind)
                FITTING_SUCCESS = False
            try:
                fitted_K, params = get_fitted_k(col_p, ref_p,
                                                ias, ps, no_cloud_mask, K)
                sys.stdout.write('Nevzorov %s baseline fitted ...\n   a_ias: %.2f\n   a_p: %.2f\n' % (i.upper(), params[0], params[1]))
                FITTING_SUCCESS = True
            except Exception, e:
                pass

            flag = np.zeros(sh, dtype=np.int8)
            flag[wow_ind != 0] = 3
            p = col_p-K*ref_p
            self.outputs[n].data = flagged_data(p/(tas*area*nvl),
                                                times.reshape(sh)[:, 0],
                                                flag)
            self.outputs[n+6].data = flagged_data(col_p,
                                                  times.reshape(sh)[:, 0],
                                                  flag)
            if FITTING_SUCCESS:
                p = col_p-fitted_K*ref_p
                self.outputs[n+3].data = flagged_data(p/(tas*area*nvl),
                                                      times.reshape(sh)[:, 0],
                                                      flag)
        # The last parameter to add is the reference power
        self.outputs[-1].data = flagged_data(ref_p,
                                             times.reshape(sh)[:, 0],
                                             flag)


class rio_nevzorov_1t1l2r(cal_base):
    """
    Processing module for the vane that has
      | 1x Total Water sensor
      | 1x Liquid Water sensors
      | 2x Reference sensors

    """

    def __init__(self, dataset):
        self.input_names = ['CORCON_nv_lwc_vcol',
                            'CORCON_nv_lwc_icol',
                            'CORCON_nv_lwc_vref',
                            'CORCON_nv_lwc_iref',
                            'CORCON_nv_twc_vcol',
                            'CORCON_nv_twc_icol',
                            'CORCON_nv_twc_vref',
                            'CORCON_nv_twc_iref',
                            'TAS',
                            'IAS_RVSM',
                            'PS_RVSM',
                            'WOW_IND',
                            'CLWCIREF', 'CLWCVREF', 'CLWCICOL', 'CLWCVCOL',
                            'CTWCIREF', 'CTWCVREF', 'CTWCICOL', 'CTWCVCOL',
                            'CALNVLWC',
                            'CALNVTWC',
                            'CALNVL']

        self.outputs = [parameter('NV_TWC_U',
                                  units='gram m-3',
                                  frequency=64,
                                  number=605,
                                  long_name='Uncorrected total condensed water content from the Nevzorov probe'),
                        parameter('NV_LWC_U',
                                  units='gram m-3',
                                  frequency=64,
                                  number=602,
                                  long_name='Uncorrected liquid water content from the Nevzorov probe',
                                  standard_name='mass_concentration_of_liquid_water_in_air'),
                        parameter('NV_TWC_C',
                                  units='gram m-3',
                                  frequency=64,
                                  number=609,
                                  long_name='Corrected total condensed water content from the Nevzorov probe'),
                        parameter('NV_LWC_C',
                                  units='gram m-3',
                                  frequency=64,
                                  number=608,
                                  long_name='Corrected liquid water content from the Nevzorov probe',
                                  standard_name='mass_concentration_of_liquid_water_in_air'),
                        parameter('NV_TWC_COL_P',
                                  units='W',
                                  frequency=64,
                                  long_name='TWC collector power'),
                        parameter('NV_TWC_REF_P',
                                  units='W',
                                  frequency=64,
                                  long_name='TWC reference power'),
                        parameter('NV_LWC_COL_P',
                                  units='W',
                                  frequency=64,
                                  long_name='LWC collector power'),
                        parameter('NV_LWC_REF_P',
                                  units='W',
                                  frequency=64,
                                  long_name='LWC reference power')]

        self.version = 1.00
        cal_base.__init__(self, dataset)

    def process(self):
        """
        """
        np.seterr(divide='ignore')  # suppress divide by zero messages
        t = self.dataset.matchtimes(self.input_names)
        insts = ['twc', 'lwc']
        measurements = ['icol', 'vcol', 'iref', 'vref']
        cal = {}
        times = self.dataset['CORCON_nv_lwc_vcol'].ismatch(t).times2d
        sh = times.shape
        nev_freq = self.dataset['CORCON_nv_lwc_vcol'].frequency
        nvl = self.dataset['CALNVL'][0]

        tas = self.dataset['TAS'].ismatch(t).interp(frequency=nev_freq)
        ias = self.dataset['IAS_RVSM'].ismatch(t).interp(frequency=nev_freq)
        ps = self.dataset['PS_RVSM'].ismatch(t).interp(frequency=nev_freq)
        wow_ind = self.dataset['WOW_IND'].ismatch(t)
        wow_ind.frequency = self.dataset['WOW_IND'].frequency
        wow_ind = 1*(wow_ind.interp(frequency=nev_freq) != 0)

        for n, i in enumerate(insts):
            # For each instrument (i)
            area = self.dataset[('calnv%s' % i).upper()][1]
            K = self.dataset[('calnv%s' % i).upper()][0]
            for m in measurements:
                raw = self.dataset['CORCON_nv_%s_%s' % (i, m)].ismatch(t)
                cons = self.dataset[('c%s%s' % (i, m)).upper()]
                # Calibrate to volts or current
                cal['%s%s' % (i, m)] = (cons[0]+cons[1]*raw)*cons[2]
            # Sensor power (W).
            col_p = cal['%sicol' % i]*cal['%svcol' % i]  # V*I; collector
            ref_p = cal['%siref' % i]*cal['%svref' % i]  # V*I; reference
            if i.lower() == 'twc':
                no_cloud_mask = get_no_cloud_mask(col_p, wow_ind)
            FITTING_SUCCESS = False
            try:
                fitted_K, params = get_fitted_k(col_p, ref_p,
                                                ias,
                                                ps,
                                                no_cloud_mask, K)
                sys.stdout.write('Nevzorov %s baseline fitted ...\n   a_ias: %.2f\n   a_p: %.2f\n' % (i.upper(), params[0], params[1]))
                FITTING_SUCCESS = True
            except Exception, e:
                pass
            flag = np.zeros(sh, dtype=np.int8)
            flag[wow_ind != 0] = 3
            p = col_p-K*ref_p
            # define the uncalibrated data
            self.outputs[n].data = flagged_data(p/(tas*area*nvl),
                                                times.reshape(sh)[:, 0],
                                                flag)
            # define the collector power
            self.outputs[n+4].data = flagged_data(col_p,
                                                  times.reshape(sh)[:, 0],
                                                  flag)
            # define the reference power
            self.outputs[n+5].data = flagged_data(ref_p,
                                                  times.reshape(sh)[:, 0],
                                                  flag)

            # define the calibrated water contents *if* the fitting
            # was successful
            if FITTING_SUCCESS:
                p = col_p-fitted_K*ref_p
                self.outputs[n+2].data = flagged_data(p/(tas*area*nvl),
                                                      times.reshape(sh)[:, 0],
                                                      flag)


class rio_nevzorov(cal_base):
    """
    Main Nevzorov processing modules, which calls the appropriate module
    depending on the vanetype that was fitted on the flight.

    The fitted vanetype is defined in the flight constant file with the
    constant name 'VANETYPE'.
    """

    def __init__(self, dataset):
        if not 'VANETYPE'in dataset.keys():
            dataset['VANETYPE'] = '1T1L2R'

        if dataset['VANETYPE'] == '1T1L2R':
            rio_nevzorov_1t1l2r.__init__(self, dataset)
        elif dataset['VANETYPE'] == '1T2L1R':
            rio_nevzorov_1t2l1r.__init__(self, dataset)
        else:
            pass

    def process(self):
        pass
