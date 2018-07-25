r"""
Code Overview:
==============

Computations for converting raw SEA WCM-2000 power data into liquid water
content, ice water content, and total water content. Included are
functions for calculating dry air offsets, element efficiencies, and
housekeeping checks.

Calculation process is as follows:
* Calculate total element power
  * In calc_sense_wc(), derived from raw voltage and currents
* Subtraction of dry power to obtain wet element power (1)
  * Read function to calc Pdry from Pcomp from flight constants file (?)
  * Calculate func for current flight with dryair_calc_comp() to compare
  * Calculate wet element power in calc_sense_wc()
* Calculation of measured water content for each element (2)
  * Using latent heats from energy_liq(), calculate in calc_sense_wc()
* Simultaneous calculation of actual IWC and LWC from measured values (3)
  * Done in same was as dry air power, read efficiencies calculated for
   many flights from flight constants file. Calculate for this flight to
   compared.
* Addition of IWC and LWC to obtain TWC

Additional calculations:
1. Determination of dry power from compensation element after calibration
2. Determination of evaporative temperature, latent heat, and specific heats
3. Various element efficiencies need to be calculated for this

Units:
======

Many of the parameters are obtained from empirical equations, thus the
use of the correct units is important. To use those equations and compare
values these functions use the units as used by SEA.

.. glossary::

   Electrical power
      W (or J/s)
   Element dimensions
      mm
   Latent heat
      cal/g
   Temperature
      degrees celsius
   True air speed
      m/s
   Water content
      g/m\ :sup:`3`


Variable Names:
===============

In an attempt to clarify the use of different symbols from different sources
here they all are;

.. csv-table:: Variables
   :header: "this file", "Korolev et al. [[KICS03]_", "SEA docs", "description"
   :widths: "auto","auto","auto"

   "iwc", :math:`W_{\scriptsize\text{liq}}`, "IWC", "Actual ice water content"
   "lwc", :math:`W_{\scriptsize\text{ice}}`, "LWC", "Actual liquid water content"
   "W_twc", :math:`W_{\scriptsize\text{TWC}}`, "TWCm", "Measured total water content"
   "W_lwc", :math:`W_{\scriptsize\text{LWC}}`, "LWCm", "Measured liquid water content"
   "k", :math:`k`, "", :math:`k = L^*_{\scriptsize\text{ice}} / L^*_{\scriptsize\text{liq}}`
   "e_liqL", :math:`\epsilon_{\scriptsize\text{liqL}}`, "d", "collection efficiency of the LWC sensor for liquid droplets"
   "e_liqT", :math:`\epsilon_{\scriptsize\text{liqT}}`, "b", "collection efficiency of the TWC sensor for liquid droplets"
   "e_iceT", :math:`\epsilon_{\scriptsize\text{iceT}}`, "", "collection efficiency of the TWC sensor for ice particles"
   "", :math:`k \epsilon_{\scriptsize\text{iceT}}`, "a", ""
   "beta_iceL", :math:`\beta`, "c", "collection efficiency of the LWC sensor for ice particles"

The SEA docs referred to is not the WCM-2000 manual but from the presentation
"Simultaneous Solution for IWC and LWC Using Two Elements with Differing LWC
and IWC Sensitivities" received from Lyle Lilie via email, 20/07/2017.

.. rubic:: References:
.. [SEA16] Science Engineering Associates, WCM-2000 Users Guide. January
25, 2016. http://www.scieng.com/pdf/WCM2000User.pdf
.. [KoSI98] Korolev et al., "The Nevzorov Airborne Hot-Wire LWC-TWC Probe:
Principle of Operation and Performance Characteristics", J. Atmos. Oceanic
Technol., 15, pp1495-1510, 1998.
.. [KICS03] Korolev et al., "Microphysical characterization of mixed-phase
clouds", Q.J.R. Meteorol. Soc., 129, pp39-65, 2003.
.. [Osbo39] Osborne, N.S., "Heat of fusion of ice. A revision", J. Res.
Natl. Bur. Stand., Vol. 23, p. 643, 1939.
.. [OsSG39] Osborne, Stimson, and Ginnings, "Measurements of heat capacity
and heat of vaporization of water in the range 0 degrees to 100 degrees C",
J. Res. Natl. Bur. Stand., Vol. 23, pp197-260, 1939.
.. [Woan00] Woan, G., "The Cambridge Handbook of Physics Formulas",
Cambridge University Press, 2000.
"""

import numpy as np
from copy import deepcopy
import sys

try:
    from ppodd.core import cal_base, flagged_data, timed_data, parameter
except:
    sys.stdout.write('PPODD not available ...\n')


# Conversion from calories to joules [Woan00]_.
cal_to_J = 4.1868
J_to_cal = 1./cal_to_J


### Placeholder function that will be from flight constants file or something
### similar. This function will be derived from dryair_calc_comp() for
### many flights.
Psense_dry_tmp = lambda P: P


def moving_avg(x, N):
    """
    Running average with a top-hat filter
    https://stackoverflow.com/questions/13728392/moving-average-or-running-mean
    """

    # Have used mode 'same' as N << x
    return np.convolve(x.ravel(), np.ones((N,))/N, mode='same')


def get_instr_fault_mask(el_temperature,
                         temp_limits=[100, 180],
                         verbose=True):
    """
    Detect and flagging of SEAPROBE instrument issues.

    The temperature measurements are checked to ensure they are in a
    defined range. Checks are done on a sample-by-sample basis and a mask
    returned, True indicating that a fault has occured.

    :param el_temperature: List of element temperature arrays. Any single
        element temperature array can be given or all three. It is assumed
        that each element has the same temperature limits. All arrays should
        have the same shape. Note that there is no error checking on the
        form of el_temperature so it must be correct!
    :type el_temperature: List of numpy arrays of floats (degC)
    :param temp_limits: Temperature min and max (degC). Default is [100,180].
    :type temp_limits: list of floats
    :param boolean verbose: If True then output percentage of data flagged.
        Default is True.
    :return: instrument mask
    :rtype: np.array of np.bool type

    .. todo::
        So far this routine only uses temperature to determine if it
        is working correctly. More failure conditions might be added in the
        future. Note that all inputs should have the same shape.
    .. todo::
        Add error checking for the inputs. Attempted np.atleast_2d but if
        inputs are different lengths this will not catch it.
    """

    # Note that all inputs are assumed to have the same shape
    el_shape = el_temperature[0].shape

    # Ensure order of min and max temperatures
    try:
        temp_limits.sort()
    except AttributeError:
        # Probably a tuple
        temp_limits = list(temp_limits[::])
        temp_limits.sort()

    # Mask _any_ values that are outside the allowed temperature range
    # ie there is no smoothing or windowing done
    temp_mask = np.any([np.logical_or(t_<temp_limits[0],
                                      t_>temp_limits[1]) for t_ in el_temperature],axis=0)

    ### Add any other instrument flagging in here

    instr_mask = temp_mask.copy()

    if verbose:
        if np.any(instr_mask) == True:
            perc_flagged = np.count_nonzero(instr_mask)/float(temp_mask.size) * 100.
        else:
            perc_flagged = 0.
        sys.stdout.write('Percentage of data flagged due to instrument ' +
                         'issues: {:.2f}% (n={:d})\n'.format(perc_flagged,
                                                             np.count_nonzero(instr_mask)))

    return np.reshape(instr_mask,el_shape)


def get_cloud_mask_from_el_temperature(el_temperature,
                                       var_thres=0.6,
                                       set_temp=140.,
                                       var_temp=1.,
                                       _buffer=3,
                                       freq=1., win=1.):
    """
    Create cloud mask by calculating TWC power variation in a rolling window.
    Create cloud mask from variation in element temperature in a rolling window.

    Function determines whether the measurement is taken inside or outside
    of cloud. It compares the variation (max-min) in temperature values
    within a time interval, win, to a given threshold value, var_thres.
    The variation inside a cloud is much larger than that outside of cloud.
    Additionally, if the element temperture strays too far from the set-point
    temperature it is assumed to be in cloud.

    The _buffer is used to extend the masked region in time. For example
    a _buffer value of three also masks the three seconds before *and* three
    seconds after each masked value as cloud.

    :param el_temperature: Element temperature (deg C).
    :type el_temperature: 2D-array of floats
    :key var_thres: Values varying by more than the threshold in the
        timespan given by `win` are flagged as cloud. Default is 0.6C.
    :type var_thres: float
    :param set_temp: Set-point temperature of element, default is 140C.
    :type set_temp: float
    :key var_temp: +/- variation around set_temp over which it is adjudged
        to be in cloud.
    :type var_temp: float
    :key _buffer: Time buffer around detected cloud event that is also
        masked. Default is 3s.
    :type _buffer: float
    :key freq: Frequency of input data (Hz). Default is 1Hz.
    :type freq: float
    :key win: Window length for range calculation (sec). Default is 1s.
    :type win: float

    :return cloud_mask: Boolean array; `True` equals cloud, `False`
        equals no cloud.
    """

    el_shape = el_temperature.shape

    # Convert from per second to per data sample
    win_s = int(np.rint(freq  *win))
    _buffer_s = int(np.rint(freq * _buffer))

    # Reshape inputs as necessary to determine trend in change
    if len(el_shape) > 1:
        el_temperature = el_temperature[::].ravel()

    # Create fancy indexer that provides a top-hat sliding window
    # https://stackoverflow.com/questions/15722324/sliding-window-in-numpy
    # The window has length win_s
    s = 1
    w = win_s
    # Step window across array by the window width
    #idx = np.arange(w)[None, :] + w*np.arange(el_power.size/w)[:, None]
    #var = np.ptp(el_power[idx],axis=1) >= var_thres
    #cloud_mask_ = np.broadcast_to(var, idx.T.shape)
    #cloud_mask = np.resize(cloud_mask_.transpose(),el_shape)

    # Step window across array by a single sample
    # Edge effects are dealt with by clipping. NOTE that this only works as
    # the peak-to-peak values are being calculated, mean etc will not work!
    idx = np.arange(w)[None, :] + s*np.arange(el_temperature.size/s)[:, None] - w/2
    _ = np.clip(idx,0,el_temperature.size-1,out=idx)

    # Determine range across each window and extreme values to create mask
    cloud_mask = np.logical_or(np.ptp(moving_avg(el_temperature,w/2)[idx],axis=1) >= var_thres,
                               np.min(moving_avg(el_temperature,w/2)[idx],axis=1) < set_temp-var_temp,
                               np.max(moving_avg(el_temperature,w/2)[idx],axis=1) > set_temp+var_temp)

    # Add any extra masking for _buffer
    # Note that if _buffer_s is even the actual added buffer shall be one sample shorter
    idx_buf = np.ravel([range(i_-_buffer_s/2,
                              1+i_+_buffer_s/2) for i_ in np.where(np.diff(cloud_mask))[0]])
    cloud_mask[np.clip(idx_buf,0,el_temperature.size-1,idx_buf)] = True

    return np.reshape(cloud_mask,el_shape)



    # get number of rows
    n = twc_temperature.shape[0]
    # init cloud mask
    cloud_mask = np.zeros((n,), dtype=np.bool)
    rng = np.max(twc_temperature, axis=1)-np.min(twc_temperature, axis=1)
    ix = np.where((rng > rng_threshold) | (np.min(twc_temperature, axis=1) < min_val) | (np.max(twc_temperature, axis=1) > max_val))[0]
    for i in range(_buffer*-1, _buffer+1):
        ix = list(set(list(np.concatenate((np.array(ix), np.array(ix)+i)))))
    # make sure that the indices do not exceed array dimensions
    ix = np.clip(ix, 0, n-1)
    if ix.size != 0:
        cloud_mask[ix] = True
    return cloud_mask


def get_cloud_mask_from_el_power(el_power, var_thres=0.45,
                                 _buffer=3,
                                 freq=1., win=1.):
    """
    Create cloud mask from the variation in element power in a rolling window.

    Function determines whether the measurement is taken inside or outside
    of cloud. It compares the variation (max-min) in power values within
    a time interval, win, to a given threshold value, var_thres. The
    variation inside a cloud is much larger than that outside of cloud.

    The _buffer is used to extend the masked region in time. For example
    a _buffer value of three also masks the three seconds before *and* three
    seconds after each masked value as cloud.

    :param el_power: element power, may be TWC, 083, or 021 (Watts).
    :type el_power: 2D-array of floats
    :key var_thres: Values varying by more than the threshold in the
        timespan given by `win` are flagged as cloud. Default is 0.45W.
    :type var_thres: float
    :key _buffer: Time buffer around detected cloud event that is also
        masked. Default is 3s.
    :type _buffer: float
    :key freq: Frequency of input data (Hz). Default is 1Hz.
    :type freq: float
    :key win: Window length for range calculation (sec). Default is 1s.
    :type win: float

    :return cloud_mask: Boolean array; `True` equals cloud, `False`
        equals no cloud.
    """

    el_shape = el_power.shape

    # Convert from per second to per data sample
    win_s = int(np.rint(freq  *win))
    _buffer_s = int(np.rint(freq * _buffer))

    # Reshape inputs as necessary to determine trend in change
    if len(el_shape) > 1:
        el_power = el_power[::].ravel()

    # Create fancy indexer that provides a top-hat sliding window
    # https://stackoverflow.com/questions/15722324/sliding-window-in-numpy
    # The window has length win_s
    s = 1
    w = win_s
    # Step window across array by the window width
    #idx = np.arange(w)[None, :] + w*np.arange(el_power.size/w)[:, None]
    #var = np.ptp(el_power[idx],axis=1) >= var_thres
    #cloud_mask_ = np.broadcast_to(var, idx.T.shape)
    #cloud_mask = np.resize(cloud_mask_.transpose(),el_shape)

    # Step window across array by a single sample
    # Edge effects are dealt with by clipping. NOTE that this only works as
    # the peak-to-peak values are being calculated, mean etc will not work!
    idx = np.arange(w)[None, :] + s*np.arange(el_power.size/s)[:, None] - w/2
    _ = np.clip(idx,0,el_power.size-1,out=idx)

    # Determine variation across each window and compare to given threshold
    cloud_mask = np.ptp(moving_avg(el_power,w/2)[idx],axis=1) >= var_thres

    # Add any extra masking for _buffer
    # Note that if _buffer_s is even the actual added buffer shall be one sample shorter
    idx_buf = np.ravel([range(i_-_buffer_s/2,
                              1+i_+_buffer_s/2) for i_ in np.where(np.diff(cloud_mask))[0]])
    cloud_mask[np.clip(idx_buf,0,el_power.size-1,idx_buf)] = True

    return np.reshape(cloud_mask,el_shape)


def get_cloud_mask(mask_func = get_cloud_mask_from_el_power,
                   mask_func_arg = [],
                   mask_func_kargs = {}):
    """
    Convenience function for accessing different cloud mask creation functions

    Cloud masks can be generated in several different ways. These methods may
    be besed entirely on the SEA data and are included in p_calc_sea, the
    default is to call p_calc_sea.get_cloud_mask_from_el_power(). Functions
    in external files may also be called, for example to base the cloud
    mask on the CDP data.

    :param mask_func: function call to calculate cloud mask. Default is
        p_calc_sea.get_cloud_mask_from_el_power().
    :type mask_func: function
    :param mask_func_arg: Any arguments for function.
    :type mask_func_arg: List of arguments
    :param mask_func_kargs: Any keyword arguments for function
    :type mask_func_kargs: Dictionary of keyword arguments

    :returns: Return value/s from function
    """

    return mask_func(*mask_func_arg,**mask_func_kargs)


def get_slr_mask(hdgr, altr, hdgr_atol=10., altr_atol=2, freq=1., win=1.):
    """
    Create a straight and level run mask based on aircraft flight conditions.

    Straight and level mask for aircraft based on rate of change of heading
    and altitude. Both these rates are arrays of floats with no timestep
    information, this means that the window length, win, and tolerances in
    the rates of change are per sample.

    The input arrays may be 1D, in which case a tophat running average of
    width win is used to smooth the data. If the input arrays have more
    dimensions then the first dimension is taken and no smoothing is done.
    Both input arrays must be the same shape and the returned mask has the
    same shape as the input arrays.

    :param hdgr: Rate of heading change (deg/sample).
    :type hdgr: float
    :param altr: Rate of altitude change (distance/sample). Distance
        units may be imperial or metric
    :type altr: float
    :key hdgr_atol: Absolute tolerance for determining acceptable rate
        of change of heading. Default is 10 deg/sec.
    :type hdgr_atol: float
    :key altr_atol: Absolute tolerance for determining acceptable rate
        of change of altitude (distance). Default is 2 distance/sec.
    :type altr_atol: float
    :key freq: Frequency of input data (Hz). Default is 1Hz.
    :type freq: float
    :key win: Window length for moving average (sec). Default is 1s.
    :type win: float

    :returns: slr_mask: boolean array. `True` means SLR, `False` means in
        climb or turn.

    .. note::

        This means that the masking is the inverse of cloud_mask. Change?
    """

    # Note that the input arrays should be the same size
    hdgr_shape = hdgr.shape
    altr_shape = altr.shape
    if hdgr_shape != altr_shape:
        print('Input array shape must be the same.')
        raise Exception

    # Convert from per second to per data sample
    win_s = int(np.rint(freq*win))
    hdgr_atol_s = np.divide(hdgr_atol,freq,dtype=float)
    altr_atol_s = np.divide(altr_atol,freq,dtype=float)

    # Reshape inputs as necessary to determine trend in change
    if len(hdgr.shape) == 1:
        # Reduce number of samples by window length after doing smoothing
        hdgr_ = moving_avg(hdgr,win_s)[::win_s]
    elif len(hdgr.shape) > 1:
        # Take only first dimension. No smoothing is done
#### TODO: Is it worth ravelling and then smoothing?
#### Have no smoothing at this stage for performance reasons
        hdgr_ = hdgr[::,0]

    if len(altr.shape) == 1:
        # Reshape array with length of smoothing window
        altr_ = moving_avg(altr,win_s)[::win_s]
    elif len(hdgr.shape) > 1:
        altr_ = altr[::,0]

    hdgr_mask = np.isclose(hdgr_, 0.0, atol=hdgr_atol_s, rtol=0)
    altr_mask = np.isclose(altr_, 0.0, atol=altr_atol_s, rtol=0)
    slr_mask_ = np.logical_and(hdgr_mask, altr_mask)
    slr_mask = np.broadcast_to(slr_mask_, (win_s,slr_mask_.size))

    return np.resize(slr_mask.transpose(),hdgr_shape)


def dryair_calc(Psense,T,ts,ps,tas,cloud_mask=None,verbose=True):
    """
    Calculate dry air power term by fitting constants for 1st principles.

    The calculation of the dry air power term is based on method three
    as described on page 58 of the WCM-2000 manual. This uses a fit
    between the theoretical and measured (in cloud-free conditions)
    sense powers to find the fitting constants K1 and K2.

    Psense,dry = k1 * (T - ts) * (ps * tas)**k2

    Psense,total = Psense,dry + Psense,wet
    =>
    Psense,total - Psense,dry = 0 in cloud-free conditions

    :param Psense: sense element power (W)
    :type Psense: float
    :param T: temperature of sense element (deg C)
    :type T: float
    :param ts: ambient static temperature (deg C)
    :type ts: float
    :param ps: ambient static air pressure (mbar)
    :type ps: float
    :param tas: true air speed (m/s)
    :type tas: float
    :param cloud_mask: Array of True/False or 1/0 for in/out of cloud
        Default is None for no cloud.
    :type cloud_mask: Boolean

    :returns result: Array of dry air power terms calculated for each
        value of Psense. Will return None if the fitting routine fails.

    """

    from scipy.optimize import curve_fit


    import pdb
    pdb.set_trace()

    if cloud_mask is None:
        n = Psense.shape[0]
        cloud_mask = np.zeros((n,), dtype=np.bool)

    _Psense = Psense[cloud_mask == 0, :].ravel()
    _T = T[cloud_mask == 0, :].ravel()
    _ts = ts[cloud_mask == 0, :].ravel()
    _ps = ps[cloud_mask == 0, :].ravel()
    _tas = tas[cloud_mask == 0, :].ravel()

    # remove all nans
    ix = np.where((np.isfinite(_Psense)) &
                  (np.isfinite(_T)) &
                  (np.isfinite(_ts)) &
                  (np.isfinite(_ps)) &
                  (np.isfinite(_tas)))[0]
    _Psense = _Psense[ix]
    _T = _T[ix]
    _ts = _ts[ix]
    _ps = _ps[ix]
    _tas = _tas[ix]

    # Linear fitting function
    func1 = lambda x,a,b: a * (_T - _ts) * (_ps * _tas)**b - x
    func2 = lambda a,b: a * (T - ts) * (ps * tas)**b

    # Find fitting constants that minimise difference between calculated
    # and actual total power on sense element.
    # Interpolations don't accept masked arrays so delete masked elements
    popt,pcov = curve_fit(func1, _Psense, np.zeros(_Psense.size))

    if np.any(np.isinf(pcov)):
        # No convergence
        return None

    if verbose:
        sys.stdout.write('K1: %8.3f   K2: %6.3f\n' % tuple(popt))

    result = func2(popt[0], popt[1])

    return result


def dryair_calc_comp(Psense,Pcomp,cloud_mask=None,
                     rtn_func=False,verbose=True):
    """
    Calculate dry air power term from compensation element measurements.

    The calculation of the dry air power term is based on the use of the
    compensation element as described on page 56 of the WCM-2000 manual.
    This finds the slope and offset for conversion of the compensation
    power to dry air sense element power.

    Psense,dry = P0 + K * Pcomp
    Psense,total = P0 + K * Pcomp when in clear air

    This will be TAS and Pambient dependent (possibly Tambient)

    Note that the Nevzerov calculation of the dry air term has been
    defined in a slightly different way and this same method may be applied
    to the SEA probe in future. eg p_nevzerov.get_fitted_k()

    :param Psense: Array of powers of sense element
    :type Psense: float
    :param Pcomp: Array of powers of compensation element for same
            times. len(Pcomp)==len(Psense)
    :type Pcomp: float
    :key cloud_mask: Array of True/False or 1/0 for in/out of cloud
            Default is None for no cloud.
    :type cloud mask: np.array
    :key rtn_func: If False [default] array of dry air powers is returned
        in same shape as inputs. If True then the lambda function to
        calculate the dry air power from Pcomp is returned.
    :type rtn_func: boolean
    :key verbose: If True [default] then print to stdout the fitting parameters
    :type verbose: boolean

    :returns: Array of dry air powers if rtn_func is False, if rtn_func is
        True then returns the fitting function.
    """
    from scipy.optimize import curve_fit

    # Linear fitting function
    func = lambda x,a,b: a*x + b

    # Create mask based on cloud_mask
    # This step is to cope with different types of binary elements
    # np.all statement so that cloud does not become False in make_mask step
    if cloud_mask is None or np.all(cloud_mask==False):
        cloud = np.array([False]*len(Pcomp))
    else:
        cloud = np.ma.make_mask(cloud_mask)

    # Remove any nan's from input arrays by wrapping up into cloud mask
    nan_mask = np.logical_or(~np.isfinite(Pcomp),~np.isfinite(Psense))
    cloud = np.logical_or(cloud[::],nan_mask)

    # Fit compensation power to sense power
    # Interpolations don't accept masked arrays so delete masked elements
    try:
        popt,pcov = curve_fit(func, Pcomp[~cloud], Psense[~cloud])
    except (TypeError, ValueError, RuntimeError):
        # Incompatible inputs or minimization failure
        pcov = np.inf

    if np.any(np.isinf(pcov)):
        # No convergence
        # TODO: This should RAISE an error instead?
        if verbose:
            sys.stdout.write('Dry air power fitting:\n Fitting failure')
        return None

    # Calculate standard deviation of fitting parameters
    perr = np.sqrt(np.diag(pcov))
    opt_err = np.dstack((popt,perr)).ravel()

    if verbose:
        sys.stdout.write('Dry air power fitting:\n'
            ' K: {:6.3f} (+/-{:0.3})\tP0: {:6.3f} (+/-{:0.3f})\n'.format(*opt_err))

    if rtn_func == True:
        return lambda x: func(x,*popt)
    else:
        return func(Pcomp,*popt)


def T_check(V,I,Tset,R100,dTdR,Twarn=None):
    """
    Compare calculated element temperature to setpoint temperature

    This function calculates the element resistance from the voltage and
    current readings and with the R100 and dT/dR calibration data converts
    this into element temperature. This is compared to the setpoint
    temperature.

    From WCM-2000 manual page 62.

    :param V: voltage across element (V)
    :type V: float or int
    :param I: current through element (I)
    :type I: float or int
    :param Tset: setpoint temperature (dec C)
    :type Tset: float or int
    :param R100: calibrated element resistance at 100deg C
    :type R100: float or int
    :param dTdR: calibrated element differential temp resistance ratio
    :type V: float or int
    :param Twarn: Temperature difference from setpoint at which to
            trigger a warning. Default is None and defined in function.
    :type Twarn: float or int

    :returns: Array of differences between setpoint and calculated temperature.
        Masked for differences greater than set warning level
    :rtype: float
    """

    # +/- temperature difference (deg C) at which to trigger warning
    if not Twarn:
        Twarn = 5

    # Calculate resistance. Make 64bit so can cope with I -> 0
    R = np.divide(V,I,dtype='f8')

    # Calculate element temperature and difference
    Tcalc = 100 + (R-R100) * dTdR
    Tdiff = Tcalc - Tset

    return np.ma.masked_outside(Tdiff, -abs(Twarn), abs(Twarn))


def energy_liq(ps):
    """
    Calculate the evaporative temperature and latent heat of evaporation.

    Based on WCM-2000 empirical equations on page 64. No references found.
    Valid for pressures 100-1050mb.

    :param ps: ambient static air pressure (mbar)
    :type ps: float
    :returns Tevap: Temperature of evaporation (deg C)
    :rtype Tevap: float
    :returns Levap: Latent heat of evaporation (cal/g)
    :rtype: float
    """

    # Ensure array so equations work correctly for all inputs
    ps = np.asarray(ps)

    # Calculate evaporative temperature of liquid water with pressure
    Tevap = 32.16 + \
           ( 0.1801 * ps) - \
            (2.391e-4 * ps**2.) + \
            (1.785e-7 * ps**3.) - \
            (5.19e-11 * ps**4.)

    # Calculate latent heat of evaporation (cal/g)
    Levap = 594.4 - \
            (0.484 * Tevap) - \
            (7.0e-4 * Tevap**2.)

    return Tevap, Levap


def glaciated():
    """
    Special case calculation of LWC and TWC for fully glaciated cloud.

    """
    # TODO:
    pass


def liquid():
    """
    Special case calculation of LWC and TWC for warm liquid cloud.

    """
    # TODO:
    pass


def calc_L(T,ps):
    """
    Calculate the specific energies for melting and/or evaporation

    The specific energy expended to evaporate water of a given temperature,
    T, is given by L*_l. The specific energy expended to melt then evaporate
    ice of a given temperature, T, is given by L*_i. The ratio of L*_i to
    L*_l is designated as k.

    This is described in Korolev 1998 and 2003. For the Nevzerov probe,
    a constant value is given but this includes efficiencies and temperatures
    specific to that probe. k for the SEA probe must be calculated.

    Variables names:
        L^*_liq     SpecEnergy_liq  Specific energy expended to evaporate
        C_liq       C_liq           Specific heat of liquid water
        T_e         T_e             Temperature of evaporation
        T           T               Ambient temperature
        L_liq(Te)   L_liq           Latent heat of evaporation at T_e
        L^*_ice     SpecEnergy_ice  Specific energy expended to melt+evaporate
        C_ice       C_ice           Specific heat of ice
        L_ice       L_ice           Latent heat of fusion

    .. note::

       1cal/gK == 4.184J/gK
       1J == 0.239 cal
       Specific heat (cal/gK) is same as heat capacity (J/molK)

    :param T: Ambient temperature (degree C)
    :type T:  float
    :param ps: Ambient static pressure (mb)
    :type ps: float

    :returns SpecEnergy_liq: The specific energy expended for liquid water (cal/g)
    :rtype: float
    :returns SpecEnergy_ice: The specific energy expended for ice (cal/g)
    :rtype: float
    """

    # Latent heat of fusion for ice (cal/g)
    # 333.5J/g == 79.71cal/g from Osborne, 1939.
    # Note that I don't understand the difference between International and
    # Absolute joules in these papers. Could be 79.72 as quoted by Wikipedia
    L_ice = 333.5 * J_to_cal

    # Specific heat of water from 0-100deg C converted to cal/gC
    # From Osborne et al. 1939.
    C_liq = lambda t: J_to_cal * (4.169828 \
                      + (0.000364 * (t+100)**5.26 )*1e-10 \
                      + 0.046709*10**(-0.036*t))

    # Specific heat of ice
    # from http://www.kayelaby.npl.co.uk/general_physics/2_3/2_3_6.html
    # Convert from J to cal
    # Have pinned to zero for values greater than zero
    C_ice_T = np.array([-196.,-100.,0.])
    C_ice_data = np.array([0.686,1.372,2.097]) * J_to_cal
    C_ice = lambda t: np.interp(t,C_ice_T,C_ice_data,right=0)

    # Obtain evaporation temperature and latent heat for ambient pressure
    T_e, L_liq = energy_liq(ps)

    # Calculate specific energy of liquid water
    # from Korolev et al. 2003. eq 5
    SpecEnergy_liq = C_liq(T_e-T) + L_liq

    # Equation divided into melting (up to 0C) and evaporation (0C -> T_e)
    # from Korolev et al. 2003. eq 6
    SpecEnergy_ice = C_ice(T) + L_ice + C_liq(T_e) + L_liq

    return SpecEnergy_liq, SpecEnergy_ice


def calc_sense_wc(Psense, Levap, Tevap, tat, tas, sens_dim):
    """
    Calculate the sense element-measured water content (either LWC or IWC).

    Use equation as given on pages 61,65 of the WCM-2000 manual. This is
    the same as eqs 3,4 from Korolev 2003 but for calories instead of joules.

    .. note::

       The same L*liq is used for both liquid and total water contents.
       This is fine as the difference between liquid and ice is accounted for
       by including the appropriate efficiencies in calc_lwc() and calc_iwc().

    :param float Psense: Sense element power (W)
    :param float Levap:
    :param float Tevap:
    :param float tat: True air temperature (degC)
    :param float tas: True air speed (m s-1)
    :param tuple sens_dim: (length, width) both in mm
    :returns W_meas: Water content (liquid or total) as measured by the
        sense element (g/m**3)
    :rtype: float
    """

    # Calculate wet power by subtracting dry air power term
    # TODO: Where Psense_dry_tmp() is the placeholder function to derive Psens_dry from Pcomp
    #Psense_wet = Psense_tot - Psense_dry_tmp(Pcomp)
    lwc = (Psense * 2.389*10**5) / ((Levap + (Tevap-tat))*tas*sens_dim[0]*sens_dim[1])
    return lwc


def calc_lwc(W_twc,W_lwc,k,e_liqL,e_liqT,beta_iceL,e_iceT):
    """
    Calculate liquid water content from the measured LWC and TWC.

    """

    lwc = np.divide(beta_iceL * W_twc - k*e_iceT * W_lwc,
                    beta_iceL * e_liqT - e_liqL * k*e_iceT)


def calc_iwc(W_twc,W_lwc,k,e_liqL,e_liqT,beta_iceL,e_iceT):
    """
    Calculate ice water content from the measured LWC and TWC

    """

    iwc = np.divide(e_liqL * W_twc - e_liqT * W_lwc,
                    e_liqL * k*e_iceT - beta_iceL * e_liqT)


def calc_wc():
    r"""
    Calculate real water contents including sensor efficiencies.

    The actual liquid water content, :math:`W_{\scriptsize\text{liq}}`, and
    ice water content, :math:`W_{\scriptsize\text{ice}}`, are derived from the
    measured liquid water content, :math:`W_{\scriptsize\text{LWC}}` and
    total water contents, :math:`W_{\scriptsize\text{TWC}}`. The element
    efficiencies are required for this process which are given by equations
    1 and 2 in [KICS03]_.

    .. math::

    W_{\scriptsize\text{TWC}} = \epsilon_{\scriptsize\text{liqT}} W_{\scriptsize\text{liq}}
        + k \epsilon_{\scriptsize\text{iceT}} W_{\scriptsize\text{ice}}

    W_{\scriptsize\text{LWC}} = \epsilon_{\scriptsize\text{liqL}} W_{\scriptsize\text{liq}}
        + \beta W_{\scriptsize\text{ice}}

    where :math:`\epsilon_{\scriptsize\text{liqT}}` and
    :math:`\epsilon_{\scriptsize\text{iceT}}` are the integrated collection
    efficiencies of the TWC sensor for liquid droplets and ice particles
    respectively. :math:`\epsilon_{\scriptsize\text{liqL}}` is the integrated
    collection efficiency of the LWC sensor/s to liquid droplets (this will)
    be size dependent but this is not dealt with here) while :math:`\beta` is
    the residual collection efficiency of the LWC sensor/s to ice particles.
    :math:`k = L^*_{\scriptsize\text{ice}} / L^*_{\scriptsize\text{liq}}` is
    the ratio of energies required to melt and evaporate ice to that required
    to evaporate water.

    Rearranging these equations gives the actual liquid and ice water contents
    that are dependent on both the liquid water and total water elements.

    .. math::

    W_{\scriptsize\text{liq}} = \frac{\beta W_{\scriptsize\text{TWC}} - k \epsilon_{\scriptsize\text{iceT}} W_{\scriptsize\text{LWC}}}
        {\beta \epsilon_{\scriptsize\text{liqT}} - \epsilon_{\scriptsize\text{liqL}} k \epsilon_{\scriptsize\text{iceT}}}

    W_{\scriptsize\text{ice}} = \frac{\epsilon_{\scriptsize\text{liqL}} W_{\scriptsize\text{TWC}} - \epsilon_{\scriptsize\text{liqT}} W_{\scriptsize\text{LWC}}}
        {\epsilon_{\scriptsize\text{liqL}} k \epsilon_{\scriptsize\text{iceT}} - \beta \epsilon_{\scriptsize\text{liqT}}}

    .. note:: Note that for the SEA WCM-2000 there are two LWC sensors, the 083 and 021.

    """

    pass


def find_efficiencies(W_twc,W_083,W_021,
                      e_liq=None,e_ice=None,
                      cloud_mask=None,liq_mask=None,ice_mask=None):
    """
    Find the sense element collection efficencies by fitting.


    :param W_twc: array of as-measured total water content from TWC element
            in g/m**3.
    :type W_twc: floats
    :param W_083: array of as-measured total water content from 083 LWC
            element in g/m**3.
    :type W_083: floats
    :param W_021: array of as-measured total water content from 021 LWC
            element in g/m**3.
    :type W_021: floats
    :param e_liq: record array of existing efficiencies for each sense element
            to liquid droplets. Used as a starting point for fit. Default
            is None with e_liq_default values used.
    :type e_liq: numpy record of floats
    :param e_ice: record array of existing efficiencies for each sense element
            to ice particles. Used as a starting point for fit. Default
            is None with e_ice_default values used.
    :type e_ice: numpy record of floats
    :param cloud_mask: Array of True/False or 1/0 for in/out of cloud
            Default is None for no cloud.
    :type cloud_mask: np.array
    :param liq_mask: Array of True/False or 1/0 for in/out of pure liquid cloud
            Default is None for no liquid cloud.
    :type liq_mask: np.array
    :param ice_mask: Array of True/False or 1/0 for in/out pure glaciated cloud
            Default is None for no ice cloud.
    :type ice_mask: np.array
    """
    from scipy.optimize import curve_fit

    # Define default element efficiencies as record arrays for each of the
    # three elements, TWC, 083, and 021
    # These were obtained from Lyle Lilie via email, 20/07/2017
    e_liq_default = np.array([(0.95,0.9,None)],
                     dtype={'names': ['TWC','083','021'],
                            'formats': ['f','f','f']})
    e_ice_default = np.array([(0.462,0.095,None)],
                     dtype={'names': ['TWC','083','021'],
                            'formats': ['f','f','f']})

    if e_liq is None:
        e_liq = deepcopy(e_liq_default)
    if e_ice is None:
        e_ice = deepcopy(e_ice_default)

    # Create masks based on cloud_mask, liq_mask, and ice_mask
    # This step is to cope with different types of binary elements
    if cloud_mask is None:
        cloud = np.array([False]*len(W_twc))
    else:
        cloud = np.ma.make_mask(cloud_mask)

    if liq_mask is None and ice_mask is None:
        liq = np.array([False]*len(W_twc))
        ice = np.array([False]*len(W_twc))
    elif liq_mask is None:
        # None of the cloud designated by cloud_mask is liquid
        # therefor make ice_mask same as cloud_mask
        liq = np.array([False]*len(W_twc))
        ice = cloud
    elif ice_mask is None:
        # None of the cloud designated by cloud_mask is glaciated
        # therefor make liq_mask same as cloud_mask
        ice = np.array([False]*len(W_twc))
        ice = cloud
    else:
        liq = np.ma.make_mask(liq_mask)
        ice = np.ma.make_mask(ice_mask)

    # Find efficiencies in glaciated clouds
    def ice_fit(Wvars,fitvars):
        """
        W_liq083 = W_liq021 = 0 therefore
        beta * W_twc = k * e_iceT*W_083 and beta*W_twc = k*e_iceT*W_021
        """
        (W_twc,W_083,W_021) = Wvars
        (e_liq083,e_liq021,e_liqT,beta_ice083,beta_ice021,e_iceT) = fitvars

        return [calc_lwc(W_twc,W_083,k,e_liq083,e_liqT,beta_ice083,e_iceT),
                calc_lwc(W_twc,W_21,k,e_liq021,e_liqT,beta_ice021,e_iceT)]


    popt,pcov = curve_fit(ice_fit,(W_twc[ice],W_083[ice],W_021[ice]),
                          np.zeros(len(W_twc[ice])),
                          p0=None,  #### <- this needs to be done
                          bounds=([0.1,0,0],[1,0.5,0.5])) # are all efficiencies

    #### NOTE: Efficiency bounds can be narrowed down.
    #### eg know that beta_iceL is very small
    #### and e_iceT, eliqT, and e_liqL are large.

    # Find efficiencies in liquid clouds
    # W_twc = 0 therefore
    # e_liq083 * W_twc = e_liqT * W_083 and e_liq021 * W_twc = e_liqT * W_021



class calc_sea(cal_base):
    """
    Class to calculate water content from the raw data supplied by p_read_sea.

    """

    def __init__(self, dataset):
        """
        :param dataset: ppodd.core.decades_dataset
        """

        self.input_names = ['WOW_IND',
                            'PS_RVSM',
                            'TAS_RVSM',
                            'TAT_DI_R',
                            'SEAPROBE_021',
                            'SEAPROBE_021_A',
                            'SEAPROBE_021_T',
                            'SEAPROBE_021_V',
                            'SEAPROBE_083',
                            'SEAPROBE_083_A',
                            'SEAPROBE_083_T',
                            'SEAPROBE_083_V',
                            'SEAPROBE_CMP',
                            'SEAPROBE_CMP_A',
                            'SEAPROBE_CMP_T',
                            'SEAPROBE_CMP_V',
                            'SEAPROBE_DCE',
                            'SEAPROBE_DCE_A',
                            'SEAPROBE_DCE_T',
                            'SEAPROBE_DCE_V',
                            'SEAPROBE_TWC',
                            'SEAPROBE_TWC_A',
                            'SEAPROBE_TWC_T',
                            'SEAPROBE_TWC_V',
                            'SEAPROBE_date',
                            'SEAPROBE_id',
                            'SEAPROBE_powerboxtemp',
                            'SEAPROBE_pstatic',
                            'SEAPROBE_tas',
                            'SEAPROBE_time',
                            'SEAPROBE_tstatic',
                            'SEAPROBE_zerostate']

        self.outputs = [parameter('SEA_TWC',
                                  units='WATER PER VOLUME',
                                  frequency=20,
                                  long_name='',
                                  standard_name=''),
                        parameter('SEA_LWC1',
                                  units='WATER PER VOLUME',
                                  frequency=20,
                                  long_name='From element 021',
                                  standard_name=''),
                        parameter('SEA_LWC2',
                                  units='WATER PER VOLUME',
                                  frequency=20,
                                  long_name='From element 083',
                                  standard_name='')]
        self.version = 1.00
        cal_base.__init__(self, dataset)

    def process(self):
        """
        We catch
        """
        # Pull the sensor element dimensions from the c0 message
        CALSEA083LENGTH = float(self.dataset['SEAPROBE_083_l'][0])
        CALSEA083WIDTH = float(self.dataset['SEAPROBE_083_w'][0])
        CALSEA021LENGTH = float(self.dataset['SEAPROBE_021_l'][0])
        CALSEA021WIDTH = float(self.dataset['SEAPROBE_021_w'][0])
        CALSEATWCLENGTH = float(self.dataset['SEAPROBE_TWC_l'][0])
        CALSEATWCWIDTH = float(self.dataset['SEAPROBE_TWC_w'][0])

        match = self.dataset.matchtimes(self.input_names)
        # The frequency of the SEAPROBE measurement can be set by the user
        # Therefore to cover different settings, we have to make sure that all
        # the data arrays have the same shape (equals frequency)
        freq = self.dataset['SEAPROBE_021'].shape[1]
        wow_ind = self.dataset['WOW_IND'].data.ismatch(match)
        tas = self.dataset['TAS_RVSM'].data.ismatch(match)
        tas = tas.interp(frequency=freq)
        ps = self.dataset['PS_RVSM'].data.ismatch(match)
        ps = ps.interp(frequency=freq)
        tat = self.dataset['TAT_DI_R'].data.ismatch(match)
        tat = tat.interp(frequency=freq)
        # The seaprobe functions work in degC
        tat -= 273.15

        sea_twc_a = self.dataset['SEAPROBE_TWC_A'].data.ismatch(match)
        sea_021_a = self.dataset['SEAPROBE_021_A'].data.ismatch(match)
        sea_083_a = self.dataset['SEAPROBE_083_A'].data.ismatch(match)
        sea_cmp_a = self.dataset['SEAPROBE_CMP_A'].data.ismatch(match)
        sea_dec_a = self.dataset['SEAPROBE_DCE_A'].data.ismatch(match)

        sea_twc_v = self.dataset['SEAPROBE_TWC_V'].data.ismatch(match)
        sea_021_v = self.dataset['SEAPROBE_021_V'].data.ismatch(match)
        sea_083_v = self.dataset['SEAPROBE_083_V'].data.ismatch(match)
        sea_cmp_v = self.dataset['SEAPROBE_CMP_V'].data.ismatch(match)
        sea_dec_v = self.dataset['SEAPROBE_DCE_V'].data.ismatch(match)

        sea_twc_p_sense_total = sea_twc_a*sea_twc_v
        sea_021_p_sense_total = sea_021_a*sea_021_v
        sea_083_p_sense_total = sea_083_a*sea_083_v
        sea_cmp_p_sense_total = sea_cmp_a*sea_cmp_v
        sea_dec_p_sense_total = sea_dec_a*sea_dec_v

        sea_twc_t = self.dataset['SEAPROBE_TWC_T'].data.ismatch(match)
        sea_021_t = self.dataset['SEAPROBE_021_T'].data.ismatch(match)
        sea_083_t = self.dataset['SEAPROBE_083_T'].data.ismatch(match)
        sea_cmp_t = self.dataset['SEAPROBE_CMP_T'].data.ismatch(match)
        sea_dec_t = self.dataset['SEAPROBE_DCE_T'].data.ismatch(match)

        cloud_mask = get_cloud_mask_from_twc_temperature(sea_twc_p_sense_total)

        sea_twc_p_sense_dry = dryair_calc(sea_twc_p_sense_total, tat,
                                          sea_twc_t, ps, tas,
                                          cloud_mask=cloud_mask)
        sea_021_p_sense_dry = dryair_calc(sea_021_p_sense_total, tat,
                                          sea_021_t, ps, tas,
                                          cloud_mask=cloud_mask)
        sea_083_p_sense_dry = dryair_calc(sea_083_p_sense_total, tat,
                                          sea_083_t, ps, tas,
                                          cloud_mask=cloud_mask)

        sea_twc_p_sense_wet = sea_twc_p_sense_total-sea_twc_p_sense_dry
        sea_021_p_sense_wet = sea_021_p_sense_total-sea_021_p_sense_dry
        sea_083_p_sense_wet = sea_083_p_sense_total-sea_083_p_sense_dry

        Tevap, Levap = energy_liq(ps)

        def calc_water_content(wet_power, Levap, Tevapt, tat, tas, element_length, element_width):
            result = (wet_power*2.389E5)/((Levap+1.0*(Tevap-tat))*tas*element_length*element_width)

        twc = calc_water_content(sea_twc_p_sense_wet, Levap, Tevap,
                                 tat, tas,
                                 CALSEATWCLENGTH, CALSEATWCWIDTH)
        lwc_021 = calc_water_content(sea_021_p_sense_wet, Levap, Tevap,
                                     tat, tas,
                                     CALSEA021LENGTH, CALSEA021WIDTH)
        lwc_083 = calc_water_content(sea_083_p_sense_wet, Levap, Tevap,
                                     tat, tas,
                                     CALSEA083LENGTH, CALSEA083WIDTH)

        # TODO: More flagging needs to be done
        flag = np.zeros(twc.shape, dtype=np.int8)

        flag[wow_ind != 0, :] = 3  # flag everything `3` when ARA on ground

        self.outputs[0].data = flagged_data(twc, match, flag)
        self.outputs[1].data = flagged_data(lwc_021, match, flag)
        self.outputs[2].data = flagged_data(lwc_083, match, flag)
