
#import ppodd
#from ppodd.core import *

import numpy as np


def dryair_cal(Psense,T,ts,ps,tas,cloud_mask=None):
    """
    Find dry air power fitting constants for 1st principles calculation.

    The calculation of the dry air power term is based on method three
    as described on page 58 of the WCM-2000 manual. This uses a fit
    between the theoretical and measured (in cloud-free conditions)
    powers to find the fitting constants K1 and K2.

    Psense,dry = k1 * (T - ts) * (ps * tas)**k2

    Args:
        Psense (float): sense element power (W)
        T (float): temperature of sense element (deg C)
        ts (float): ambient static temperature (deg C)
        ps (float): ambient static air pressure (mbar)
        tas (float): true air speed (m/s)
        cloud_mask (boolean): Array of True/False or 1/0 for in/out of cloud
            Default is None for no cloud.

    Returns:
        k1,k2 coefficients
    """

    from scipy.optimize import curve_fit

    # Linear fitting function
    func = lambda x,a,b: a * (T - ts) * (ps * tas)**b - x

    # Create mask based on cloud_mask
    # This step is to cope with different types of binary elements
    if cloud_mask is None:
        cloud = np.array([False]*len(Psense))
    else:
        cloud=np.ma.make_mask(cloud_mask)

    # Find fitting constants that minimise difference between calculated
    # and actual total power on sense element.
    # Interpolations don't accept masked arrays so delete masked elements
    (k1,k2),pcov = curve_fit(func,Psense[~cloud],np.zeros(len(Psense[~cloud])))

    return k1,k2


def dryair_cal_comp(Psense,Pcomp,cloud_mask=None):
    """
    Find dry air power term from compensation element measurements

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

    Args:
        Psense (float): Array of powers of sense element
        Pcomp (float): Array of powers of compensation element for same
            times. len(Pcomp)==len(Psense)
        cloud_mask (array): Array of True/False or 1/0 for in/out of cloud
            Default is None for no cloud.

    Returns:
        Function for calculating dry air power of sense element from Pcomp
    """
    from scipy.optimize import curve_fit

    # Linear fitting function
    func = lambda x,a,b: a*x + b

    # Create mask based on cloud_mask
    # This step is to cope with different types of binary elements
    if cloud_mask is None:
        cloud = np.array([False]*len(Pcomp))
    else:
        cloud=np.ma.make_mask(cloud_mask)

    # Fit compensation power to sense power
    # Interpolations don't accept masked arrays so delete masked elements
    popt,pcov = curve_fit(func, Pcomp[~cloud], Psense[~cloud])

    return lambda x: func(x,*popt)


def T_check(V,I,Tset,R100,dTdR,Twarn=None):
    """
    Compare calculated element temperature to setpoint temperature

    This function calculates the element resistance from the voltage and
    current readings and with the R100 and dT/dR calibration data converts
    this into element temperature. This is compared to the setpoint
    temperature.

    From WCM-2000 manual page 62.

    Args:
        V (float): voltage across element (V)
        I (float): current through element (I)
        Tset (float): setpoint temperature (dec C)
        R100 (float): calibrated element resistance at 100deg C
        dTdR (float): calibrated element differential temp resistance ratio
        Twarn (float): Temperature difference from setpoint at which to
            trigger a warning. Default is None and defined in funtion.

    Returns:
        Array of differences between setpoint and calculated temperature.
        Masked for differences greater than set warning level
    """

    # +/- temperature difference (deg C) at which to trigger warning
    if Twarn is None: Twarn = 5

    # Calculate resistance. Make 64bit so can cope with I -> 0
    R = np.divide(V,I,dtype='f8')

    # Calculate element temperature and difference
    Tcalc = 100 + (R-R100) * dTdR
    Tdiff = Tcalc - Tset

    return np.ma.masked_outside(Tdiff,-abs(Twarn),abs(Twarn))


def latent_heat(ps):
    """
    Calculate latent heat of evaporation

    Based on WCM-2000 empirical equations on page 64. No references given.
    Valid for pressures 100-1050mb

    Args:
        ps (float): ambient static air pressure (mbar)

    Returns:
        Float of latent heat of evaporation (cal/gm)
    """

    # Calculate evaporative temperature of liquid water with pressure
    Tevap = 32.16 + \
            0.1801 * ps - \
            2.391e-4 * ps**2. + \
            1.785e-7 * ps**3. - \
            5.19e-11 * ps**4.

    # Calculate latent heat of evaporation (cal/gm)
    Levap = 594.4 - \
            0.484 * Tevap - \
            7.0e-4 * Tevap**2.

    return Levap


def glaciated():
    """
    Special case calculation of LWC and TWC for fully glaciated cloud.

    """

def liquid():
    """
    Special case calculation of LWC and TWC for warm liquid cloud.

    """

def calc_lwc():
    """
    Calculate liquid water content based on method of SEA WCM-2000 manual

    Args:
        tas (float): true air speed (m/s)
        ts (float): ambient static air temperature (deg C)
        ps (float): ambient static air pressure (mbar)

    """


def calc_twc():
    """
    Calculate total water content

    """

def calc_combi(e_liq,e_ice):
    """
    Calculate TWC and LWC from combined TWC and LWC element.

    The method uses both the TWC element and LWC element/s simultaneously
    to calculate the TWC and LWC. This is not described in the SEA manual
    (is described by L Lilie in email, 20170720) but is used by Korolev etc.

    The element efficiencies are defined by SEA and Korolev 2003 as follows;
    a, k*epsilon_iceT: collection efficiency of TWC element to ice
        where k is the ratio of expended specific energy for sublimation to evaporation
    b, epsilon_liqT: collection efficiency of TWC element to liquid
    c, beta: collection efficiency of LWC element (083 or 021) to ice
    d, epsilon_liqL: collection efficiency of LWC element (083 or 021) to liquid

    ref: Korolev et al., "Microphysical charasterization of mixed-phase
    clouds", Q.J.R. Meteorol. Soc., 129, pp39-65, 2003.


    Args:
        dataset (?)
    """

    # Define default element efficiencies as record arrays for each of the
    # three elements, TWC, 083, and 021
    # These were obtained from Lyle Lilie via email, 20/07/2017
    e_liq = np.array([(0.95,0.9,None)],
                     dtype={'names': ['TWC','083','021'],
                            'formats': ['f','f','f']})
    e_ice = np.array([(0.462,0.095,None)],
                     dtype={'names': ['TWC','083','021'],
                            'formats': ['f','f','f']})



class calc_sea(cal_base):
    """
    Class to calculate water content from the raw data supplied by p_read_sea.



    def __init__(self,dataset):
        """


        """