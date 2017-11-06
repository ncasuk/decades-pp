"""
Computations for converting raw SEA WCM-2000 power data into liquid water
content, ice water content, and total water content. Included are
functions for calculating dry air offsets, element efficiencies, and
housekeeping checks.

Calculation process is as follows;
- Calculate total element power
- Subtraction of dry power to obtain wet element power (1)
- Calculation of measured water content for each element (2)
- Simultaneous calculation of actual IWC and LWC from measured values (3)
- Addition of IWC and LWC to obtain TWC

Additional calculations;
(1) Determination of dry power from compensation element after calibration
(2) Determination of evaporative temperature, latent heat, and specific heats
(3) Various element efficiencies need to be calculated for this

Units:
Many of the parameters are obtained from empirical equations, thus the
use of the correct units is important. To use those equations and compare
values these functions use the units as used by SEA.

Water content:      g/m**3
Electrical power:   W (or J/s)
Temperature:        degrees celsius
True air speed:     m/s
Element dimensions: mm
Latent heat:        cal/g

References:
Science Engineering Associates, WCM-2000 Manual. January 25, 2016.
Korolev et al., "Microphysical characterization of mixed-phase clouds",
    Q.J.R. Meteorol. Soc., 129, pp39-65, 2003.
Korolev et al., "The Nevzorov Airborne Hot-Wire LWC–TWC Probe: Principle
    of Operation and Performance Characteristics", J. Atmos. Oceanic
    Technol., 15, pp1495-1510, 1998.
Osborne, N.S., "Heat of fusion of ice. A revision", J. Res. Natl. Bur.
    Stand., Vol. 23, p. 643, 1939.
Osborne, Stimson, and Ginnings, "Measurements of heat capacity and heat
    of vaporization of water in the range 0 degrees to 100 degrees C",
    J. Res. Natl. Bur. Stand., Vol. 23, pp197-260, 1939.
"""



#import ppodd
from ppodd.core import cal_base, flagged_data, timed_data, parameter

import numpy as np


# Conversion from calories to joules
# ref: Woan, G., "The Cambridge Handbook of Physics Formulas", Cambridge University Press, 2000.
cal_to_J = 4.1868
J_to_cal = 1./cal_to_J



def dryair_cal(Psense,T,ts,ps,tas,cloud_mask=None):
    """
    Find dry air power fitting constants for 1st principles calculation.

    The calculation of the dry air power term is based on method three
    as described on page 58 of the WCM-2000 manual. This uses a fit
    between the theoretical and measured (in cloud-free conditions)
    powers to find the fitting constants K1 and K2.

    Psense,dry = k1 * (T - ts) * (ps * tas)**k2


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

    :returns k1: fitting coefficient
    :rtype k1: float
    :returns k2: fitting coefficient
    :rtype k2: float
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

    :param Psense: Array of powers of sense element
    :type Psense: float
    :param Pcomp: Array of powers of compensation element for same
            times. len(Pcomp)==len(Psense)
    :type Pcomp: float
    :param cloud_mask: Array of True/False or 1/0 for in/out of cloud
            Default is None for no cloud.
    :type cloud mask: np.array

    :returns: Function for calculating dry air power of sense element from Pcomp
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
    if Twarn is None: Twarn = 5

    # Calculate resistance. Make 64bit so can cope with I -> 0
    R = np.divide(V,I,dtype='f8')

    # Calculate element temperature and difference
    Tcalc = 100 + (R-R100) * dTdR
    Tdiff = Tcalc - Tset

    return np.ma.masked_outside(Tdiff,-abs(Twarn),abs(Twarn))


def energy_liq(ps):
    """
    Calculate the evaporative temperature and latent heat of evaporation

    Based on WCM-2000 empirical equations on page 64. No references found.
    Valid for pressures 100-1050mb.

    :param ps: ambient static air pressure (mbar)
    :type ps: float:
    :returns Tevap: Temperature of evaporation (deg C)
    :rtype Tevap: float

    :returns Levap: Latent heat of evaporation (cal/g)
    :rtype: float
    """

    # Ensure array so equations work correctly for all inputs
    ps = np.asarray(ps)

    # Calculate evaporative temperature of liquid water with pressure
    Tevap = 32.16 + \
            0.1801 * ps - \
            2.391e-4 * ps**2. + \
            1.785e-7 * ps**3. - \
            5.19e-11 * ps**4.

    # Calculate latent heat of evaporation (cal/g)
    Levap = 594.4 - \
            0.484 * Tevap - \
            7.0e-4 * Tevap**2.

    return Tevap, Levap


def glaciated():
    """
    Special case calculation of LWC and TWC for fully glaciated cloud.

    """

def liquid():
    """
    Special case calculation of LWC and TWC for warm liquid cloud.

    """

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

    Notes:  1cal/gK == 4.184J/gK
            1J == 0.239 cal
            Specific heat (cal/gK) is same as heat capacity (J/molK)

    :param T: Ambient temperature (degree C)
    :type T:  float
    :param ps: Ambient static pressure (mb)
    :type  ps: float

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


def calc_el_wc():
    """
    Calculate the element-measured water content.



    """


    Pel / (tas * el_w * el_l * L)


def calc_lwc():
    """
    Calculate liquid water content based on method of SEA WCM-2000 manual

    The element efficiencies are defined by SEA and Korolev 2003 as follows;
    k*e_iceT: collection efficiency of TWC element to ice
        where k is the ratio of expended specific energy for sublimation to evaporation
    b, epsilon_liqT: collection efficiency of TWC element to liquid
    c, beta: collection efficiency of LWC element (083 or 021) to ice
    d, epsilon_liqL: collection efficiency of LWC element (083 or 021) to liquid

    ref: Korolev et al., "Microphysical characterization of mixed-phase
    clouds", Q.J.R. Meteorol. Soc., 129, pp39-65, 2003.

    """


    lwc = np.divide(beta_iceL * W_twc - k*e_iceT * W_lwc,
                    beta_iceL * e_liqT - e_liqL * k*e_iceT)




def calc_iwc():
    """
    Calculate ice water content

    """

    iwc = np.divide(e_liqL * W_twc - e_liqT * W_lwc,
                    e_liqL * k*e_iceT - beta_lwc * e_liqT)



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

    """

    def __init__(self, dataset):
        """
        :param dataset: ppodd.core.decades_dataset
        """
        self.input_names = ['WOW_IND',
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
                                  frequency=1,
                                  long_name='',
                                  standard_name='')]
        self.version = 1.00
        cal_base.__init__(self, dataset)

    def process(self):
        match = self.dataset.matchtimes(self.input_names)
        wow_ind = self.dataset['WOW_IND'].data.ismatch(match)
        # TODO: Not working yet, because of wrong shape of data array
        sea_twc_data = self.dataset['SEAPROBE_TWC_A'].data.ismatch(match)
        flag = np.zeros(sea_twc_data.shape, dtype=np.uint8)
        ix = np.where(wow_ind != 0)[0]
        flag[ix,:] = 3

        sea_twc = flagged_data(sea_twc_data, match, flag)
        self.outputs[0] = seaprobe_twc

