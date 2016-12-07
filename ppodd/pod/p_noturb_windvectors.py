from ppodd.core import *

import numpy as np


def add_hdg_offset(hdg, hdg_offset):
    """function takes care of angle calculations and substracts 360 if
    hdg is greater than 360 and adds 360 if angle is smaller than 0.
    """
    result = np.array(hdg) + hdg_offset
    result[np.where(result > 360.)] += -360.
    result[np.where(result < 0.)] += 360.
    return result


def correct_tas_rvsm(tas_rvsm, dit, tas_scale_factor=None):
    """
    Correcting true airspeed measurement for temperature effect.
    dit is the deiced temperature measurements.

    see: http://en.wikipedia.org/wiki/Airspeed
    :param tas_rvsm: True Air Speed from the flight computer
    :param dit: deiced temperature
    :param tas_scale_factor: Correction factor for the true air speed

    """
    if not tas_scale_factor:
        tas_scale_factor = 0.9984
    mach = tas_rvsm / (661.4788 * 0.514444) / np.sqrt(dit/288.15)
    delta_tas = 4.0739 - (32.1681 * mach) + (52.7136 * (mach**2))
    tas = (tas_rvsm - delta_tas) * tas_scale_factor
    return tas


def calc_noturb_wspd(tas_rvsm, hdg_gin, gspd_north, gspd_east, dit, hdg_offset=None, tas_scale_factor=None):

    """
    Calculates u and v as the aircraft does it

    see: http://delphiforfun.org/programs/Math_Topics/WindTriangle.htm
         http://www.pilotfriend.com/training/flight_training/nav/calcs.htm

    """
    if hdg_offset:
        hdg = add_hdg_offset(hdg_gin, hdg_offset)
    else:
        hdg = hdg_gin
    if not tas_scale_factor:
        tas_scale_factor = 0.9984

    #adjust tas_rvsm for temperature effects
    tas = correct_tas_rvsm(tas_rvsm, dit, tas_scale_factor=tas_scale_factor)
    air_spd_east = np.cos(np.deg2rad(hdg - 90.)) * tas
    air_spd_north = np.sin(np.deg2rad(hdg - 90.)) * tas
    u = gspd_east - air_spd_east
    v = air_spd_north + gspd_north
    return (u, v)


def calc_noturb_flag(tas_rvsm, hdg_gin, veln_gin, vele_gin, tat_di_r, roll_gin, roll_threshold=None):
    """
    uses the flags for the variables that are used to calculate the
    noturb winds. noturb wind speeds are invalid in turns. This is taken care of
    by flagging those values using a roll_threshold (default=1.5)

    """
    if not roll_threshold:
        roll_threshold = 1.5
    tas_rvsm_flag = np.max(tas_rvsm.flag, axis=1)
    hdg_gin_flag = np.max(hdg_gin.flag, axis=1)
    gspd_north_flag = np.max(veln_gin.flag, axis=1)
    gspd_east_flag = np.max(vele_gin.flag, axis=1)
    dit_flag = np.max(tat_di_r.flag, axis=1)

    #flag all data points 3, that exceed roll_threshold
    roll_flag = np.where(np.abs(roll_gin) < roll_threshold,
                         roll_gin*0,
                         roll_gin*0+3)

    flag_data = np.column_stack((tas_rvsm_flag,
                                 hdg_gin_flag,
                                 gspd_north_flag,
                                 gspd_east_flag,
                                 dit_flag,
                                 roll_flag))
    flag_data = np.max(flag_data, axis=1)
    flag_data[flag_data < 0] = 3
    flag_data[flag_data > 3] = 3
    return flag_data



class noturb_windvectors(cal_base):
    """
    Calculation of windvectors that do not rely on the turbulence probe in the radom of the aircraft. The data are
    espially useful in icing conditions, when the 

:Input:
  | VELE_GIN
  | VELN_GIN
  | HDG_GIN
  | TAT_DI_R
  | TAS_RVSM
  | ROLL_GIN


    tas_scale_factor = 0.9984
    
:Flagging:
  The flag is inherited from the input data. The flag is the worst from all the Input
  variables. In addtion to this all a roll angle threshold is used, which is set by default
  to 1.5 degrees. All values with a absolute greater roll value than this is flagged "3".


     
    """
    def __init__(self,dataset):
        #print('   *** ADD NOTURB WINDVECTORS - INIT ***')
        self.input_names=['VELE_GIN', 'VELN_GIN', 'HDG_GIN', 'TAT_DI_R', 'TAS_RVSM', 'ROLL_GIN']
        self.outputs=[parameter('U_NOTURB',
                                units='m s-1',
                                frequency=1,
                                long_name='Eastward wind component derived from aircraft instruments and GIN',
                                standard_name='eastward_wind'),
                      parameter('V_NOTURB',
                                units='m s-1',
                                frequency=1,
                                long_name='Northward wind component derived from aircraft instruments and GIN',
                                standard_name='northward_wind')]
        self.version=1.00
        cal_base.__init__(self,dataset)

    def process(self):
        #TODO: move the two calibration coefficients to the flight-cst file
        hdg_offset = 0.35
        tas_scale_factor = 0.9984
        d=self.dataset
        match = d.matchtimes(['TAS_RVSM', 'HDG_GIN', 'VELN_GIN', 'VELE_GIN', 'TAT_DI_R'])
        tas_rvsm = d['TAS_RVSM'].data.ismatch(match)
        hdg_gin = d['HDG_GIN'].data.ismatch(match)
        veln_gin = d['VELN_GIN'].data.ismatch(match)
        vele_gin = d['VELE_GIN'].data.ismatch(match)
        tat_di_r = d['TAT_DI_R'].data.ismatch(match)
        roll_gin = d['ROLL_GIN'].data.ismatch(match)
        u_noturb_data, v_noturb_data = calc_noturb_wspd(tas_rvsm,
                                                        hdg_gin,
                                                        veln_gin,
                                                        vele_gin,
                                                        tat_di_r,
                                                        hdg_offset=hdg_offset,
                                                        tas_scale_factor=tas_scale_factor)
        flag_data = calc_noturb_flag(tas_rvsm, hdg_gin, veln_gin, vele_gin, tat_di_r, roll_gin)
        u_noturb_data = np.mean(u_noturb_data, axis=1)
        v_noturb_data = np.mean(v_noturb_data, axis=1)
        u_noturb=flagged_data(u_noturb_data, tas_rvsm.times, flag_data)
        v_noturb=flagged_data(v_noturb_data, tas_rvsm.times, flag_data)
        self.outputs[0].data=u_noturb
        self.outputs[1].data=v_noturb
