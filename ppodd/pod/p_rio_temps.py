from ppodd.core import parameter, fort_cal
import numpy as np


class rio_temps(fort_cal):
    """
FORTRAN routine C_TEMPS2

:ROUTINE:
  C_TEMPS2 SUBROUTINE FORTVAX

:PURPOSE:
  Produces calibrated deiced and non-deiced temperatures

:DESCRIPTION:
  Calculates indicated and true air temperatures in K for the
  Deiced and Non-Deiced temperature sensors as follows:

  | 519 - Indicated Air Temperature  from Deiced     [K] at 32Hz
  | 520 - True Air Temperature       from Deiced     [K] at 32Hz
  | 524 - Indicated Air Temperature  from Non-deiced [K] at 32Hz
  | 525 - True Air Temperature       from Non-deiced [K] at 32Hz


  Note that this module only processes data recorded on the
  146 which only uses one parameter per temperature.

  The Deiced Temperature is recorded on the DRS at 32Hz as
  parameter 10 and the Non-deiced Temperature is recorded on
  the DRS as parameter 23.

  Indicated Air Temperature is derived by application of
  the appropriate second order calibration coefficients to the
  raw data.

  A correction for heating from the deicing heater is made to
  the deiced indicated air temperature if the heater is
  switched on, as indicated by bit 5 of the signal register
  (parameter 27) being clear.  This heating correction is
  obtained from graphs of Temperature vs Machno in Rosemount
  Technical Reports 7597 & 7637.  If Machno is less than
  0.1 the data is flagged 1, because the Rosemount graph is
  invalid below 0.1, and if Machno below 0.05, a value of 0.05
  is use to ensure a valid logarithm.  The algorithm used for
  heating correction is::

    (exp(exp(1.171+(log(Machno)+2.738)*(-0.000568*(s+q)-0.452))))*0.1
    where: s=static pressure       [mbs]
           q=pitot static pressure [mbs]

  True Air Temperature is derived as::

    TAT[K] = (Indicated Air Temperature[K]) /
                         (1.0 +(0.2 * MACHNO * MACHNO * TRECFTR))
    where: MACHNO  is computed by scientific subroutine S_MACH.
           TRECFTR is the Temperature recovery factor - used to
             compensate for effects of kinetic heating.
             This is supplied as a constant from the
             flight constants file to this routine.


    It can be calculated from flight results of slow/fast runs as::

      (Tindfast-Tindslow)/(Ffast*Tindslow-Fslow*Tindfast)
        where: Tind = indicated temperature [K]
               F    = 0.2 * Machno * Machno

:FLAGGING:
  Both deiced and non-deiced temperature calculations follow
  a similar scheme for error flagging, with worst case flags
  being propagated through the calculations.  Sources of error
  flags are:

    | Absence of calibration constants    - flag 3
    | Absence of recovery factor constant - flag 3
    | Static pressure errors              - Parameter 576 flag
    | Pitot pressure errors               - Parameter 577 flag
    | Max/min/rate of change errors       - flag 2
    | Mach No less than 0.1               - flag 1

  Not all the above errors need affect all measurements. For
  instance pressure errors will not affect Indicated Air
  Temperatures, unless the deicing heater is on. Note that
  this module cannot be called if any of the raw (not derived)
  parameters are missing.  Also note that no raw data on which
  this module can be used will be carrying flags (only raw
  data transcribed on the Gould computer can carry flags). If
  any temperature has a flag of three, its value is set to
  0.0 K (and flagged with a three).

 :VERSION:
   1.00  10/09/92  W.D.N.JACKSON

:ARGUMENTS:
  :Constants:
    | RCONST(1)   Recovery factor for Deiced sensor
    | RCONST(2)   Recovery factor for Non-deiced sensor
    | RCONST(3)   Deiced X0 calibration constant (deg C)
    | RCONST(4)   Deiced X1 calibration constant (deg C)
    | RCONST(5)   Deiced X2 calibration constant (deg C)
    | RCONST(6)   Non-deiced X0 calibration constant (deg C)
    | RCONST(7)   Non-deiced X1 calibration constant (deg C)
    | RCONST(8)   Non-deiced X2 calibration constant (deg C)

  :Inputs:
    | DEICED TEMPERATURE            [bits 0-15]     Para  10 32Hz
    | NON DEICED TEMPERATURE        [bits 0-15]     Para  23 32Hz
    | SIGNAL REGISTER               [drs units-bcd] Para  27  2Hz
    | STATIC PRESSURE               [mbs]           Para 576 32Hz
    | PITOT STATIC PRESSURE         [mbs]           Para 577 32Hz

  :Outputs:
    | INDICATED AIR TEMPERATURE (Deiced)   [K]      Para 519 32Hz
    | TRUE AIR TEMPERATURE      (Deiced)   [K]      Para 520 32Hz
    | INDICATED AIR TEMPERATURE (NonDeiced)[K]      Para 524 32Hz
    | TRUE AIR TEMPERATURE      (NonDeiced)[K]      Para 525 32Hz

:SUBPROGRAMS:
  | S_MACH           Calculates Mach no
  | ITSTFLG          Examines bits 16,17 for flags
  | ISETFLG          Sets flag bits 16,17 = 0 --> 3
  | S_QCPT           Performs range and rate of change check

:REFERENCES:
  Code adapted from C_TEMPS module. See MRF Internal Note 55 -
  'Temperature Measurement Working Group Report' for full
  details of C-130 temperature measurement.

:CHANGES:
  V1.01  27/09/02  W.D.N.JACKSON
  Changed to handle 16 bit temperature recording.

  V1.02  23/05/05  D.A.TIDDEMAN
  Temperature heater correction changed to opposite sense
  Now raw para 27 bit 5 on = heater on

"""
    def __init__(self, dataset):
        self.input_names = ['TRFCTR',
                            'CALDIT',
                            'CALNDT',
                            'CORCON_di_temp',
                            'SREG',
                            'CORCON_ndi_temp',
                            'SECS',
                            'PS_RVSM',
                            'Q_RVSM']

        self.outputs = [parameter('ITDI',
                                  units='DEG K',
                                  frequency=32,
                                  number=519,
                                  long_name='DEICED IND TEMP'),
                        parameter('TAT_DI_R',
                                  units='degK',
                                  frequency=32,
                                  number=520,
                                  long_name='True air temperature from the Rosemount deiced temperature sensor',
                                  standard_name='air_temperature'),
                        parameter('NDTI',
                                  units='DEG K',
                                  frequency=32,
                                  number=524,
                                  long_name='NONDEICED IND TEMP'),
                        parameter('TAT_ND_R',
                                  units='degK',
                                  frequency=32,
                                  number=525,
                                  long_name='True air temperature from the Rosemount non-deiced temperature sensor',
                                  standard_name='air_temperature')]
        self.fortname = 'TEMPS2'
        # self.name='RIO_TEMPS'
        self.version = 1.00
        fort_cal.__init__(self, dataset)

    def set_temperature_diff_flag(self, threshold=1):
        """
        Whenever there is a significant difference between DI and NDI
        temperatures, there is clearly an issue with one of the instruments.
        As we cannot be sure which, we flag both variables with 1 whenever a
        threshold delta is exceeded (assuming flag is not already > 1).

        kwargs:
            threshold: the maximum absolute difference between DI and ND before
            the data are flagged.
        """

        # Find the indicies where a threshold is exceeded
        ndi_di_flag = np.where(
            np.abs((self.outputs[1].data - self.outputs[3].data)) > threshold
        )

        for _output in (1, 3):
            # For TAT_DI_R and TAT_ND_R (outputs 1 & 3), ensure that the flag
            # where temps disagree is at least 1, but is not decremented if
            # already greater than 1.
            new_flag = np.maximum(
                np.ones_like(self.outputs[_output].flag[ndi_di_flag]),
                self.outputs[_output].flag[ndi_di_flag]
            )
            self.outputs[_output].flag[ndi_di_flag] = new_flag

            # Add a parameter to output netCDF to record the threshold at which
            # we flag.
            self.dataset.add_para(
                'Attribute', 'RosemountTemperatureThreshold', threshold
            )

    def process(self):
        self.dataset[self.input_names[3]].number = 10
        self.dataset[self.input_names[5]].number = 23
        fort_cal.process(self)

        # Check if Sensortype and Serialnumber information is available from
        # the flight constant file and if so include it in the long_name
        # attribute of the NC-variable
        if 'DITSENS' in list(self.dataset.keys()):
            tmpl = 'True air temperature from the Rosemount deiced temperature sensor (Type: %s; SN: %s)'
            tat_di_r_long_name = tmpl % (str(self.dataset['DITSENS'][1]), str(self.dataset['DITSENS'][0]))
            self.outputs[1].long_name = tat_di_r_long_name

        if 'NDTSENS' in list(self.dataset.keys()):
            tmpl = 'True air temperature from the Rosemount non-deiced temperature sensor (Type: %s; SN: %s)'
            tat_nd_r_long_name = tmpl % (str(self.dataset['NDTSENS'][1]), str(self.dataset['NDTSENS'][0]))
            self.outputs[3].long_name = tat_nd_r_long_name

        # Flag temperature data where ND/DI do not agree sufficiently.
        self.set_temperature_diff_flag()
