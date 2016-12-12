from ppodd.core import *
class rio_tpress(fort_cal):
    """
FORTRAN routine C_TPRESS

:ROUTINE:
  C_TPRESS SUBROUTINE FORTVAX
     
:PURPOSE:
  Calibrates the five turbulence probe pressure transducers into mb.

:DESCRIPTION:
  Apply calibration the combined transducer and DRS 
  coefficients to DRS parameters 215 to 219 to obtain derived
  parameters 773 to 777.  Invalid data is flagged with 3, data
  outside limits is flagged with 2.

:METHOD:
  For each DRS parameter to be calibrated

    1. If data is FFFF or FFFE then flag 3
    2. Apply the calibration constants
    3. Check the results for being within acceptable values.
    4. Set data flag bits (16+17) 0: Good data
      1. Data of lower quality
      2. Probably faulty, exceed limits
      3. Data absent or invalid.

:Flagging:
  If a value can't be computed, due to missing data
  missing constants, divide be zeroes, etc, a value of 0 is
  used, flagged with a three.  If a value is outside its 
  limits for range or rate of change, it is flagged with a two.
  If there are no problems with the data it is flagged with 0.

  Missing/corrupt data output as 0 flagged 3.
  Out of range data flagged 2.

:VERSION:
  1.00  23/07/03  W.D.N.JACKSON

:ARGUMENTS:
  :Inputs:
  | para 215 TBP1 32 Hz Turbulence probe centre port
  | para 216 TBP2 32 Hz Turbulence probe attack ports
  | para 217 TBP3 32 Hz Turbulence probe sideslip ports
  | para 218 TBP4 32 Hz Turbulence probe attack check
  | para 219 TBP5 32 Hz Turbulence probe sideslip check
  :Constants:
  | RCONST(1 to 4) Para 215 cal constants X0 to X3
  | RCONST(5 to 8) Para 216 cal constants X0 to X3
  | RCONST(9 to 12) Para 217 cal constants X0 to X3
  | RCONST(13 to 14) Para 218 cal constants X0 to X1
  | RCONST(15 to 16) Para 219 cal constants X0 to X1
  :Outputs:
  | para 773 TBP0 mb 32 Hz Centre pressure
  | para 774 TBPA mb 32 Hz Attack pressure
  | para 775 TBPB mb 32 Hz Sideslip pressure
  | para 776 TBPC mb 32 Hz Attack check pressure
  | para 777 TBPD mb 32 Hz Sideslip check pressure

:SUBPROGRAMS:
  ISETFLG 

:REFERENCES: 

:CHANGES:
  V1.00 23/07/03  WDNJ Original version
  Note that V1.00 has no limit checking and no use is made of
  the check pressures.
  
  V1.01 25/03/04  WDNJ
  Now takes third order calibration constants for the main
  transducers, and first order for the check transducers.
  
  V1.02 26/01/06 Phil Brown
  Realistic min/max values provided for centre-port, Pa, Pb
  for flagging purposes. Values alsoe provided for check
  pressures Ca, Cb based on current (and probably wrong)
  calibration coefficients.
  
  V1.03 09/02/11 Axel Wellpott
  From an email from Phil Brown: "The P0-S10 differential pressure
  (para 773) is flagged 2 if it exceeds 130.0 hPa. This is easily 
  exceeded when we do acceleration to max speed (min Angle of Attack)
  so all the subsequent parameters calculated n C_TURB.for end up with a
  flag-3 saetting. I reckon a better value would be 180.0 hPa."


"""
    def __init__(self,dataset):
        self.input_names=['CALTP1', 'CALTP2', 'CALTP3', 'CALTP4', 'CALTP5', 
                          'CORCON_tp_p0_s10', 'CORCON_tp_up_down', 'CORCON_tp_left_right', 'CORCON_tp_top_s10', 'CORCON_tp_right_s10']
        self.outputs=[parameter('P0_S10',units='hPa',frequency=32,number=773,long_name='Calibrated differential pressure between centre(P0) port and S10 static')
                     ,parameter('PA_TURB',units='hPa',frequency=32,number=774,long_name='Calibrated differential pressure between turbulence probe vertical ports')
                     ,parameter('PB_TURB',units='hPa',frequency=32,number=775,long_name='Calibrated differential pressure between turbulence probe horizontal ports')
                     ,parameter('TBPC',units='hPa',frequency=32,number=776,long_name='TURB PROBE Ca')
                     ,parameter('TBPD',units='hPa',frequency=32,number=777,long_name='TURB PROBE Cb')]
        #self.name='RIO_TPRESS'
        self.fortname='TPRESS'
        self.version=1.00
        fort_cal.__init__(self,dataset)

    def process(self): 
        self.dataset[self.input_names[5]].number=215
        self.dataset[self.input_names[6]].number=216
        self.dataset[self.input_names[7]].number=217
        self.dataset[self.input_names[8]].number=218
        self.dataset[self.input_names[9]].number=219
        fort_cal.process(self)
