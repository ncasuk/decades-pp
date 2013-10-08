#from ppodd.pod import *
from ppodd.core import *
class nevz(fort_cal):
    """
FORTRAN routine C_NEVZ

 ROUTINE          C_NEVZ SUBROUTINE FORTVAX
     
 PURPOSE          Produces calibrated Nevzorov parameters

 DESCRIPTION      Calculates liquid and total water values for the Nevzorov
                  together with reference and collector voltages, using the
                  equations supplied with the unit, namely:

                  Water content = V**2/U/L/SR

                  where V is the output voltage (V)
                        U is the True air speed (m/s)
                        L is the energy expended in heating and evaporating
                             the water, for which a value of 2589 J/g is used
                        SR is the product of the sensor area and the resistanc
                           of the collector sensor at the chosen temperature.

                  Flagging:

                  If on Calibrate, as indicated by bit 1 or 2 in the signal
                  register being set then the data is flagged with a 2.
                  Otherwise the data carries the flag of the True Airspeed
                  parameter.

 VERSION          1.00  18/01/99  W.D.N.JACKSON

 ARGUMENTS     
                  Constants:
                  RCONST(1)   CALNVLW X0
                  RCONST(2)   CALNVLW X1
                  RCONST(3)   CALNVLR X0
                  RCONST(4)   CALNVLR X1
                  RCONST(5)   CALNVLC X0
                  RCONST(6)   CALNVLC X1
                  RCONST(7)   CALNVTW X0
                  RCONST(8)   CALNVTW X1
                  RCONST(9)   CALNVTR X0
                  RCONST(10)  CALNVTR X1
                  RCONST(11)  CALNVTC X0
                  RCONST(12)  CALNVTC X1
                  RCONST(13)  CALRSL X0     RS Value at T0
                  RCONST(14)  CALRSL T0
                  RCONST(15)  CALRST X0     RS value at T0
                  RCONST(16)  CALRST T0

                  Inputs: 
                  NVLW Nevzorov Liquid Water                   Para 208  8 Hz
                  NVLR Nevzorov Liquid Reference               Para 209  8 Hz
                  NVLC Nevzorov Liquid Collector               Para 210  8 Hz
                  NVTW Nevzorov Total Water                    Para 211  8 Hz
                  NVTR Nevzorov Total Reference                Para 212  8 Hz
                  NVTC Nevzorov Total Collector                Para 213  8 Hz
                  SREG Signal register [bits 1-2]              Para  27  2 Hz
                  TAS  True airspeed                           Para 517 32 Hz

                  Outputs: 
                  NVLW Nevzorov Liquid Water     [gm-3]         Para 602 8 Hz
                  NVLR Nevzorov Liquid Reference [V]            Para 603 8 Hz
                  NVLC Nevzorov Liquid Collector [V]            Para 604 8 Hz
                  NVTW Nevzorov Total Water      [gm-3]         Para 605 8 Hz
                  NVTR Nevzorov Total Reference  [V]            Para 606 8 Hz
                  NVTC Nevzorov Total Collector  [V]            Para 607 8 Hz
                
 SUBPROGRAMS      ITSTFLG          Examines bits 16,17 for flags
                  ISETFLG          Sets flag bits 16,17 = 0 --> 3

 REFERENCES       

 CHANGES           V1.01  27/09/02  W.D.N.JACKSON
                   Changed to include handling of 16 bit data from the new 
                   DRS.
                   V1.02  13/11/06
                   Signal register bits 1 and 2 swapped to be the correct way
                   round 1=TW, 2=LW
                   V1.03  12/12/06
                   Voltage flags explicitly set to zero

*******************************************************************************

"""
    def __init__(self,dataset):
        self.input_names=['CALNVLW', 'CALNVLR', 'CALNVLC', 'CALNVTW', 'CALNVTR', 'CALNVTC', 'CALRSL', 'CALRST', 
                          'Horace_SREG', 'Horace_NVLW', 'Horace_NVLR', 'Horace_NVLC', 
                          'Horace_NVTW', 'Horace_NVTR', 'Horace_NVTC', 'TAS_RVSM']
        self.outputs=[parameter('NV_LWC_U',units='gram m-3',frequency=8,number=602,long_name='Uncorrected liquid water content from the Nevzorov probe')
                     ,parameter('NVLR_CAL',units='V',frequency=8,number=603,long_name='NEVZOROV LIQUID REFE')
                     ,parameter('NVLC_CAL',units='V',frequency=8,number=604,long_name='NEVZOROV LIQUID COLL')
                     ,parameter('NV_TCW_U',units='gram m-3',frequency=8,number=605,long_name='Uncorrected total condensed water content from the Nevzorov probe.')
                     ,parameter('NVTR_CAL',units='V',frequency=8,number=606,long_name='NEVZOROV TOTAL REFER')
                     ,parameter('NVTC_CAL',units='V',frequency=8,number=607,long_name='NEVZOROV TOTAL COLLE')]
        #self.name='NEVZ'
        self.version=1.00
        fort_cal.__init__(self,dataset)
