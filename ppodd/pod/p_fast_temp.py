from ppodd.core import *

import numpy as np

class rio_fast_thermistor(cal_base):

    def __init__(self,dataset):
        self.input_names=['CORCON_fast_temp', 'CORCON_fasttemp_hi_lo', 'PS_RVSM', 'Q_RVSM',  'TRFCTR']
        self.outputs=[parameter('R_FT',
                                units='ohm',
                                frequency=32,
                                long_name='Resistance of the indicated air temperature sensor'),
                      parameter('IAT_FT',
                                units='degK',
                                frequency=32,
                                long_name='Indicated air temperature from the fast temperature sensor.',
                                standard_name='indicated_air_temperature'),
                      parameter('TAT_FT',
                                units='degK',
                                frequency=32,
                                long_name='True air temperature from the fast temperature sensor.',
                                standard_name='true_air_temperature')]
        self.version=1.00
        cal_base.__init__(self,dataset)

    def process(self):
        # TODO:
        # calibration coefficients should be moved to calibration file
        # new calibration constants:  06/06/2014
        A,B,C,D,E,F,G,H=(1.1692, -29.609, 361.04, -0.030927, 0.23438, -2.4769E-7, 0.59181, -9.7277E-7)

        TRFCTR=self.dataset['TRFCTR'].data[0]

        match=self.dataset.matchtimes(['CORCON_fast_temp', 'CORCON_fasttemp_hi_lo', 'PS_RVSM', 'Q_RVSM'])
        fast_temp_adc_counts=self.dataset['CORCON_fast_temp'].data.ismatch(match)
        #convert to float for further processing
        fast_temp_adc_counts=fast_temp_adc_counts.astype(np.float)
        sp=self.dataset['PS_RVSM'].data.ismatch(match)
        pitot=self.dataset['Q_RVSM'].data.ismatch(match)

        #calculate resistance (R in kOhm)
        R=1.0/(D+E*np.exp(fast_temp_adc_counts*F)+G*np.exp(H*fast_temp_adc_counts))

        #calculate indicated air temperature in Kelvin
        #set errstate to get rid off annoying RuntimeWarning
        with np.errstate(invalid='ignore'):
            IAT=A*np.log(R)**2+B*np.log(R)+C
        #calculate mach number
        MACHNO=np.sqrt(5.0*((1.0+pitot/sp)**(2./7.)-1.))
        #calculate true air temperature
        TAT=IAT/(1.0+(0.2*MACHNO**2*TRFCTR))

        #TODO:
        #Flag all data as 3 for the moment
        flag=np.int8(TAT[:]*0+3)

        result0=flagged_data(R, fast_temp_adc_counts.times, flag)
        result1=flagged_data(IAT, fast_temp_adc_counts.times, flag)
        result2=flagged_data(TAT, fast_temp_adc_counts.times, flag)

        self.outputs[0].data=result0
        self.outputs[1].data=result1
        self.outputs[2].data=result2
