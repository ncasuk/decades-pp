from ppodd.core import *

import numpy as np

class fast_temperature_sensor(cal_base):

    def __init__(self,dataset):
        self.input_names=['CORCON_fast_temp', 'CORCON_fasttemp_hi_lo', 'PS_RVSM', 'Q_RVSM',  'TRFCTR']
        self.outputs=[parameter('IAT_FT',
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
        #TODO:
        #calibration coefficients should be moved calibration file
        #A=1.437798
        #B=-32.38704
        #C=365.651552
        D=-1.020386E-21
        E=2.035221E-14
        F=-1.481141E-7
        G=0.4003893
        # new calibration constants:  03/06/2014
        A = 1.1692
        B =-29.609
        C = 361.04

        #d['TRFCTR'].data
        TRFCTR = 0.9928

        d=self.dataset
        match=d.matchtimes(['CORCON_fast_temp', 'CORCON_fasttemp_hi_lo', 'PS_RVSM', 'Q_RVSM'])
        fast_temp_adc_counts=d['CORCON_fast_temp'].data.ismatch(match)
        #convert to float for further processing
        fast_temp_adc_counts=fast_temp_adc_counts.astype(np.float)
        sp=d['PS_RVSM'].data.ismatch(match)
        pitot=d['Q_RVSM'].data.ismatch(match)

        #calculate resistance (R in kOhm)
        R=1.0/(D*(fast_temp_adc_counts**3)+E*(fast_temp_adc_counts**2)+F*fast_temp_adc_counts+G)
        #calculate indicated air temperature in Kelvin
        IAT=A*np.log(R)**2+B*np.log(R)+C
        #calculate mach number
        MACHNO=np.sqrt(5.0*((1.0+pitot/sp)**(2./7.)-1.))
        #calculate true air temperature
        TAT=IAT/(1.0+(0.2*MACHNO**2*TRFCTR))

        #TODO:
        #Flag all data as 3
        flag=TAT*0+3
        flag=flag.astype(np.int8)

        result0=flagged_data(IAT, fast_temp_adc_counts.times, flag)
        result1=flagged_data(TAT, fast_temp_adc_counts.times, flag)

        self.outputs[0].data=result0
        self.outputs[1].data=result1
