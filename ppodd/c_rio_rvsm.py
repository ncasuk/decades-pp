from cal_base import *
from scipy.interpolate import interp1d

class c_rio_rvsm(fort_cal):
    def __init__(self,dataset):
        self.input_names=['PRTAFT_pressure_alt', 'PRTAFT_ind_air_speed']
        self.outputs=[parameter('PS_RVSM',units='hPa',frequency=32,number=576,long_name='Static pressure from the aircraft RVSM (air data) system')
                     ,parameter('Q_RVSM',units='hPa',frequency=32,number=577,long_name='Pitot static pressure inverted from RVSM (air data) system indicated airspeed')
                     ,parameter('PALT_RVS',units='m',frequency=32,number=578,long_name='Pressure altitude from the aircraft RVSM (air data) system')]
        self.name='C_RIO_RVSM'
        self.fortname='RVSM'
        self.version=1.00
        fort_cal.__init__(self,dataset)

    def resample(self,P,flag,freq=32):
        Pflat=P.ravel()
        Pflat.interp1d()
        tnew=P.times.at_frequency(freq)
        ifl=interp1d(Pflat.times,flag.ravel(),bounds_error=False)
        flagnew=(ifl(tnew.ravel()).reshape(tnew.shape)!=0)*3
        return flagged_data(Pflat.interpolated(tnew.ravel()).reshape(tnew.shape),P.times,flagnew)
        
    def process(self):
        if(self.dataset[self.input_names[0]].data is not None):
            palt_feet=self.dataset[self.input_names[0]]*4
            flag_alt=(palt_feet<-2000) | (palt_feet>50000)
            palt_meters=palt_feet/3.28084
            high=palt_meters>11000
            low=palt_meters<=11000
            """ Compute Pressure from altitude and barometric formula """
            T0=288.15
            L0=-0.0065
            h0=0.0
            go=9.80665
            M=0.0289644
            R=8.31432
            P0=1013.2500
            P=P0*(T0/(T0+L0*(palt_meters-h0)))**(go*M/(R*L0))
            if(len(np.where(high)[0]) > 0):
                """ Lapse rate is 0.0 from 11000 to 20000 meters - maybe we never get this high.."""
                T1=216.65
                P1=226.3210
                h1=11000.0
                P.raw_data[high]=P1*np.exp((-go*M*(palt_meters.raw_data[high]-h1))/(R*T1))
            """ Resample at 32 Hz """
            self.outputs[0].data=self.resample(P,flag_alt)
            self.outputs[2].data=self.resample(palt_meters,flag_alt)
            if(self.dataset[self.input_names[1]].data  is not None):
                ias=self.dataset[self.input_names[1]]*0.514444/32.0  # m/s
                flag_ias=(ias<-50) | (ias>500) | flag_alt
                mach=ias/(340.294*np.sqrt(P/1013.25))
                pitot=P*((((mach**2.)/5.+1.)**3.5)-1.)
                self.outputs[1].data=self.resample(pitot,flag_ias)

