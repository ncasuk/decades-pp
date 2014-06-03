from ppodd.core import *

import numpy as np


def create_nephelometer_flag(aerack_neph_status):
    """The status flag definition is described in the TSI 3563
    manual on page 6-24. 
    
    The flaggin is done for the (i) scattering values, (ii) the relative
    humidity, (iii) temperature, and (iv) pressure measurements.
    """
    n=aerack_neph_status.size
    # We create a flag dictionary which holds the flagging values
    # for (i) Scattering, (ii) relative humidity, (iii) temperature,
    # and (iv) pressure. The default flag is set to 0
    flag_dict={'SC': np.array([0]*n, dtype=np.int8),
               'RH': np.array([0]*n, dtype=np.int8),
               'T':  np.array([0]*n, dtype=np.int8),
               'P':  np.array([0]*n, dtype=np.int8)}
    neph_status_list=list(aerack_neph_status)
    # convert to 16 character string representing the binary encoding 
    tmp=['{0:04}'.format(int(x)) for x in neph_status_list]
    neph_status=['{0:04b}{1:04b}{2:04b}{3:04b}'.format(int(t[0]), int(t[1]), int(t[2]),  int(t[3])) for t in tmp]
    # Lamp Fault
    ix=np.array([int(flag[-1]) for flag in neph_status])
    flag_dict['SC'][ix==1]=3
    # Valve Fault
    ix=np.array([int(flag[-2]) for flag in neph_status])
    flag_dict['SC'][ix==1]=3
    # Chopper Fault
    ix=np.array([int(flag[-3]) for flag in neph_status])
    flag_dict['SC'][ix==1]=3
    # Shutter Fault
    ix=np.array([int(flag[-4]) for flag in neph_status])
    flag_dict['SC'][ix==1]=3
    # Heater unstable
    ix=np.array([int(flag[-5]) for flag in neph_status])
    flag_dict['SC'][ix==1]=3
    flag_dict['RH'][ix==1]=3
    flag_dict['T'][ix==1]=3
    # Pressure out of range
    ix=np.array([int(flag[-6]) for flag in neph_status])
    flag_dict['SC'][ix==1]=3
    flag_dict['P'][ix==1]=3
    # Sample T out of range
    ix=np.array([int(flag[-7]) for flag in neph_status])
    flag_dict['SC'][ix==1]=3
    flag_dict['T'][ix==1]=3
    # Inlet T out of range
    ix=np.array([int(flag[-8]) for flag in neph_status])
    flag_dict['SC'][ix==1]=3
    # RH out of range
    ix=np.array([int(flag[-9]) for flag in neph_status])
    flag_dict['RH'][ix==1]=3
    # All faults
    ix=np.array([int(flag[-9:]) for flag in neph_status])
    flag_dict['SC'][ix==111111111]=3
    flag_dict['RH'][ix==111111111]=3
    flag_dict['T'][ix==111111111]=3
    flag_dict['P'][ix==111111111]=3
    
    return flag_dict
    
    

class rio_nephelometer(cal_base):

    def __init__(self,dataset):
        self.input_names=['AERACK_neph_total_blue',
                          'AERACK_neph_total_green',
                          'AERACK_neph_pressure',
                          'AERACK_neph_temp',
                          'AERACK_neph_backscatter_blue',
                          'AERACK_neph_backscatter_green',
                          'AERACK_neph_backscatter_red',
                          'AERACK_neph_total_red',
                          'AERACK_neph_humidity',
                          'AERACK_neph_status',
                          'AERACK_neph_mode']

        self.outputs=[parameter('NEPH_PR', units='hPa',  frequency=1,number=760,long_name='Internal sample pressure of the Nephelometer'),
                      parameter('NEPH_T',  units='K',    frequency=1,number=761,long_name='Internal sample temperature of the Nephelometer'),
                      parameter('NEPH_RH',units='%',     frequency=1,number=768,long_name='Relative humidity from TSI 3563 Nephelometer'),
                      parameter('TSC_BLUU',units='m-1',  frequency=1,number=762,long_name='Uncorrected blue total scattering coefficient from TSI 3563 Nephelometer'),
                      parameter('TSC_GRNU',units='m-1',  frequency=1,number=763,long_name='Uncorrected green total scattering coefficient from TSI 3563 Nephelometer'),
                      parameter('TSC_REDU',units='m-1',  frequency=1,number=764,long_name='Uncorrected red total scattering coefficient from TSI 3563 Nephelometer'),
                      parameter('BSC_BLUU',units='m-1',  frequency=1,number=765,long_name='Uncorrected blue back scattering coefficient from TSI 3563 Nephelometer'),
                      parameter('BSC_GRNU',units='m-1',  frequency=1,number=766,long_name='Uncorrected green back scattering coefficient from TSI 3563 Nephelometer'),
                      parameter('BSC_REDU',units='m-1',  frequency=1,number=767,long_name='Uncorrected red back scattering coefficient from TSI 3563 Nephelometer')]
        
        self.version=1.00
        cal_base.__init__(self,dataset)


    def process(self):
        """No data processing is done to the data that are recorded and streamed
        by the RIO modules. However, the units of the scatter values are converted
        from megameters-1 to meters-1.
        
        """
        match=self.dataset.matchtimes(self.input_names)
        time = self.dataset['AERACK_neph_status'].ismatch(match)

        # Translation list
        # TCP definition file name to netCDF variable name
        pairs = [('AERACK_neph_total_blue',        'TSC_BLUU'),
                 ('AERACK_neph_total_green',       'TSC_GRNU'),
                 ('AERACK_neph_total_red',         'TSC_REDU'),
                 ('AERACK_neph_backscatter_blue',  'BSC_BLUU'),
                 ('AERACK_neph_backscatter_green', 'BSC_GRNU'),
                 ('AERACK_neph_backscatter_red',   'BSC_REDU'),
                 ('AERACK_neph_pressure',          'NEPH_PR'),
                 ('AERACK_neph_temp',              'NEPH_T'),
                 ('AERACK_neph_humidity',          'NEPH_RH')]
        
        aerack_neph_status=self.dataset['AERACK_neph_mode'].data.ismatch(match)
        neph_flag=create_nephelometer_flag(aerack_neph_status)
                
        for p in pairs:
            ix=[i.name for i in self.outputs].index(p[1])
            data=self.dataset[p[0]].data.ismatch(match)            
            if 'SC_' in p[1]:
                #convert from megameters (Mm-1) to meters (m-1)
                data=data.astype(np.float)/10**6
                flag=neph_flag['SC']
            elif '_PR' in p[1]:                
                flag=neph_flag['P']
            elif '_T' in p[1]:                
                flag=neph_flag['T']
            elif '_RH' in p[1]:                
                flag=neph_flag['RH']
            else:
                continue                                
            fdata=flagged_data(data,data.times,flag)
            self.outputs[ix].data=fdata

