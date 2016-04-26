# -*- coding: utf-8 -*-
import numpy as np

from ppodd.core import *


class rio_cpc(cal_base):
    """Routine to process CPC (Condensation Particle Counter) data from the TSI 3786 instrument.
    
    """

    def __init__(self,dataset):
        self.input_names=['CPC37801_utc_time',
                          'CPC37801_counts',
                          'CPC37801_sample_flow',
                          'CPC37801_total_flow',
                          'CPC37801_sheath_flow',
                          'CPC37801_pressure',
                          'CPC37801_saturator_temp',
                          'CPC37801_growth_tube_temp',
                          'CPC37801_optics_temp']
                          
        self.outputs=[parameter('CPC_CNTS',
                                units='#',
                                frequency=10,
                                long_name='Condensation Particle Counter measure by the TSI 3786')]
        
        self.version=1.00
        cal_base.__init__(self,dataset)

    def process(self):
        match=self.dataset.matchtimes(self.input_names)
        cpc_cnts=self.dataset['CPC37801_counts'].data.match()
        cpc_sample_flow=self.dataset['CPC37801_sample_flow'].data.match()
        cpc_total_flow=self.dataset['CPC37801_total_flow'].data.match()
        cpc_sheath_flow=self.dataset['CPC37801_sheath_flow'].data.match()
        cpc_pressure=self.dataset['CPC37801_pressure'].data.match()
        cpc_saturator_temp=self.dataset['CPC37801_saturator_temp'].data.match()
        cpc_growth_tube_temp=self.dataset['CPC37801_growth_tube_temp'].data.match()
        cpc_optics_temp=self.dataset['CPC37801_optics_temp'].data.match()
        
        # FLAG 0 – Data OK
        # FLAG 1 = Saturator temperature more than 6 degrees C, Growth or Optics Temp more/less than 10% from prescribed value
        # FLAG 2 = Aerosol (Sample) flow more/less than 10% from prescribed value
        # FLAG 3 = Sheath flow more/less than 10% from prescribed value
        
        flag=np.array([0]*cpc_cnts.size, dtype=np.int8) # initialize empty flag array, with all flags set to 0
        
        flag[cpc_saturator_temp > 6.0]=1
        flag[(cpc_growth_tube_temp < 40.5) | (cpc_growth_tube_temp > 49.5)]=1
        flag[(cpc_optics_temp < 40.5) | (cpc_optics_temp > 49.5)]=1
        flag[(cpc_sample_flow < 0.27) | (cpc_sample_flow > 0.33)]=2
        flag[(cpc_sheath_flow < 0.27) | (cpc_sheath_flow > 0.33)]=3
        
        cpc_cnts=flagged_data(cpc_cnts, match, flag)
        self.outputs[0].data=cpc_cnts
        return

