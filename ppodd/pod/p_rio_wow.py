from ppodd.core import *

import numpy as np

class rio_weight_on_wheels(cal_base):
    """
    Weight on wheels indicator.
    
    The weight on wheel status is logged on the PRTAFT CRIO.

    :DESCRIPTION:    
      | 1 aircraft is on the ground
      | 0 aircraft is in the air
  
    :FLAGGING:    
      No flagging needed for this variable. A dummy flag is added to the core
      netCDF for consistency (all other variables have a flag variable). A lot
      of code will expect a _FLAG variable for every variable in the dataset.
    """

    def __init__(self, dataset):
        self.input_names = ['PRTAFT_utc_time',
                            'PRTAFT_wow_flag']
        self.outputs = [parameter('WOW_IND',
                                  units='-',
                                  frequency=1,
                                  long_name='Weight on wheels indicator')]
        self.version = 1.00
        cal_base.__init__(self, dataset)

    def process(self):
        match = self.dataset.matchtimes(self.input_names)
        wow = self.dataset['PRTAFT_wow_flag'].data.ismatch(match)

        flag = np.array([0]*wow.size, dtype=np.int8)
        wow_par = flagged_data(wow, match, flag)
        self.outputs[0].data = wow_par
