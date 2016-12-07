from ppodd.core import *

class rio_ozone(cal_base):
    """
Calibrated TEI Ozone instrument data
The fllagging uses also the flow in the two chambers

:Flagging:    
  | flag=flag!='1c100000'
  | flag[conc < -10]=2
  | flag[flow_a < flow_threshold]=3
  | flag[flow_b < flow_threshold]=3
  | flag[wow_ind != 0]=3
        
"""
    def __init__(self,dataset):
        self.input_names=['CALO3', 'CALO3P', 'CALO3T', 'CALO3F', 'CALO3MX',
                          'TEIOZO_conc', 'TEIOZO_benchtemp', 'TEIOZO_press',
                          'TEIOZO_flag', 'TEIOZO_FlowA', 'TEIOZO_FlowB', 'WOW_IND']
        self.outputs=[parameter('O3_TECO',
                                units='ppb',
                                frequency=1,
                                number=574,
                                long_name='Mole fraction of Ozone in air from the TECO 49 instrument',
                                standard_name='mole_fraction_of_ozone_in_air')]
        self.version=1.00
        cal_base.__init__(self,dataset)

    def process(self):
        match=self.dataset.matchtimes(self.input_names)
        conc=self.dataset['TEIOZO_conc'].data.ismatch(match)
        flow_a=self.dataset['TEIOZO_FlowA'].data.ismatch(match)
        flow_b=self.dataset['TEIOZO_FlowB'].data.ismatch(match)
        wow_ind=self.dataset['WOW_IND'].data.ismatch(match)

        flow_threshold=0.50 # flows below that are suspicious

        flag=self.dataset['TEIOZO_flag'].data.ismatch(match)
        flag=self.dataset['TEIOZO_flag'].data
        flag=flag!='1c100000'
        flag=flag.astype('int8')

        flag[conc < -10]=2
        flag[flow_a < flow_threshold]=3
        flag[flow_b < flow_threshold]=3
        flag[wow_ind != 0]=3
        #flag all data take-off+20secs
        to_ix=np.where(wow_ind[:-1]-wow_ind[1:] == 1)[0]
        flag[to_ix:to_ix+20]=3
        self.outputs[0].data=flagged_data(conc,conc.times,flag)
