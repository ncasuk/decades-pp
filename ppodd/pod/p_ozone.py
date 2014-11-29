from ppodd.core import *

class ozone(cal_base):
    """
    Calibrate TEI Ozone instrument
    Flagging uses also the flow in the two chambers
"""
    def __init__(self,dataset):
        self.input_names=['CALO3', 'CALO3P', 'CALO3T', 'CALO3F', 'CALO3MX','TEIOZO_conc','TEIOZO_benchtemp','TEIOZO_press','TEIOZO_flag', 'TEIOZO_FlowA', 'TEIOZO_FlowB', 'WOW_IND']
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
        flag=self.dataset['TEIOZO_flag'].data.ismatch(match)!='1c100000'
        flow_a=self.dataset['TEIOZO_FlowA'].data.ismatch(match)
        flow_b=self.dataset['TEIOZO_FlowB'].data.ismatch(match)
        wow_ind=self.dataset['WOW_IND'].data.ismatch(match)
        flow_threshold=0.68
        flag[conc < -10]=2
        flag[flow_a < flow_threshold]=3
        flag[flow_b < flow_threshold]=3
        flag[wow_ind != 1]=3
        self.outputs[0].data=flagged_data(conc,conc.times,flag)


