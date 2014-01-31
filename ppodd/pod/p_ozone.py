from ppodd.core import *

class ozone(cal_base):
    """
    Calibrate TEI Ozone instrument
    Flagging uses also the flow in the two chambers
"""
    def __init__(self,dataset):
        self.input_names=['CALO3', 'CALO3P', 'CALO3T', 'CALO3F', 'CALO3MX','TEIOZO_conc','TEIOZO_benchtemp','TEIOZO_press','TEIOZO_flag', 'TEIOZO_FlowA', 'TEIOZO_FlowB']
        self.outputs=[parameter('O3_TECO',
                                units='ppb',
                                frequency=1,
                                number=574,
                                long_name='Mole fraction of ozone in air from the TECO 49 instrument')]
        self.version=1.00
        cal_base.__init__(self,dataset)

    def process(self):
        conc=self.dataset['TEIOZO_conc'].data
        flag=self.dataset['TEIOZO_flag'].data!='1c100000'
        flow_a=self.dataset['TEIOZO_FlowA'].data
        flow_b=self.dataset['TEIOZO_FlowB'].data
        flag[conc < -10]=2
        flag[flow_a < 0.7]=3
        flag[flow_b < 0.7]=3
        self.outputs[0].data=flagged_data(conc,conc.times,flag)
        

