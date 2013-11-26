from ppodd.core import *
class ozone(cal_base):
    """
    Calibrate TEI Ozone instrument
"""
    def __init__(self,dataset):
        self.input_names=['CALO3', 'CALO3P', 'CALO3T', 'CALO3F', 'CALO3MX','TEIOZO_conc','TEIOZO_benchtemp','TEIOZO_press','TEIOZO_flag']
        self.outputs=[parameter('O3_TECO',units='ppb',frequency=1,number=574,long_name='Mole fraction of ozone in air from the TECO 49 instrument')]
        self.version=1.00
        cal_base.__init__(self,dataset)

    def process(self):
        conc=self.dataset['TEIOZO_conc'].data
        flag=self.dataset['TEIOZO_flag'].data!='1c100000'
        self.outputs[0].data=flagged_data(conc,conc.times,flag)
        

