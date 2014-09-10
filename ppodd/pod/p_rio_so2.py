from ppodd.core import *



class rio_so2_mixingratio(cal_base):
    """Routine to extract SO2 concentration from the SO2-TECO analyser.

    TODO:
    So far no flagging implemented
    
    """

    def __init__(self,dataset):
        self.input_names=['CHTSOO_conc', 'CHTSOO_flow', 'CHTSOO_flags', 'CHTSOO_utc_time']
        self.outputs=[parameter('SO2_TECO',
                                units='ppb',
                                frequency=1,
                                number=740,
                                long_name='Mole fraction of Sulphur Dioxide in air from TECO 43 instrument',
                                standard_name='mole_fraction_of_sulphur_dioxide_in_air')]
        self.version=1.00
        cal_base.__init__(self,dataset)

    def process(self):
        match=self.dataset.matchtimes(self.input_names)
        so2_mr=self.dataset['CHTSOO_conc'].data.ismatch(match)

        flag=np.array([0]*so2_mr.size, dtype=np.int8) # initialize empty flag array, with all flags set to 0
        #flag[co_mr<-10]=3         # flag very negative co_mr as 3
        #flag[cal_status==1]=3     # flag calibration data points
        #flag[calpress>3.2]=3      # flag when calibration gas pressure is increased

        so2_teco=flagged_data(so2_mr, match, flag)

        self.outputs[0].data=so2_teco

