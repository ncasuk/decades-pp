from ppodd.core import *

class rio_ozone(cal_base):
    """
:DESCRIPTION:
  Calibrated TEI Ozone instrument data
  The flagging of the data uses mainly the
  flow measurements in the two chambers
  | TEIOZO_FlowA
  | TEIOZO_FlowB
  Data points while on the ground (WOW_IND = 1) are
  also flagged as '3' and so are all data points for
  the first 20 seconds after take-off

  The TECO_49 also sends its own flag in the data
  stream (TEIOZO_flag) which is used for flagging

:FLAGGING:
  Flag values are set using flow_a, flow_b, weight on wheels index
  and the instruments own flag value
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


    def process(self, flow_threshold=0.5):
        """
        processes and flags the Ozone data from the TECO_49 instrument
        flagging is done using the flows

        :param float flow_threshold: definition of the flow
          threshold for good data; flows in either chamber below
          this threshold are flagged as '3'
        """
        match = self.dataset.matchtimes(self.input_names)
        conc = self.dataset['TEIOZO_conc'].data.ismatch(match)
        flow_a = self.dataset['TEIOZO_FlowA'].data.ismatch(match)
        flow_b = self.dataset['TEIOZO_FlowB'].data.ismatch(match)
        wow_ind = self.dataset['WOW_IND'].data.ismatch(match)

        flag = self.dataset['TEIOZO_flag'].data.ismatch(match)
        flag = flag!='1c100000'
        flag = flag.astype('int8')

        flag[conc < -10] = 2
        flag[flow_a < flow_threshold] = 3
        flag[flow_b < flow_threshold] = 3
        flag[wow_ind != 0]=3
        #flag all data take-off+20secs
        to_ix=int(np.where(wow_ind[:-1]-wow_ind[1:] == 1)[0])
        flag[to_ix:to_ix+20] = 3
        self.outputs[0].data = flagged_data(conc,conc.times,flag)
