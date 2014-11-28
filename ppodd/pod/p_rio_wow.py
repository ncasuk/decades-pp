from ppodd.core import *


class rio_weight_on_wheels(cal_base):
    """Weight on wheels flag; 1 indicates on the ground; 0 in the air

    """

    def __init__(self,dataset):
        self.input_names=['PRTAFT_utc_time', 'PRTAFT_wow_flag']
        self.outputs=[parameter('WOW_IND',
                                units='-',
                                frequency=1,
                                long_name='Weight on wheels indicator')]
        self.version=1.00
        cal_base.__init__(self,dataset)

    def process(self):
        match=self.dataset.matchtimes(self.input_names)
        wow=self.dataset['PRTAFT_wow_flag'].data.ismatch(match)

        flag=np.array([0]*wow.size, dtype=np.int8)
        wow_par=flagged_data(wow, match, flag)
        self.outputs[0].data=wow_par
