import numpy as np
import pandas as pd

from ppodd.core import *

FLOW_VALID_MIN = 1000


class rio_2bozone(cal_base):

    def __init__(self, dataset):
        self.input_names = [
            'TWBOZO_conc', 'TWBOZO_flow', 'WOW_IND'
        ]

        self.outputs = [
            parameter(
                'O3_2BTECH',
                units='ppb',
                frequency=1,
                long_name='Mole fraction of Ozone in air from the 2BTech instrument',
                standard_name='mole_fraction_of_ozone_in_air'
            )
        ]

        self.version = 1.00
        cal_base.__init__(self, dataset)

    def process(self):
        """
        Process the 2B Ozone signal. All that's required is resampling to 1Hz
        and filling the gaps.

        Flagging criteria:
            aircraft on ground => 3
            flow rate below threshold => 2
        """

        conc = self.dataset['TWBOZO_conc'].data.Series()
        flow = self.dataset['TWBOZO_flow'].data.Series()

        wow = self.dataset['WOW_IND']

        # Resampling
        conc = conc.reindex(wow.times).fillna(method='ffill', limit=2)
        flow = flow.reindex(wow.times).fillna(method='ffill', limit=2)

        # flagging
        flag = 0 * conc.values
        flag[wow != 0] = 3
        flag[flow < FLOW_VALID_MIN] = 2

        self.outputs[0].data = flagged_data(
            conc.values,
            timestamp(conc.index.values),
            flag.astype(np.int8)
        )
