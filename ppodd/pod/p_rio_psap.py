from ppodd.core import *

class rio_psap(cal_base):

    def __init__(self,dataset):
        self.input_names=['AERACK_psap_flow',
                          'AERACK_psap_lin',
                          'AERACK_psap_log',
                          'AERACK_psap_transmission']

        self.outputs=[parameter('PSAP_LIN', units='m-1', frequency=1, number=648, long_name='Uncorrected absorption coefficient at 565nm, linear, from PSAP'),
                      parameter('PSAP_LOG', units='*',   frequency=1, number=649, long_name='Uncorrected absorption coefficient at 565nm, log, from PSAP'),
                      parameter('PSAP_FLO', units='standard l min-1', frequency=1, long_name='PSAP Flow'),
                      parameter('PSAP_TRA', units='percent', frequency=1, long_name='PSAP Transmittance')]

        self.version=1.00
        cal_base.__init__(self,dataset)


    def process(self):
        match=self.dataset.matchtimes(self.input_names)
        psap_flow=self.dataset['AERACK_psap_flow'].ismatch(match)
        psap_flow*=0.5
        psap_lin=self.dataset['AERACK_psap_lin'].ismatch(match)
        pasp_lin=psap_lin*0.5E-5
        psap_log=self.dataset['AERACK_psap_log'].ismatch(match)
        pasp_log=10.0**((psap_log/2.0)-7.0)
        psap_transmission=self.dataset['AERACK_psap_transmission'].ismatch(match)
        psap_transmission*=0.125

        n=psap_flow.size
        # create default flag array set to 3
        flag=np.array([0]*n, dtype=np.int8)
        # Flagging using flow threshold
        flag[psap_flow<1.0]=2
        flag[(psap_transmission<0.5) | (psap_transmission>1.0)]=1
        flag[((psap_transmission<0.5) | (psap_transmission>1.0)) & (psap_flow<1.0)]=3
        self.outputs[0].data=flagged_data(psap_lin, psap_lin.times, flag)
        self.outputs[1].data=flagged_data(psap_log, psap_log.times, flag)
        self.outputs[2].data=flagged_data(psap_flow, psap_flow.times, flag)
        self.outputs[3].data=flagged_data(psap_transmission, psap_transmission.times, flag)
        return
