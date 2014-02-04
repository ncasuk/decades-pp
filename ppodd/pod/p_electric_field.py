from ppodd.core import *


class electric_field_zeus(cal_base):

    def __init__(self,dataset):
        self.input_names=['ZEUS00_zeus_eField']
        self.outputs=[parameter('EXX_ZEUS',
                                units='adc counts',
                                frequency=1,
                                long_name='Electric field mill output as measured from the Zeus sensor'),]
        self.version=1.00
        cal_base.__init__(self,dataset) 

    def process(self):
        d=self.dataset
        zeus_data=d['ZEUS00_zeus_eField'].data
        zeus_flag=d['ZEUS00_zeus_eField'].data*0+3
        zeus_var=flagged_data(zeus_data, zeus_data.times, zeus_flag)
        self.outputs[0].data=zeus_var
    
        
class electric_field_jci140(cal_base):

    def __init__(self,dataset):
        self.input_names=['PRTAFT_jci140_signal', 'PRTAFT_jci140_range']
        self.outputs=[parameter('EXX_JCI',
                                units='adc counts',
                                frequency=1,
                                long_name='Raw data from the Fwd Core Console JCI static monitor, static signal'),]
        self.version=1.00
        cal_base.__init__(self,dataset) 

    def process(self):
        d=self.dataset
        jci_data=d['PRTAFT_jci140_signal'].data
        jci_flag=d['PRTAFT_jci140_signal'].data*0+3
        jci_var=flagged_data(jci_data, jci_data.times, jci_flag)
        self.outputs[0].data=jci_var





