from cal_base import *
class c_rio_sreg(cal_base):
    def __init__(self,dataset):
        self.input_names=['CALCABT','PRTAFT_heimann_calib_flag','PRTAFT_deiced_temp_flag','CORCON_cabin_t']
        self.outputs=[parameter('SREG',units='bits',frequency=2,number=27,long_name='Signal register')
                     ,parameter('SREG_CAL',units='IBITS',frequency=2,number=641,long_name='SIGNAL REGISTER')
                     ,parameter('CAB_TEMP',units='degC',frequency=1,number=660,long_name='Cabin temperature at the core consoles')]
        self.name='RIO_SREG'
        self.version=1.00
        cal_base.__init__(self,dataset)
        
    def process(self):
        heim=self.dataset[self.input_names[1]].data
        di=np.left_shift(self.dataset[self.input_names[2]],5)
        sreg=timed_data(di.twod_array(arr=heim | di,frequency=2),di.times)
        self.outputs[0].data=sreg
        self.outputs[1].data=flagged_data(sreg.raw_data,sreg.times,np.zeros(sreg.raw_data.shape,dtype='uint8'))
        calcabt=self.dataset[self.input_names[0]].data
        cbt=self.dataset[self.input_names[3]]*calcabt[1]+calcabt[0]
        self.outputs[2].data=flagged_data(cbt.raw_data,cbt.times,np.zeros(cbt.raw_data.shape,dtype='uint8'))
        
        
        
