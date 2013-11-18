from ppodd.core import *
from ppodd.pod.p_wvssiia import wvssii
from scipy.interpolate import interp1d

class wvssiib(wvssii):
    def __init__(self,dataset):
        wvssii.__init__(self,dataset,'WVSS2B')

