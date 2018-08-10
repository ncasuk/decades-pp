"""
**ppodd** has been written as the standard post processing for core data from
the `FAAM <http://www.faam.ac.uk/>`_ BAe146 recorded with the
`DECADES <http://www.faam.ac.uk/index.php/new-projects/196-projectspace/450-integration-decades>`_
data acquisition system, installed from October 2013. It is also compatible
with the majority of data recorded with the FAAM aircraft since 2004, but not
every legacy data type is supported (yet).

There is a programming interface, a command line (decades-pp), and a graphical
user interface (PPODD).

It is intended to be a flexible and modular system.

"""

import logging
import os.path
import time
import warnings

import pandas as pd
import numpy as np

import scipy.stats

logging.basicConfig()

logger = logging.getLogger('PPODD')
logger.setLevel(logging.DEBUG)
version = '1.1'

_pd_freqs = {
    'N': 10**-9,
    'U': 10**-6,
    'L': 10**-3,
    'S': 10**0,
    'T': 60,
    'H': 3600
}

def logfile(filename=None):
    if(not filename):
        rouops = '$ROUOPS'
        if os.path.expandvars(rouops) != rouops:
            rouops = os.path.expandvars(rouops)
        else:
            rouops = ''

        filename = os.path.join(
            rouops, time.strftime('ppodd_log_%Y%m%d_%H%M%S.txt')
        )

    try:
        filelog = logging.FileHandler(filename)
        filelog.setLevel(logger.level)
        logger.addHandler(filelog)
    except IOError:
        logger.warning('Cannot write to %s' % filename)


@pd.api.extensions.register_dataframe_accessor('ppodd')
class PpoddUtils(object):
    """
    This class is registered as a DataFrame accessor
    to allow metadata and utility functions to be attached
    to DataFrames.
    """

    def __init__(self, _pandas):
        """
        Initializer.

        Args:
            _pandas: the pandas DataFrame to be associated with this
                instance.
        """
        self._obj = _pandas
        self._freq = None
        self._freq_conf = None

        self._sea_meta = None

    def _guess_freq(self):
        """
        'Guess' the frequency of the DataFrame index, by looking at the modal
        value of the finite difference array of the index.

        Returns:
            (freqstr, conf): the pandas frequency string and the confidence
                that we have that the data is, in fact, at this frequency.
        """

        _diff = np.diff(self._obj.index)

        # scipy tends to issue a warning about ignoring nans. This probably
        # isn't very helpful in this context.
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            _mode = scipy.stats.mode(_diff)

        # Build the format string in Nanos
        freqstr = '{}N'.format(
            int(_mode[0] / np.timedelta64(1, 'ns'))
        )

        # Confidence of the frequency guess
        conf = (_mode[1] / len(_diff))[0]

        return freqstr, conf

    @property
    def freq(self):
        """
        Get the frequency of the DataFrame Index. Initially attempt
        pd.infer_freq, if that fails use PpoddUtils._guess_freq.

        Also sets the confidence in the frequency inference, which is 1
        if pd.infer_freq is successful.

        Returns:
            _freq: The inferred frequency of the attached DataFrame Index.
        """
        _freq = pd.infer_freq(self._obj.index)
        if _freq is None:
            _freq, _conf = self._guess_freq()
        else:
            _conf = 1

        self._freq = _freq
        self._freq_conf = _conf

        return _freq

    @property
    def freq_hz(self):
        """
        Return the frequency of the attached DataFrame Index, by parsing the
        freqstr inferred from the Index.

        Returns:
            the inferred frequency of the instance DataFrame Index, in Hz.
        """
        _num_part = self.freq[:-1] or 1
        _name_part = self._freq[-1]

        return (int(_num_part) * _pd_freqs[_name_part])**-1

    @property
    def freq_conf(self):
        """
        Return the confidence of the frequency inferrence.
        """
        return self._freq_conf


    def set_sea_meta(self,el,param):
        """
        Set SEA WCM-2000 metadata. Probe metadata should be stored
        in an el subdict 'sea', while element metadata should be stored
        in an el subdict 'TWC','083','021', or 'CMP'). param is a
        dictionary of items to be added to _sea_meta[el].

        Note that due to the expanded kwarg, it is up to the user to
        monitor key consistency in the dictionary _sea_meta.
        """

        if self._sea_meta == None:
            self._sea_meta = {}

        if el not in self._sea_meta:
            self._sea_meta[el] = {}

        self._sea_meta[el].update(param)


    def get_sea_meta(self,el,param=None):
        """
        Return element parameter. If param is None then all parameters
        for element el are returned as a dictionary. More than one
        parameter as a dictionary shall be returned if param is list-like
        """

        try:
            self._sea_meta[el]
        except (NameError, AttributeError, KeyError):
            # Element hardware dictionary not created
            return None

        if param is None:
            return self._sea_meta[el]
        elif type(param) is str:
            param = [param]

        p = {k_:v_ for k_,v_ in self._sea_meta[el].items() if k_ in param}

        if len(p.keys()) == 0:
            return None
        elif len(param) == 1:
            # If only a single param requested then return value
            return p[param[0]]
        else:
            # Return dictionary of params
            return p


    def sea_meta(self,meta=None):
        """
        If meta is None [default] then return metadata dictionary
        If meta dictionary is given then overwirte any existing metadata
        Return entire SEA metadata dictionary
        """

        if meta == None:
            return self._sea_meta
        else:
            # The nested dictionaries are necessary to remove any empty values
            self._sea_meta = \
                {_k:{__k:__v for __k,__v in _v.items() if __v not in ['',np.nan]} \
                 for _k,_v in meta.items()}


