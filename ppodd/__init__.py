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


    def set_el_dim(self,el,l,w):
        """
        Set element ('TWC','083','021') dimensions; length and width
        """

        try:
            self.hdware_dict
        except NameError:
            self.hdware_dict = {}

        self.hdware_dict[el] = {'l': l, 'w': w}

    def get_el_dim(self,el):
        """
        Return element ('TWC','083','021') dimensions; length and width
        """

        try:
            return self.hdware_dict[el]
        except (NameError, KeyError):
            # Element hardware dictionary not created
            return None

