"""
ppodd reader module for the SEAPROBE data stream ending with "*.wcm". The data
file is a csv file and contains a range of data sentences. The first column
is the data sentence identifier and the parsing has to be done accordingly.

"""

from datetime import datetime, timedelta
import numpy as np
import pandas as pd
import pytz
import sys

try:
    import ppodd
    from ppodd.core import file_read, timed_data, parameter, timestamp
except:
    pass

# Define time zone for data. If None then is timezone naive
# Note np.datetime64 is used and as of numpy 1.11 does not support timezones
tz = pytz.UTC

# Dictionary of parser functions for each sentence type
# Each top-level sub-dict key is the id string of the sentence
parser_f = {}
parser_f['d0'] = {'descr': 'Raw element power and temperature',
                  'dtypes': ['S2',]+['S3', 'float32', 'float32', 'float32']*5,
                  'names': ['id',] + \
                           [s1+s2 for s1 in ['TWC', '083', '021', 'CMP', 'DCE'] \
                                  for s2 in ['', '_V', '_A', '_T']],
                  'long_names': ['id',] + \
                                [s1+s2 for s1 in ['TWC', '083', '021', 'CMP', 'DCE'] \
                                       for s2 in ['', ' voltage', ' current', ' temperature']],
                  'units': ['',] + ['', 'volt','amp','deg C']*5,
                  'converters': None}
parser_f['d1'] = {'descr': 'Calculated total and liquid water contents',
                  'dtypes': ['S2',] + ['S3', 'float32']*3,
                  'names': ['id',] + \
                           [s1+s2 for s1 in ['TWC', '083', '021'] \
                                  for s2 in ['', '_wc']],
                  'long_names': ['id', '', 'total water content', '',
                                 'liquid water content', '', 'liquid water content'],
                  'units': ['', '', 'g/m^3', '', 'g/m^3', '', 'g/m^3'],
                  'converters': None}
parser_f['d2'] = {'descr': 'Element status information',
                  'dtypes': ['S2',]+['S3', 'S6', 'int', 'i1', 'i1', 'i1']*5,
                  'names': ['id',] + \
                           [s1+s2 for s1 in ['TWC', '083', '021', 'CMP', 'DCE'] \
                                  for s2 in ['', '_status', '_DAC', '_pt', '_it', '_dt']],
                  'long_names': ['id'] + \
                                [s1+s2 for s1 in ['TWC', '083', '021', 'CMP', 'DCE'] \
                                       for s2 in ['', '_status', '_DAC', '_pt', '_it', '_dt']],
                  'units': ['']+['']*30,
                  'converters': None}
parser_f['d3'] = {'descr': 'Aircraft parameters',
                  'dtypes': ['S2', 'object', 'object', 'float32', 'float32', 'float32', 'int', 'float32'],
                  'names': ['id', 'date', 'time', 'tas', 'tstatic', 'pstatic', 'zerostate','powerboxtemp'],
                  'long_names': ['id', 'date', 'time', 'true air speed',
                                 'static temperature', 'static pressure',
                                 'request zero state', 'internal temperature power box'],
                  'units': ['', '', 'UTC', 'm/s', 'deg C', 'mb', '', 'deg C'],
                   # For some reason using names in converters does not work,
                   # use col indicies instead
                   # As SEA controller is sync'd to timeserver is in UTC
                  'converters': {1: lambda x: datetime.strptime(x, '%Y/%m/%d').date(),
                                 2: lambda x: datetime.strptime(x, '%H:%M:%S.%f').time().replace(tzinfo=tz)}}
parser_f['c0'] = {'descr': 'Sense element information',
                  'dtypes': ['S2', 'S4'] + \
                            ['S3', 'float32', 'float32', 'float32', 'float32', 'float32']*3 + \
                            ['S3', 'float32', 'float32', 'float32'],
                  'names': ['id', 'sn'] + \
                           [s1+s2 for s1 in ['TWC', '083', '021'] \
                                  for s2 in ['','_l','_w','_f','_s','_o']] + \
                           ['CMP'+s2 for s2 in ['','_l','_w','_f']],
                  'long_names': ['id', 'serial number'] + \
                                [s1+s2 for s1 in ['TWC', '083', '021'] \
                                   for s2 in ['',' length',' width',
                                              ' fixture resistance', ' slope correction',
                                              ' offset correction']] + \
                               ['CMP'+s2 for s2 in ['',' length',' width',' fixture resistance']],
                 'units': ['', ''] + ['', 'mm', 'mm', 'milliohm', '', '']*3 + \
                          ['', 'mm', 'mm', 'milliohm'],
                 'converters': None}
parser_f['cpbx'] = {'descr': 'Power box configuration',
                    'dtypes': ['S4', 'S4', 'S16', 'S16', 'S16', 'S4', 'S4']+(['int', 'int']+['float32']*6)*5,
                    'names': ['id', 'sn', 'chipid', 'tid', 'endid', 'rev'] + \
                             [s1+s2 for s1 in ['ele1', 'ele2', 'ele3', 'cmp', 'dce']
                                    for s2 in ['state', 'vrawv', 'vrawi', 'shunt', 'maxamps', 'maxvolts', 'hardver', 'softver']],
                    'long_names': ['id', 'serial number', 'chip id', 'Temperature EPROM id', 'End EPROM id'] + \
                                  [s1+s2 for s1 in ['ele1', 'ele2', 'ele3', 'cmp', 'dce']
                                      for s2 in [' state of element',
                                                 ' Vraw-V value',
                                                 ' Vraw-I value',
                                                 ' shunt',
                                                 ' maximum amps',
                                                 ' maximum volts',
                                                 ' hardware Version',
                                                 ' software versionr']],
                    'units': ['',]*6 + ['', 'volt', 'amp', 'milliohm', 'amp', 'volt', '', '']*5,
                    'converters': None}
parser_f['cprb'] = {'descr': 'Probe configuration',
                    'dtypes': ['S4', 'S4', 'S16', 'S4'] + \
                              ['S3',]+['int',]*5+['float32',]*5 + \
                              (['S6', 'float32', 'float32', 'float32', 'S3'] + ['int']*5+['float32']*5)*2 + \
                              ['S4', 'float32', 'float32', 'float32'] + \
                              ['S3',] + ['int',]*5 + ['float32',] * 5 + \
                              ['S4', 'float32'] + \
                              ['S3',]+['int']*11,
                    'names': ['id','sn','chipid','rev'] + \
                             [s1+s2 for s1 in ['TWC','083','021','CMP','DCE'] \
                                    for s2 in ['','_kp','_ki','_kd','_dacmin','_setpoint']] + \
                             [s1+s2 for s1 in ['TWC','083','021','CMP'] \
                                    for s2 in ['_r100','_dtdr','_l','_w','_d','_shape','_f']] + \
                             [s1+s2 for s1 in ['TWC','083','021'] for s2 in ['_s','_o']] + \
                             [s1+s2 for s1 in ['caldate','calduedate'] \
                                    for s2 in ['_month','_day','_year']],
                    'long_names': ['id','serial number','chip id','revision'] + \
                                  [s1+s2 for s1 in ['TWC','083','021','CMP','DCE'] \
                                      for s2 in ['',' integral control loop gain',
                                                 ' differential control loop gain',
                                                 ' proportional control loop gain',
                                                 ' dac minimum',
                                                 ' setpoint temperature']] + \
                                  [s1+s2 for s1 in ['TWC','083','021','CMP'] \
                                      for s2 in [' r100 calibration parameter',
                                                 ' dtdr calibration parameter',
                                                 ' length',' width',' depth',
                                                 ' shape',' fixture resistance']] + \
                                  [s1+s2 for s1 in ['TWC','083','021'] \
                                         for s2 in [' slope correction',' offset correction']] + \
                                  [s1+s2 for s1 in ['calibration','calibration due'] \
                                         for s2 in [' month',' day',' year']],
                    'units': ['']*4 + \
                             ['', '', '', '', '', 'deg C']*5 + \
                             ['milliohm', 'deg C/milliohm', 'mm', 'mm', 'mm', '', 'milliohm']*4 + \
                             ['', '']*3 + ['', '', '']*2,
                    'converters': None}
parser_f['cmcb'] = {'descr': 'Main cable configuration',
                    'dtypes': ['S4', 'S5', 'S16', 'S4']+['int',]*4,
                    'names': ['id', 'sn', 'chipid', 'rev', 'cablelen', 'ele3res', 'cmpres', 'dceres'],
                    'long_names': ['id', 'serial number', 'chip id',
                                   'revision', 'cable length',
                                   'element 3 resistance',
                                   'compensation resistance',
                                   'deice resistance'],
                    'units': ['']*4 + ['ft', 'milliohm', 'milliohm', 'milliohm'],
                    'converters': None}
parser_f['cscb'] = {'descr': 'Secondary cable configuration',
                    'dtypes': ['S4', 'S5', 'S16', 'S4']+['int',]*3,
                    'names': ['id', 'sn', 'chipid', 'rev', 'cablelen', 'ele1res', 'ele2res'],
                    'long_names': ['id', 'serial number', 'chip id', '',
                                   'cable length',
                                   'element 1 resistance',
                                   'element 2 resistance'],
                    'units': ['']*4 + [ 'ft', 'milliohm', 'milliohm'],
                    'converters': None}


def timestamp_func(d3):
    """
    Create function to calculate interpolated time stamps

    The d3 sentence provides date and time stamps for that data
    line. Use linear interpolation based on the row number to
    produce a function to calculate datetime stamps for all data
    lines.

    :params d3:
    :type d3: dict
    """

    dt = np.array([(datetime.combine(d, t)).replace(tzinfo=None) for (d, t)
                       in zip(d3['data']['date'], d3['data']['time'])])

    # Convert to seconds to do fitting
    delta_dt = [timedelta.total_seconds(dt_-dt[0]) for dt_ in dt]

    row_nums = d3['row']

    x = np.vstack([row_nums, np.ones(len(row_nums))]).T
    try:
        a, b = np.linalg.lstsq(x, delta_dt)[0]
    except np.linalg.LinAlgError:
        # Did not converge, probably not enough data points
        return None
    else:
        return lambda r_: dt[0] + timedelta(seconds=(a*r_ + b))


def get_frequency(timestamp):
    """
    The frequency of the different data sentences of the SEADAS probe can
    be adjusted in the software. This small function estimates the most likely
    frequency setting using the timedelta of consecutive timestamps.

    :param timestamp: array of timestamps for every data line
    :type timestamp: numpy.array with numpy.datetime64 elements
    :returns: frequency
    :rtype: int
    """

    possible_freq_setting = [20, 10, 5, 2, 1]
    tolerance = 20  # in percent

    if timestamp.size < 3:
        return None

    _f = 1./np.median((timestamp[1:]-timestamp[:-1])).item().total_seconds()
    for pfs in possible_freq_setting:
        delta = abs(_f-float(pfs))
        if delta/pfs < float(tolerance)/100.:
            return pfs
    return None


def to_dataframe(ifile, rtn_all=False):
    """
    returns a dictionary where each item holds the data for a data sentence.

    :param ifile: input file
    :key rtn_all: set to `True` if all data sentences should be parsed
    :return: dictionary of pandas.DataFrame

    :Example:

      In [1]: ifile = 'seaprobe_20171214_090141_C072.wcm'

      In [2]: d = to_dataframe(ifile, rtn_all=True)

      In [3]: print(d.keys())
      ['c0', 'd2', 'd3', 'd0', 'd1']

      In [4]: d['d0'].head()

    """
    # Read the wcm txt file into raw_data as a 1D-numpy.array of strings
    with open(ifile) as f:
        raw_data = np.genfromtxt(f, dtype='S')

    # Create a list of sentence ids
    mtype = np.array([l.split(',')[0] for l in raw_data])

    if rtn_all is True:
        sentence_id = np.unique(mtype)
    else:
        # Only return d0, d3 and c0 by default
        # Those sentences are all that is
        # required for calculation of water content
        sentence_id = np.asarray(['d0', 'd3', 'c0'])

    # Dictionary of raw and parsed data sentences
    # 'row' key is array of row numbers in file
    wcm = {'raw': {k: raw_data[(mtype == k)] for k in sentence_id},
           'parsed': {k: {'description': parser_f[k]['descr'],
                          'row': np.where(mtype == k)[0]} for k in sentence_id}}

    # Parse raw data sentences
    for k in sentence_id:
        wcm['parsed'][k]['data'] = np.genfromtxt(wcm['raw'][k],
                                                 delimiter=',',
                                                 dtype=zip(parser_f[k]['names'],
                                                 parser_f[k]['dtypes']),
                                                 converters=parser_f[k]['converters'])

    # Determine interpolated time stamp function
    dt_func = timestamp_func(wcm['parsed']['d3'])

    if dt_func is None:
        msg = 'SEA file too short for timestamp interpolation.'
        if ppodd:
            ppodd.logger.info(msg)
        else:
            sys.stdout.write(msg+'\n')
        return None

    df_dic = {}
    for k in list(wcm['parsed'].keys()):

        #wcm['parsed'][k]['dt'] = np.array([np.datetime64(dt_func(t_).replace(tzinfo=tz)) for \
        #                                   t_ in wcm['parsed'][k]['row']])
        # Changed tzinfo due to Deprecation Warning
        wcm['parsed'][k]['dt'] = np.array([np.datetime64(dt_func(t_).replace(tzinfo=None)) for \
                                          t_ in wcm['parsed'][k]['row']])
        freq = get_frequency(wcm['parsed'][k]['dt'])

        wcm['parsed'][k]['f'] = freq
        ts_start = wcm['parsed'][k]['dt'][0]
        ts_end = wcm['parsed'][k]['dt'][-1]

        data_list = []
        for name in parser_f[k]['names']:
            data_list.append(wcm['parsed'][k]['data'][name])
        df = pd.DataFrame(np.column_stack(data_list),
                          columns=parser_f[k]['names'],
                          index=wcm['parsed'][k]['dt'])

        dd = dict(zip(parser_f[k]['names'], parser_f[k]['dtypes']))
        for name in parser_f[k]['names']:
            if 'float' in dd[name]:
                df[name] = df[name].astype(float)

        if freq:
            # create a new index for resampling the irregular data
            # This new Index starts on a full second and is the correct frequency
            newIndex = pd.date_range(start=np.datetime64(ts_start, 's'),
                                     end=np.datetime64(ts_end, 's'),
                                     freq='%ims' % (1000/freq,), closed='left')

            df = df.reindex(index=newIndex, method='nearest')

        df_dic[k] = df.copy()

    return df_dic


class read_sea(file_read):
    """
    Class to read and parse "*.wcm" text file produced by SEA SeaWcm software.

    The ASCII file has lines starting with a sentence id. The frequencies of
    these different id's are not the same and are set between 0 and 20Hz.
    by the instrument operator. The d3 line is fixed at 1Hz and is the
    only line with a date and time stamp. A linear interpolation is used
    to calculate time stamps for all other data lines based on their row
    number within the file compared to the row number of the d3 lines.
    If the file has too few d3 sentences for a successful interpolation
    no processed data is returned.

    Sentence descriptions for each id are;

    d0 row: raw data sentence. Structure described on pg44 of manual
        d0,ele,ele_voltage,ele_current,ele_temperature, repeated...

        where else is the element id string 'TWC','083','021','CMP', and 'DCE'
        'TWC' is the total water scoop
        '083' is the King probe LWC element
        '021' is the JW LWC element
        'CMP' is the compensation element
        'DCE' is the sensor deicing circuit

    d1 row: Calculated physical parameters sentence. Described on pg45
        d1,'TWC',value,'O83',value,'021',value
        Note that this depends on DECADES transmitting aircraft parameters
        otherwise is all zeroes. This is fixed at 1Hz.

    d2 row: Diagnostic sentence. Structure described on pg45
        d2,ele,status,dac,pt,it,dt, repeated...

    d3 row: Aircraft parameters sentence. Described on pg45
        d3,date,time,tas,sat,pstatic,zerostate,powerboxtemp
        This is fixed at 1Hz.

    """

    def __init__(self, dataset, rtn_all=False):
        """

        :param dataset: Input deacdes dataset
        :type dataset: ppodd.core.decades_dataset
        :param rtn_all: If False [Default] then return only those
            fields required for data processing. If True return all
            housekeeping and ancillary fields.
        :type rtn_all: boolean (default=False)
        :returns: dictionary with dictionary keys representing data
            sentence ids (d0, d1, d2, d3, c0, cscb, cpbx, cmcb, cprb)
        :rtype: dictionary
        """

        self.input_names = ['SEA']
        self.outputs = []
        self.patterns = ('SEAPROBE_*', '*.wcm')
        self.rtn_all = rtn_all
        file_read.__init__(self, dataset)

    def readfile(self, filename):
        """ require 2 parts the definition and the data
            may have several data files that need combining
            Assume they are all in one folder.
            Assume the name given is just
            folder/type
            eg.
            decades_data/Bxxx/AERACK01
            so any thing in folder/*type* is data
            and             folder/type_TCP*.csv is definition
        """

        if ppodd:
            ppodd.logger.info('Open SEA file {!s}'.format(filename))

        df_dic = to_dataframe(filename)

        # Define outputs
        for k in list(df_dic.keys()):
            df = df_dic[k]
            for i, name in enumerate(parser_f[k]['names']):
                freq = int(np.timedelta64(1, 's') / df_dic[k].index.freq.delta)
                if freq > 1:
                    shp = (df.shape[0]/freq, freq)
                    _data = np.reshape(df[name].values, shp)
                else:
                    _data = df[name].values
                self.outputs.append(parameter('SEAPROBE_' + name,
                                              long_name=parser_f[k]['long_names'][i],
                                              units=parser_f[k]['units'][i],
                                              frequency=freq,
                                              data=timed_data(_data,
                                                              timestamp(df.index.values[::freq]))))
