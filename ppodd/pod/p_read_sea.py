import os
import ppodd
from ppodd.core import file_read, timed_data, parameter, timestamp
from datetime import datetime, timedelta
import numpy as np
import pandas as pd

import pytz

#import pdb

class read_sea(file_read):
    """
    Class to read and parse .wcm text file produced by SEA SeaWcm software.

    The ascii file has lines starting with a sentence id. The frequencies of
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

    Attributes:
        dataset ()
    """

    # Dictionary of parser functions for each sentence type
    # Each top-level sub-dict key is the id string of the sentence
    parser_f = {}
    parser_f['d0'] = {'descr': 'Raw element power and temperature',
            'dtypes': ['S2']+['S3','f','f','f']*5,
            'names': ['id'] + \
                    [s1+s2 for s1 in ['TWC','083','021','CMP','DCE'] \
                           for s2 in ['','_A','_V','_T']],
            'long names': ['id'] + \
                    [s1+s2 for s1 in ['TWC','083','021','CMP','DCE'] \
                           for s2 in ['',' current',' voltage',' temperature']],
            'units': [''] + ['','amp','volt','deg C']*5,
            'converters': None}
    parser_f['d1'] = {'descr': 'Calculated total and liquid water contents',
            'dtypes': ['S2']+['S3','f']*3,
            'names': ['id'] + \
                     [s1+s2 for s1 in ['TWC','083','021'] \
                            for s2 in ['','_wc']],
            'long names': ['id','','total water content',
                           '','liquid water content','','liquid water content'],
            'units': ['','','g/m^3','','g/m^3','','g/m^3'],
            'converters': None}
    parser_f['d2'] = {'descr': 'Element status information',
           'dtypes': ['S2']+['S3','S6','i1','i1','i1','i1']*5,
           'names': ['id'] + \
                    [s1+s2 for s1 in ['TWC','083','021','CMP','DCE'] for \
                     s2 in ['','_status','_DAC','_pt','_it','_dt']],
            'long names': ['id'] + \
                    [s1+s2 for s1 in ['TWC','083','021','CMP','DCE'] for \
                     s2 in ['','_status','_DAC','_pt','_it','_dt']],
            'units': ['']+['']*30,
            'converters': None}
    parser_f['d3'] = {'descr': 'Aircraft parameters',
            'dtypes': ['S2','object','object','f','f','f','i1','f'],
            'names': ['id','date','time','tas','tstatic','pstatic',
                      'zerostate','powerboxtemp'],
            'long names': ['id','date','time','true air speed',
                           'static temperature','static pressure',
                           'request zero state','internal temperature power box'],
            'units': ['','','UTC','m/s','deg C','mb','','deg C'],
            # For some reason using names in converters does not work,
            # use col indicies instead
            # As SEA controller is sync'd to timeserver is in UTC
            'converters': {1: lambda x: datetime.strptime(x,'%Y/%m/%d').date(),
                           2: lambda x: datetime.strptime(x,'%H:%M:%S.%f').time().replace(tzinfo=pytz.utc)}}
    parser_f['c0'] = {'descr': 'Sense element information',
            'dtypes': ['S2','i2'] + ['S3','f','f','f','f','f']*3 + \
                      ['S3','f','f','f'],
            'names': ['id','s/n'] + \
                     [s1+s2 for s1 in ['TWC','083','021'] \
                            for s2 in ['','_l','_w','_f','_s','_o']] + \
                     ['CMP'+s2 for s2 in ['','_l','_w','_f']],
            'long names': ['id','s/n'] + \
                          [s1+s2 for s1 in ['TWC','083','021'] \
                                 for s2 in ['',' length',' width',
                                            ' fixture resistance',' slope correction',
                                            ' offset correction']] + \
                          ['CMP'+s2 for s2 in ['',' length',' width',' fixture resistance']],
            'units': [''] + ['','mm','mm','milliohm','','']*3 + \
                     ['','mm','mm','milliohm'],
            'converters': None}
    # Have not done the parser for this yet
    parser_f['cpbx'] = {'descr': 'Power box configuration',
            'dtypes': ['str'],
            'names': ['rowstr'],
            'long names': ['entire sentence'],
            'units': [''],
            'converters': None}
    # Have not done the parser for this yet
    parser_f['cprb'] = {'descr': 'Probe configuration',
            'dtypes': [],
            'names': ['id','s/n','chipid','rev'] + \
                     [s1+s2 for s1 in ['TWC','083','021','CMP','DCE'] \
                            for s2 in ['','_kp','_ki','_kd','_dacmin','_setpoint']] + \
                     [s1+s2 for s1 in ['TWC','083','021','CMP'] \
                            for s2 in ['_r100','_dtdr','_l','_w','_d','_shape','_f']] + \
                     [s1+s2 for s1 in ['TWC','083','021'] for s2 in ['_s','_o']] + \
                     [s1+s2 for s1 in ['caldate','calduedate'] \
                            for s2 in ['_month','_day','_year']],
            'long names': ['id','s/n','chip id','revision'] + \
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
                     ['','','','','','deg C']*5 + \
                     ['milliohm','deg C/milliohm','mm','mm','mm','','milliohm']*4 + \
                     ['','']*3 + ['','','']*2,
            'converters': None}
    # Have not done the parser for this yet
    parser_f['cmcb'] = {'descr': 'Main cable configuration',
            'dtypes': ['str'],
            'names': ['rowstr'],
            'long names': ['entire sentence'],
            'units': [''],
            'converters': None}
    # Have not done the parser for this yet
    parser_f['cscb'] = {'descr': 'Secondary cable configuration',
            'dtypes': ['str'],
            'names': ['rowstr'],
            'long names': ['entire sentence'],
            'units': [''],
            'converters': None}


    def __init__(self,dataset,rtn_all=False):
        """
        Args:
            dataset (?): ??
            rtn_all (Boolean): If False [Default] then return only those
                fields required for data processing. If True return all
                housekeeping and ancillary fields.

        """

        self.input_names=['SEA']
        self.outputs=[]
        self.patterns=('*.wcm',)
        self.rtn_all=rtn_all

        file_read.__init__(self,dataset)


    def readfile(self,filename):
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

        def timestamp_func(d3):
            """
            Create function to calculate interpolated time stamps

            The d3 sentence provides date and time stamps for that data
            line. Use linear interpolation based on the row number to
            produce a function to calculate datetime stamps for all data
            lines.

            Args:
                d3 (dict)
            """

            dt = np.array([datetime.combine(d,t) for (d,t) \
                        in zip(d3['data']['date'],d3['data']['time'])])

            # Convert to seconds to do fit
            delta_dt = [timedelta.total_seconds(dt_-dt[0]) for dt_ in dt]

            row_nums = d3['row']

            x = np.vstack([row_nums, np.ones(len(row_nums))]).T
            try:
                a,b = np.linalg.lstsq(x,delta_dt)[0]
            except LinAlgError:
                # Did not converge, probably not enough data points
                return None
            else:
                return lambda r_: dt[0] + timedelta(seconds=(a*r_ + b))


        ppodd.logger.info('Open SEA file {!s}'.format(filename))
#        dirname=os.path.dirname(filename)

        # Read the wcm file into raw_data as a 1D numpy array of strings
        with open(filename) as f:
            raw_data=np.genfromtxt(f,dtype='S')

        # Create a list of sentence ids
        mtype=np.core.defchararray.rjust(raw_data,2)

        if self.rtn_all is True:
            sentence_id = np.unique(mtype)
        else:
            # Only return d0 and d3 sentences are they are all that is
            # required for calculation of water content
            sentence_id = np.asarray(['d0','d3'])

        # Dictionary of raw and parsed data sentences
        # 'row' key is array of row numbers in file
        wcm = {'raw': {k: raw_data[(mtype==k)] for k in sentence_id},
               'parsed': {k: {'description': self.parser_f[k]['descr'],
                              'row': np.where(mtype==k)[0]} for k in sentence_id}}

        # Parse raw data sentences
        for k in sentence_id:
            wcm['parsed'][k]['data'] = np.genfromtxt(wcm['raw'][k],
                                       delimiter=',',
                                       dtype=zip(self.parser_f[k]['names'],
                                                 self.parser_f[k]['dtypes']),
                                       converters=self.parser_f[k]['converters'])

        # Determine interpolated time stamp function
        dt_func = timestamp_func(wcm['parsed']['d3'])

        if dt_func is None:
            ppodd.logger.info('SEA file too short for timestamp interpolation.')
            return None

        for k in wcm['parsed'].keys():
            wcm['parsed'][k]['dt'] = np.array([np.datetime64(dt_func(t_)) for \
                                               t_ in wcm['parsed'][k]['row']])
            
            # Set frequency for the data sentences
            if k == 'd3':
                 freq = 1
            elif k == 'd0':
                freq = 20
            else:
                continue

            wcm['parsed'][k]['f'] = freq
            ts_start = wcm['parsed'][k]['dt'][0]
            ts_end = wcm['parsed'][k]['dt'][-1]
            # create a new index for resampling the irregular data
            # This new Index starts on a full second and is the correct frequency
            newIndex = pd.date_range(start=np.datetime64(ts_start, 's'),
                                     end=np.datetime64(ts_end, 's'),
                                     freq='%ims' % (1000/freq,), closed='left')
            
            # Define outputs
            for i, name in enumerate(self.parser_f[k]['names']):
                series_ = pd.Series(wcm['parsed'][k]['data'][name],
                                    index=wcm['parsed'][k]['dt'])
                series = series_.reindex(index=newIndex, method='nearest')
                shape_ = (len(series)/freq, freq)
                data_ = np.reshape(series.values, shape_)
                            
                self.outputs.append(parameter('SEAPROBE_'+  name,
                               long_name=self.parser_f[k]['long names'][i],
                               units=self.parser_f[k]['units'][i],
                               frequency=wcm['parsed'][k]['f'],
                               data=timed_data(data_,
                                               timestamp(newIndex.values[::freq]))))
