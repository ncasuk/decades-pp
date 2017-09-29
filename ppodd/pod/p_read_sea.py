from ppodd.core import *
import datetime as dt

class read_sea(file_read):
    """
    Routine for reading SEA WCM data
    """

    # Dictionary of parser functions for each sentence type
    # Each top-level sub-dict key is the id string of the sentence
    parser_f = {}
    parser_f['d0'] = {'descr': 'Raw element power and temperature',
            'dtypes': ['S2']+['S3','f','f','f']*5,
            'names': ['id'] + \
                   [s1+s2 for s1 in ['TWC','083','021','CMP','DCE'] for \
                    s2 in ['','_A','_V','_T']],
            'converters': None}
    parser_f['d1'] = {'descr': 'Calculated total and liquid water contents',
            'dtypes': ['S2']+['S3','f']*3,
            'names': ['id'] + [s1+s2 for s1 in ['TWC','083','021'] for \
                               s2 in ['','_wc']],
            'converters': None}
    parser_f['d2'] = {'descr': 'Element status information',
           'dtypes': ['S2']+['S3','S6','i1','i1','i1','i1']*5,
           'names': ['id'] + \
                    [s1+s2 for s1 in ['TWC','083','021','CMP','DCE'] for \
                     s2 in ['','_status','_DAC','_pt','_it','_dt']],
            'converters': None}
    parser_f['d3'] = {'descr': 'Aircraft parameters',
            'dtypes': ['S2','object','object','f','f','f','i1','f'],
            'names': ['id','date','time','tas','tstatic','pstatic',
                      'zerostate','powerboxtemp'],
          # For some reason using names in converters does not work,
          # use col indicies instead
            'converters': {1: lambda x: dt.datetime.strptime(x,'%Y/%m/%d').date(),
                           2: lambda x: dt.datetime.strptime(x,'%H:%M:%S.%f').time()}}
    parser_f['c0'] = {'descr': 'Sense element information',
            'dtypes': ['S2','i2'] + ['S3','f','f','f','f','f']*3 + \
                      ['S3','f','f','f'],
            'names': ['id','s/n'] + [s1+s2 for s1 in ['TWC','083','021'] for \
                                     s2 in ['','_l','_w','_f','_s','_o']] + \
                                    ['CMP'+s2 for s2 in ['','_l','_w','_f']],
          'converters': None}
    # Have not done the parser for this yet
    parser_f['cpbx'] = {'descr': 'Power box configuration',
            'dtypes': ['str'],
            'names': ['rowstr'],
            'converters': None}
    # Have not done the parser for this yet
    parser_f['cprb'] = {'descr': 'Probe configuration',
            'dtypes': ['str'],
            'names': ['rowstr'],
            'converters': None}
    # Have not done the parser for this yet
    parser_f['cmcb'] = {'descr': 'Main cable configuration',
            'dtypes': ['str'],
            'names': ['rowstr'],
            'converters': None}
    # Have not done the parser for this yet
    parser_f['cscb'] = {'descr': 'Secondary cable configuration',
            'dtypes': ['str'],
            'names': ['rowstr'],
            'converters': None}


    def __init__(self,dataset):

        # What are input_names? Are these nc short_names?
        # Or are these some sort of filename
        self.input_names=['SEA']
        self.outputs=[]
        self.patterns=('*.wcm',)

        file_read.__init__(self,dataset)


