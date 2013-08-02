from ppodd.cal_base import *
class c_ins1(fort_cal):
    def __init__(self,dataset):
        self.input_names=['INSLEVL', 'IN01', 'TTAG', 'VXVY', 'VZTH', 'RORR', 'PIPR', 'PAYR']
        self.outputs=[parameter('IACF',units='M S-2',frequency=1,number=538,long_name='INS ACCELN FORWARD')
                     ,parameter('IACS',units='M S-2',frequency=1,number=539,long_name='INS ACCELN STARBOARD')
                     ,parameter('IACU',units='M S-2',frequency=1,number=540,long_name='INS ACCELN UP')
                     ,parameter('ILAT',units='DEGREES',frequency=1,number=541,long_name='INS UNCORR LATITUDE')
                     ,parameter('ILNG',units='DEGREES',frequency=1,number=542,long_name='INS UNCORR LONGITUDE')
                     ,parameter('IALT',units='METRES',frequency=1,number=543,long_name='INS UNCORR ALTITUDE')
                     ,parameter('VZ',units='M S-1',frequency=32,number=557,long_name='INS VERTICAL VEL')
                     ,parameter('VN',units='M S-1',frequency=32,number=558,long_name='INS VELOCITY NORTH')
                     ,parameter('VE',units='M S-1',frequency=32,number=559,long_name='INS VELOCITY EAST')
                     ,parameter('ROLL',units='DEGREES',frequency=32,number=560,long_name='INS ROLL')
                     ,parameter('PTCH',units='DEGREES',frequency=32,number=561,long_name='INS PITCH')
                     ,parameter('IHDG',units='DEGREES',frequency=32,number=562,long_name='INS HEADING')
                     ,parameter('IGS',units='M S-1',frequency=32,number=563,long_name='INS GROUND SPEED')
                     ,parameter('IDA',units='DEGREES',frequency=32,number=564,long_name='INS DRIFT ANGLE')
                     ,parameter('PITR',units='DEG S-1',frequency=32,number=565,long_name='INS PITCH RATE')
                     ,parameter('YAWR',units='DEG S-1',frequency=32,number=566,long_name='INS YAW RATE')
                     ,parameter('ROLR',units='DEG S-1',frequency=32,number=567,long_name='INS ROLL RATE')]
        self.name='INS1'
        self.version=1.00
        fort_cal.__init__(self,dataset)
