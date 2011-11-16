from cal_base import *
class c_ins1(fort_cal):
    def __init__(self,dataset):
        self.input_names=['INSLEVL', 'IN01', 'TTAG', 'VXVY', 'VZTH', 'RORR', 'PIPR', 'PAYR']
        self.outputs=[parameter('IACF',units='M S-2',frequency=1,number=538,description='INS ACCELN FORWARD')
                     ,parameter('IACS',units='M S-2',frequency=1,number=539,description='INS ACCELN STARBOARD')
                     ,parameter('IACU',units='M S-2',frequency=1,number=540,description='INS ACCELN UP')
                     ,parameter('ILAT',units='DEGREES',frequency=1,number=541,description='INS UNCORR LATITUDE')
                     ,parameter('ILNG',units='DEGREES',frequency=1,number=542,description='INS UNCORR LONGITUDE')
                     ,parameter('IALT',units='METRES',frequency=1,number=543,description='INS UNCORR ALTITUDE')
                     ,parameter('VZ',units='M S-1',frequency=32,number=557,description='INS VERTICAL VEL')
                     ,parameter('VN',units='M S-1',frequency=32,number=558,description='INS VELOCITY NORTH')
                     ,parameter('VE',units='M S-1',frequency=32,number=559,description='INS VELOCITY EAST')
                     ,parameter('ROLL',units='DEGREES',frequency=32,number=560,description='INS ROLL')
                     ,parameter('PTCH',units='DEGREES',frequency=32,number=561,description='INS PITCH')
                     ,parameter('IHDG',units='DEGREES',frequency=32,number=562,description='INS HEADING')
                     ,parameter('IGS',units='M S-1',frequency=32,number=563,description='INS GROUND SPEED')
                     ,parameter('IDA',units='DEGREES',frequency=32,number=564,description='INS DRIFT ANGLE')
                     ,parameter('PITR',units='DEG S-1',frequency=32,number=565,description='INS PITCH RATE')
                     ,parameter('YAWR',units='DEG S-1',frequency=32,number=566,description='INS YAW RATE')
                     ,parameter('ROLR',units='DEG S-1',frequency=32,number=567,description='INS ROLL RATE')]
        self.name='INS1'
        self.version=1.00
        fort_cal.__init__(self,dataset)
