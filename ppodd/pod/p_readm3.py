#from ppodd.pod import *
from ppodd.core import *
import struct
import os
import datetime

mfdpardesc="""MFD parameter descriptors list                         04-AUG-03  MFDPARDESC.DAT
______________________________

Param  Freq  Name  Units     Full name

    1        FLNO  DRS       FLIGHT NUMBER       
    2        GMTH  DRS       FLIGHT GMT(HH+M)    
    3        GMTM  DRS       FLIGHT GMT(M+SS)    
    4        EVM   DRS       EVENT MARK          
    5        DRSS  DRS       DRS STATUS          
   10        DITM  DRS       DE-ICED TEMP        
   14        CABP  DRS       CABIN PRESSURE      
   23        NDTM  DRS       NON-DEICED TEMP     
   27        SREG  DRS       SIGNAL REGISTER     
   37        RDHT  DRS       RADAR ALTIMETER     
   42        J/W   DRS       JOHNSON WILLIAMS    
   50        CNC   DRS       CNC CONCENTRATION   
   58        HYGR  DRS       GEN EAST DEW POINT  
   59        HYCS  DRS       GEN EAST CNTL COND  
   70        TWCD  DRS       TWC DETECTOR        
   71        TNOS  DRS       TWC NOSE TEMP       
   72        TSAM  DRS       TWC SAMPLE TEMP     
   73        TAMB  DRS       TWC AMBIENT TEMP    
   74        TSRC  DRS       TWC SOURCE TEMP     
   75        HTR1  DRS       TWC EVAP1 CURRNT    
   76        HTR2  DRS       TWC EVAP2 CURRNT    
   77        ISRC  DRS       TWC SRCE CURRNT     
   78        STAT  DRS       TWC STATUS WORD     
   81        UPOS  DRS       UPP VIS CLR SIG     
   82        UPIS  DRS       UPP VIS RED SIG     
   83        USIS  DRS       UPP I/R SIGNAL      
   84        UPOZ  DRS       UPP VIS CLR ZERO    
   85        UPIZ  DRS       UPP VIS RED ZERO    
   86        USIZ  DRS       UPP I/R ZERO        
   87        UPOT  DRS       UPP VIS CLR TEMP    
   88        UPIT  DRS       UPP VIS RED TEMP    
   89        USIT  DRS       UPP I/R TEMP        
   91        LS1S  DRS       LWR VIS CLR SIG     
   92        LS2S  DRS       LWR VIS RED SIG     
   93        LS3S  DRS       LWR I/R SIGNAL      
   94        LS1Z  DRS       LWR VIS CLR ZERO    
   95        LS2Z  DRS       LWR VIS RED ZERO    
   96        LS3Z  DRS       LWR I/R ZERO        
   97        LS1T  DRS       LWR VIS CLR TEMP    
   98        LS2T  DRS       LWR VIS RED TEMP    
   99        LS3T  DRS       LWR I/R TEMP        
  100        O3    DRS       OZONE SIGNAL        
  106        O3P   DRS       O3 PRESSURE         
  113        O3T   DRS       O3 TEMPERATURE      
  114        O3F   DRS       O3 FLOW             
  140        CCN   DRS       CLOUD COND NUC CNT  
  141        HEIM  DRS       HEIM TARGET TEMP    
  142        HEIC  DRS       HEIM CALIB TEMP     
  150        HCHO  DRS       FORMALDEHYDE        
  151        ORGP  DRS       ORGANIC PEROXIDE    
  152        H2O2  DRS       HYDROGEN PEROXIDE   
  154        CO    DRS       CARBON MONOXIDE     
  157        PRC1  DRS       PERCA1              
  158        PRC2  DRS       PERCA2              
  163        IN01  DRS       INS I01 MESSAGE     
  164        TTAG  DRS       INS TIME TAG        
  165        VXVY  DRS       INS FINE VX/VY      
  166        VZTH  DRS       INS FINE VZ/TH      
  167        RORR  DRS       INS FINE RO/RR      
  168        PIPR  DRS       INS FINE PI/PR      
  169        PAYR  DRS       INS FINE PA/YR      
  170        EV1V  DRS       TWC EVAP 1 VOLTS    
  171        EV2V  DRS       TWC EVAP 2 VOLTS    
  172        NPWR  DRS       TWC NOSE HEATER PWR 
  173        EV1C  DRS       TWC EVAP 1 CURR O/S 
  174        EV2C  DRS       TWC EVAP 2 CURR O/S 
  175        NPRS  DRS       NEPH PRESSURE       
  176        NTMP  DRS       NEPH TEMPERATURE    
  177        NBTS  DRS       NEPH BLUE SP        
  178        NGTS  DRS       NEPH GREEN SP       
  179        NRTS  DRS       NEPH RED SP         
  180        NBBS  DRS       NEPH BLUE BSP       
  181        NRBS  DRS       NEPH RED BSP        
  182        NGBS  DRS       NEPH GREEN BSP      
  183        NHUM  DRS       NEPH HUMIDITY       
  184        NSTS  DRS       NEPH STATUS         
  185        PLIN  DRS       PSAP LIN ABS COEFF  
  186        PLOG  DRS       PSAP LOG ABS COEFF  
  187        PTRN  DRS       PSAP TRANSMITTANCE  
  188        PFLO  DRS       PSAP FLOW RATE      
  195        F1FL  DRS       FILTER 1 FLOW       
  196        F1PR  DRS       FILTER 1 PRESSURE   
  197        F2FL  DRS       FILTER 2 FLOW       
  198        F2PR  DRS       FILTER 2 PRESSURE   
  199        NO    DRS       NOXY NO             
  200        NO2   DRS       NOXY NO2            
  201        NOY1  DRS       NOXY NOY1           
  202        NOY2  DRS       NOXY NOY2           
  203        NOX1  DRS       NOX CHANNEL 1       
  204        NOX2  DRS       NOX CHANNEL 2       
  205        NOX3  DRS       NOX CHANNEL 3       
  207        CABT  DRS       CABIN TEMPERATURE   
  208        NVLW  DRS       NEVZ LIQUID WATER   
  209        NVLR  DRS       NEVZ LIQUID REFERENC
  210        NVLC  DRS       NEVZ LIQUID COLLECTO
  211        NVTW  DRS       NEVZ TOTAL WATER    
  212        NVTR  DRS       NEVZ TOTAL REFERENCE
  213        NVTC  DRS       NEVZ TOTAL COLLECTOR
  214        SO2   DRS       TECO SO2            
  215        TBP1  DRS       TURB PROBE PITOT    
  216        TBP2  DRS       TURB PROBE ATTACK   
  217        TBP3  DRS       TURB PROBE SIDESLIP 
  218        TBP4  DRS       TURB PROBE AOA CHECK
  219        TBP5  DRS       TURB PROBE AOSS CHEC
  220        TBP6  DRS       Not Used            
  221        S9SP  DRS       S9 STATIC PRESSURE  
  222        RVAL  DRS       RVSM ALTITUDE       
  223        RVAS  DRS       RVSM AIRSPEED       
  224        USOS  DRS       UPP STBD OUT SIG    
  225        USOZ  DRS       UPP STBD OUT ZERO   
  226        USOT  DRS       UPP STBD OUT TEMP   
  227        LS4S  DRS       LWR STBD AFT SIG    
  228        LS4Z  DRS       LWR STBD AFT ZERO   
  229        LS4T  DRS       LWR STBD AFT TEMP   
  230        FWVS  DRS       FWVS MK II
  515     1  SECS  SECS      SECS FROM MIDNIGHT  
  516    32  IAS   M S-1     IND. AIR SPEED      
  517    32  TAS   M S-1     TRUE AIR SPEED      
  519    32  ITDI  DEG K     DEICED IND TEMP     
  520    32  TTDI  DEG K     DEICED TRUE TEMP    
  522     4  IATN  DEG K     NONDEICED IND TEMP  
  523     4  TTND  DEG K     NONDEICED TRUE TEMP 
  524    32  NDTI  DEG K     NONDEICED IND TEMP  
  525    32  NDTT  DEG K     NONDEICED TRUE TEMP 
  526     4  REFR  RNUM      REFRACTIVE INDEX    
  529     4  DEWP  DEG K     DEW POINT           
  530     2  MHDP  DEG K     MAN HYGRO DEW POINT 
  535     4  LWC   G KG-1    J/W LWC             
  536     1  NEFT  10-4M-1   TOTAL SCATTER COEFF 
  537     4  HBT   DEG K     HEIMANN BRIGHT TEMP 
  538     1  IACF  M S-2     INS ACCELN FORWARD  
  539     1  IACS  M S-2     INS ACCELN STARBOARD
  540     1  IACU  M S-2     INS ACCELN UP       
  541     1  ILAT  DEGREES   INS UNCORR LATITUDE 
  542     1  ILNG  DEGREES   INS UNCORR LONGITUDE
  543     1  IALT  METRES    INS UNCORR ALTITUDE 
  544    32  ICRD  RBITS     IN CLOUD RADIANCE   
  545     4  MHDG  DEGREES   MAG COMPASS HEADING 
  546     2  DGS   M S-1     DOPPLER GROUNDSPEED 
  547     2  DDA   DEGREES   DOPPLER DRIFT ANGLE 
  548    32  AOA   DEGREES   ANGLE OF ATTACK     
  549    32  AOSS  DEGREES   ANGLE OF SIDESLIP   
  550     1  OLAT  DEGREES   OMEGA LATITUDE      
  551     1  OLNG  DEGREES   OMEGA LONGITUDE     
  552     1  OGS   M S-1     OMEGA GROUNDSPEED   
  553     1  OHDG  DEGREES   OMEGA HEADING       
  554     1  OWS   M S-1     OMEGA WINDSPEED     
  555     1  OWA   DEGREES   OMEGA WIND ANGLE    
  556     1  ODRA  DEGREES   OMEGA DRIFT ANGLE   
  557    32  VZ    M S-1     INS VERTICAL VEL    
  558    32  VN    M S-1     INS VELOCITY NORTH  
  559    32  VE    M S-1     INS VELOCITY EAST   
  560    32  ROLL  DEGREES   INS ROLL            
  561    32  PTCH  DEGREES   INS PITCH           
  562    32  IHDG  DEGREES   INS HEADING         
  563    32  IGS   M S-1     INS GROUND SPEED    
  564    32  IDA   DEGREES   INS DRIFT ANGLE     
  565    32  PITR  DEG S-1   INS PITCH RATE      
  566    32  YAWR  DEG S-1   INS YAW RATE        
  567    32  ROLR  DEG S-1   INS ROLL RATE       
  568     1  CNC   CC-1      CNC CONCENTRATION   
  569     8  FLMR            FWVS VOL MIX RATIO  
  570     8  LFMR            FWVS LONG PATH VMR  
  571     8  SFMR            FWVS SHORT PATH VMR 
  572    64  TWC   G KG-1    TOTAL WATER CONTENT 
  573     1  FLDP  DEG K     FWVS DEW POINT      
  574     1  OZMR  PPB       OZONE MIXING RATIO  
  575     2  RHGT  METRES    RADAR HEIGHT        
  576    32  SPR   MB        STATIC PRESSURE     
  577    32  PSP   MB        PITOT-STATIC PRESSUR
  578    32  PHGT  METRES    PRESSURE HEIGHT     
  579     1  CABP  MB        CABIN PRESSURE      
  580     1  GLAT  DEGREES   GPS LATITUDE        
  581     1  GLNG  DEGREES   GPS LONGITUDE       
  582     1  GALT  METRES    GPS ALTITUDE        
  583     1  GVN   M S-1     GPS VELOCITY NORTH  
  584     1  GVE   M S-1     GPS VELOCITY EAST   
  585     1  GVZ   M S-1     GPS VELOCITY UP     
  586     8  SARA  W M-2 -   SAFIRE DET A RADIANC
  587     8  SBRA  W M-2 -   SAFIRE DET B RADIANC
  588     8  SCRA  W M-2 -   SAFIRE DET C RADIANC
  589     8  SDRA  W M-2 -   SAFIRE DET D RADIANC
  590     8  SABT  DEG K     SAFIRE DET A BRT TMP
  591     8  SBBT  DEG K     SAFIRE DET B BRT TMP
  592     8  SCBT  DEG K     SAFIRE DET C BRT TMP
  593     8  SDBT  DEG K     SAFIRE DET D BRT TMP
  594     8  SFP             SAFIRE FILTER POSN  
  595     8  SMP             SAFIRE MIRROR POSN  
  596     1  SHBB  DEG K     SAFIRE HOT BB TEMP  
  597     1  SCBB  DEG K     SAFIRE COLD BB TEMP 
  598     1  SRBB  DEG K     SAFIRE REF BB TEMP  
  599     1  SIP   DEG       SAFIRE INST PITCH   
  600     1  SIR   DEG       SAFIRE INST ROLL    
  601     1  SHP             SAFIRE SHUTTER POSN 
  602     8  NVLW  G M-3     NEVZOROV LIQUID WATE
  603     8  NVLR  V         NEVZOROV LIQUID REFE
  604     8  NVLC  V         NEVZOROV LIQUID COLL
  605     8  NVTW  G M-3     NEVZOROV TOTAL WATER
  606     8  NVTR  V         NEVZOROV TOTAL REFER
  607     8  NVTC  V         NEVZOROV TOTAL COLLE
  610    32  GLat  m s-1     GIN Latitude
  611    32  GLon  m s-1     GIN Longitude
  612    32  GAlt  m s-1     GIN Altitude
  613    32  GINN  deg       GIN N velocity
  614    32  GINE  deg       GIN E velocity
  615    32  GIND  deg       GIN D velocity
  616    32  Grol  deg       GIN Aircraft roll
  617    32  Gpit  deg       GIN Aircraft pitch
  618    32  Ghdg  m s-1     GIN Aircraft heading
  619    32  Gwan  deg s-1   GIN wander angle
  620    32  Gtrk  deg s-1   GIN track angle
  621    32  Gspd  deg s-1   GIN Aircraft speed
  622    32  Garl  m s-2     GIN ang rate long
  623    32  Gart  m s-2     GIN ang rate trans
  624    32  Gard  m s-2     GIN ang rate down
  625    32  Gacl  s         GIN accel long
  641     2  SREG  IBITS     SIGNAL REGISTER     
  642     1  SAZI  DEGREES   SOLAR AZIMUTH       
  643     1  SZEN  DEGREES   SOLAR ZENITH ANGLE  
  644     2  EVM   RNUM      EVENT MARK          
  645     1  REF+  RBITS     REFERENCE VOLTS +VE 
  646     1  REF0  RBITS     REFERENCE VOLTS ZERO
  647     1  REF-  RBITS     REFERENCE VOLTS -VE 
  648     1  PLIN            PSAP LIN ABS COEFF  
  649     1  PLOG            PSAP LOG ABS COEFF  
  650     4  PUR   ZONES     DECCA PURPLE        
  651     4  RED   ZONES     DECCA RED           
  652     4  GRN   ZONES     DECCA GREEN         
  655     1  GCHK  IBITS     ECGC HOUSEKEEPING   
  656     4  HOLO  IBITS     HOLOGRAPHY          
  657     1  NEFA  10-4M-1   AERO SCATT COEFF    
  658    32  CICT  DEG K     ICTP TRUE TEMP      
  659     8  ICHK  RBITS     ICTP HOUSEKEEPING   
  660     1  CABT  DEG C     CABIN TEMPERATURE   
  662     1  INST  RBITS     INS STATUS          
  664    64  LYMN  RBITS     TWC DETECTOR        
  665     1  TNOS  DEG K     TWC NOSE TEMP       
  666     1  TSAM  DEG K     TWC SAMPLE TEMP     
  667     1  TAMB  DEG K     TWC AMBIENT TEMP    
  668     1  TSRC  DEG K     TWC SOURCE TEMP     
  669     1  HTR1  AMPS      TWC EVAP1 CURRENT   
  670     1  HTR2  AMPS      TWC EVAP2 CURRENT   
  671     1  ISRC  AMPS      TWC SOURCE CURRENT  
  672     1  STAT  RBITS     TWC STATUS WORD     
  673     1  UP1S  W M-2     UPP VIS CLR SIG     
  674     1  UP2S  W M-2     UPP VIS RED SIG     
  675     1  UIRS  W M-2     UPP I/R SIGNAL      
  676     1  UP1Z  W M-2     UPP VIS CLR ZERO    
  677     1  UP2Z  W M-2     UPP VIS RED ZERO    
  678     1  UIRZ  W M-2     UPP I/R ZERO        
  679     1  UP1T  DEG C     UPP VIS CLR TEMP    
  680     1  UP2T  DEG C     UPP VIS RED TEMP    
  681     1  UIRT  DEG C     UPP I/R TEMP        
  682     1  LP1S  WM-2      LWR VIS CLR SIG     
  683     1  LP2S  WM-2      LWR VIS RED SIG     
  684     1  LIRS  WM-2      LWR I/R SIGNAL      
  685     1  LP1Z  WM-2      LWR VIS CLR ZERO    
  686     1  LP2Z  WM-2      LWR VIS RED ZERO    
  687     1  LIRZ  WM-2      LWR I/R ZERO        
  688     1  LP1T  DEG C     LWR VIS CLR TEMP    
  689     1  LP2T  DEG C     LWR VIS RED TEMP    
  690     1  LIRT  DEG C     LWR I/R TEMP        
  691     1  O3    PPB       OZONE (NO P/T CORR)               
  692     1  O3P   MB        O3 PRESSURE         
  693     1  O3T   K         O3 TEMP            
  694     8  FCNT  IBITS     FLUORESC.  COUNT    
  695     8  LINT  IBITS     L-A INTENSITY       
  696     8  PSIG  IBITS     SHORT PATH SIG      
  697     1  PREF  IBITS     SHORT PATH REF      
  698     1  FLOA  L/MIN     FLOW                
  699     1  FLOB  DEG C     FLOW METER TEMP     
  700     1  FPRS  MB        PRESSURE            
  701     1  FTMP  DEG C     FLUOR CH THERM      
  702     1  PTMP  DEG C     SHORT PATH THERM    
  703     1  HSIG  IBITS     H ALPHA SIGNAL      
  704     1  FTIM  MINS      FL TUBE RUN TIME    
  705     1  PTIM  MINS      SH PATH RUN TIME    
  706     1  FDEW  DEG K     CAL DEW/FROST PT    
  707     1  FSTA  IBITS     STATUS WORD         
  708     5  MCRA  IBITS     MCR CHANNEL A       
  709     5  MCRB  IBITS     MCR CHANNEL B       
  710     5  MCRC  IBITS     MCR CHANNEL C       
  711     5  MCRD  IBITS     MCR CHANNEL D       
  712     5  MCAL  IBITS     MCR CALIBRATION     
  713     5  MHKP  IBITS     MCR HOUSEKEEPING    
  714    32  V     M S-1     NORTHWARD WIND COMPT
  715    32  U     M S-1     EASTWARD WIND COMPT 
  716    32  W     M S-1     VERTICAL WIND COMPT
  717    16  MAHK            MARSS HOUSEKEEPING  
  718    16  MAPS            MARSS POSITION      
  719    16  M89             MARSS 89 GHz RAW    
  720    16  M157            MARSS 157 GHz RAW   
  723     4  CHBT  DEG K     CORR HEIM BRGHT TEMP
  724     4  CLWC  G KG-1    CORR LIQUID WATER   
  725    64  TWDP  DEG K     TOTAL WATER DEW POIN
  726     1  GCS1  IBITS     ECGC SIGNAL 1       
  727     1  GCS2  IBITS     ECGC SIGNAL 2       
  728     1  GCS3  IBITS     ECGC SIGNAL 3       
  729     2  RHTC  METRES    CORR RADAR HEIGHT   
  730     1  CLAT  DEGREES   CORRECTED LATITUDE  
  731     1  CLNG  DEGREES   CORRECTED LONGITUDE 
  732     1  CALT  DEGREES   CORRECTED ALTITUDE  
  735    32  CVN   M S-1     CORRECTED NORTH VEL 
  736    32  CVE   M S-1     CORRECTED EAST VEL  
  737    32  CVZ   M S-1     CORRECTED VERT VEL  
  740     1  SOMR  PPB       SO2 MIXING RATIO    
  741     1  NO    RBITS     NO                  
  742     1  NOX   RBITS     NOX                 
  743     1  SO2P  RBITS     SO2 PRESSURE        
  744     1  NOXP  RBITS     NOX PRESSURE        
  745     4  CRIN  RNUM      CORR REFRACT INDEX  
  746     4  RIDP  DEG K     REFRACT INDEX DEW PO
  747    32  RAL   VOLTS     RAL 400MHZ SIGNAL   
  760     1  NPRS  MB        NEPH PRESSURE       
  761     1  NTMP  DEG K     NEPH TEMPERATURE    
  762     1  NBTS  M-1       NEPH BLUE SP        
  763     1  NGTS  M-1       NEPH GREEN SP       
  764     1  NRTS  M-1       NEPH RED SP         
  765     1  NBBS  M-1       NEPH BLUE BSP       
  766     1  NGBS  M-1       NEPH GREEN BSP      
  767     1  NRBS  M-1       NEPH RED BSP        
  768     1  NHUM  %         NEPH HUMIDITY       
  769     1  NSTS  RBITS     NEPH STATUS         
  770     1  NO    PPB       TECO 42 NO          
  771     1  NO2   PPB       TECO 42 NO2         
  772     1  NOx   PPB       TECO 42 NOx         
  773    32  TBP0  MB        TURB PROBE P0-S10   
  774    32  TBPA  MB        TURB PROBE Pa       
  775    32  TBPB  MB        TURB PROBE Pb       
  776    32  TBPC  MB        TURB PROBE Ca       
  777    32  TBPD  MB        TURB PROBE Cb       
  778    32  S9SP  MB        S9 STATIC PRESSURE  
  779    32  TASD  MS-1      TURB PROBE DRY TAS  
  780    32  TASW  MS-1      TURB PROBE WET TAS  
  781    32  TPSP  MB        TURB PROBE TRUE PSP 
  782     1  COMR  PPB       CARBON MONOXIDE
  901     1  FCON  CM-3      FSSP CONCENTRATION  
  902     1  FRAV  MICRON    FSSP MEAN RADIUS    
  903     1  FLWC  G M-3     FSSP LWC            
  904     1  FRV   MICRON    FSSP MEAN VOL. RAD. 
  905     1  FRA   MICRON    FSSP MEAN AREAL RAD.
  906     1  FRE   MICRON    FSSP EFF. RADIUS    
  907     1  C-TC  L-1       2D-C TOTAL CONC.    
  908     1  CMAX  MICRON    2D-C MAXIMUM DIAM.  
  909     1  CCON  L-1       2D-C CONCENTRATION  
  910     1  CIWC  G M-3     2D-C IWC            
  911     1  CDAV  MICRON    2D-C MEAN DIAMETER  
  912     1  CLWC  G M-3     2D-C LWC            
  913     1  P-TC  L-1       2D-P TOTAL CONC.    
  914     1  PMAX  MICRON    2D-P MAXIMUM DIAM.  
  915     1  PCON  L-1       2D-P CONCENTRATION  
  916     1  PIWC  G M-3     2D-P IWC            
  917     1  PDAV  MICRON    2D-P MEAN DIAMETER  
  918     1  PLWC  G M-3     2D-P LWC            
  919     1  PCAS  CM-3      PCASP CONCENTRATION 
  920     1  PCAR  MICRON    PCASP MEAN RADIUS   
  921     1  PCAM  G M-3     PCASP MASS CONTENT  
  922     1  PCAV  MICRON    PCASP MEAN VOL. RAD.
  923     1  PCAA  MICRON    PCASP MEAN AREAL RAD
  924     1  PCAE  MICRON    PCASP EFF. RADIUS   
 1019     1  CUCF  W M-2     CORR UPPER CLR FLUX 
 1020     1  CURF  W M-2     CORR UPPER RED FLUX 
 1021     1  CUIF  W M-2     CORR UPPER I/R FLUX 
 1022     1  CLCF  W M-2     CORR LOWER CLR FLUX 
 1023     1  CLRF  W M-2     CORR LOWER RED FLUX 
 1024     1  CLIR  W M-2     CORR LOWER I/R FLUX """

class readm3(file_read):
    """
Routine for reading in M3 data
"""
    def __init__(self,dataset):
        self.input_names=['M3']
        self.outputs=[]
        self.patterns=('*raw_data.dat','*raw_hddr.dat','*raw_data.dat;*','*raw_hddr.dat;*')
        file_read.__init__(self,dataset)
        
    def fixfilename(self,filename):
        return filename[:filename.find('_raw')+4]
        
    def readfile(self,filename):
        hddr='_hddr.dat'
        self.filename=filename
        try:
            hd=open(self.filename+hddr)
            self.dat=self.filename+'_data.dat'
        except:
            try:
                hddr='_HDDR.DAT'
                hd=open(self.filename+hddr)
                self.dat=self.filename+'_DATA.DAT'
            except:
                return 
        a=hd.read(512)
        im=struct.unpack('<128i',a)
        if(im[21]==2 or im[21]==3):
            self.type='M3'
            self.fltno=str(a[0:4])
            self.date=str(a[4:6])+'-'+str(a[6:9])+'-'+str(a[10:12]) 
            self.idprms=im[32]
            self.iqprms=im[33]
            self.iqsecs=im[34]
            a=hd.read(512)
            im=struct.unpack('<128i',a)
            self.isectn=im[0]
            self.issrtt=im[1:1+self.isectn]
            self.isendt=im[41:41+self.isectn]
            self.isrecd=im[81:81+self.isectn]
            vms=1
            self.iqrsiz=im[121]
            ia=struct.unpack('<256h',hd.read(512))
            self.npara=256
            if 0 in ia:
                self.npara=list(ia).index(0)
                self.paras=np.zeros((self.npara),dtype=[('Numb','<i4'),('Freq','<i4'),('Other','<i4'),
                                                ('Name','|S20'),('Units','|S8'),('Shortname','|S4')])
            self.paras['Numb']=ia[0:self.npara]
            ia=struct.unpack('<256h',hd.read(512))
            self.paras['Other']=(np.array(ia[0:self.npara])-1)*2
            ia=struct.unpack('<256h',hd.read(512))
            self.paras['Freq']=ia[0:self.npara]
            self.paradesc()
        hd.close()
        dtype=[('Horace_dontknow','|S%i' % self.paras[0]['Other'])]
        for n in range(self.npara):
            dtype.append(('Horace_'+str(self.paras[n]['Shortname']),'<u2',int(self.paras[n]['Freq'])))
        dsize=2*np.sum(self.paras['Freq'])
        spare=2*self.iqrsiz-dsize-self.paras[0]['Other']
        dtype.append(('Horace_Spare','|S%i' % spare))
        self.time=self.get_time()
        self.data=np.memmap(self.dat,dtype=dtype,mode='r',shape=len(self.time))
        for p in self.paras:
            n='Horace_'+p['Shortname']
            f=p['Freq']
            self.outputs.append(parameter(n,number=p['Numb'],frequency=f,
                                          long_name=p['Name'],data=timed_data(self.data[n],self.time)
                                          ,units=p['Units']))

    def get_time(self):
        T=[]
        basetime=np.datetime64(datetime.datetime.strptime(self.date,"%d-%b-%y"))
        for i in range(self.isectn):
            T+=range(self.issrtt[i],self.isendt[i]+1,1)
        T=basetime+np.array(T,dtype='timedelta64[s]')
        return timestamp(T)

    def get_BCDtime(self):
        s=10*((self.data['Horace_GMTM']%256)/16)+self.data['Horace_GMTM']%16
        m=10*(self.data['Horace_GMTH']%16)+self.data['Horace_GMTM']/256
        h=10*(self.data['Horace_GMTH']/256)+(self.data['Horace_GMTH']%256)/16
        return timestamp(h*3600+m*60+s,dtype='f8')

    def time_BCD(self):
        s0=self.time%10
        s1=(self.time.astype(int)/10)%6
        m0=(self.time.astype(int)/60)%10
        m1=(self.time.astype(int)/600)%6
        h0=(self.time.astype(int)/3600)%10
        h1=(self.time.astype(int)/36000)
        return (s0+s1*16+m0*256,m1+h0*16+h1*256)

    def get_timex(self):
        basetime=np.datetime64(datetime.datetime.strptime(self.date,"%d-%b-%y"))
        return basetime+np.arange(self.issrtt[0],self.isendt[self.isectn-1]+1).astype('timedelta64[s]')

    def paradesc(self): 
        lines=mfdpardesc.split('\n')
        for i in range(self.npara):
            self.paras[i]['Name']='PARAMETER %i' % self.paras[i]['Numb']
            self.paras[i]['Units']='BITS'
            self.paras[i]['Shortname']='P%i' % self.paras[i]['Numb']
            for line in lines[5:]:
                p=int(line[0:6])
                if p==self.paras[i]['Numb']:
                    self.paras[i]['Name']=line[29:].strip()
                    self.paras[i]['Units']=line[19:27].strip()
                    self.paras[i]['Shortname']=line[13:17].strip().replace('/','_')
        return

 
