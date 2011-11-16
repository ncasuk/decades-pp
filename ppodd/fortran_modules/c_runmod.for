C
C ROUTINE         C_RUNMOD
C
C PURPOSE         Choose which module to run and run it.
C
C DESCRIPTION     CASE statement to choose which module.
C
C                 f2py --f77exec=ifort -c -m c_runmod c_runmod.for callib.a 
C
C VERSION         
C
C ARGUMENTS       COMMAND - Command name
C                 IRAW(86400,64,512) - contains the raw signal
C                 IFRQ(512) - Frequencies
C                 RCONST(64) - constants values 
C                 RDER(86400,64,1024) - contains the derived data
C
      SUBROUTINE C_RUNMOD(COMMAND,CONSTIN,
     &         PIN,FRQIN,DIN,FLAGIN,POUT,FRQOUT,NOUTALL,
     &         DOUT,FLAGOUT,
     &         LENGTH,NIN,NOUT,
     &         NINALL,NCONST)
      CHARACTER COMMAND*80
      INTEGER*4 NIN,LENGTH,NOUT,I,IR,IFR,IFIX,NINALL,NOUTALL
      INTEGER*4 PIN(NIN),FRQIN(NIN),POUT(NOUT),FRQOUT(NOUT)
      INTEGER*4 IRAW(64,512),IFRQ(512),NCONST
      INTEGER*1 FLAGIN(LENGTH,NINALL),FLAGOUT(LENGTH,NOUTALL)
      REAL*4 CONSTIN(NCONST)
      REAL*4 RCONST(64),RDER(64,1024),DOUT(LENGTH,NOUTALL)
      REAL*4 DIN(LENGTH,NINALL)
Cf2py character*80 :: command
Cf2py real*4 dimension(nconst) :: constin
Cf2py integer*4 dimension(nin) :: pin
Cf2py integer*4 dimension(nin),depend(nin) :: frqin
Cf2py real*4 dimension(length,ninall) :: din
Cf2py integer*1 dimension(length,ninall) :: flagin
Cf2py integer*4 dimension(nout) :: pout
Cf2py integer*4 dimension(nout),depend(nout) :: frqout
Cf2py integer*4 :: noutall
Cf2py real*4 intent(out),dimension(length,noutall),depend(length) :: dout
Cf2py integer*1 intent(out),dimension(length,noutall),depend(length) :: flagout
Cf2py integer*4 intent(hide),depend(din) :: length=shape(din,0)
Cf2py integer*4 intent(hide),depend(pin) :: nin=len(pin)
Cf2py integer*4 intent(hide),depend(pout) :: nout=len(pout)
Cf2py integer*4 intent(hide),depend(din) :: ninall=shape(din,1)
Cf2py integer*4 intent(hide),depend(constin) :: nconst=len(constin)

CDEC$ IDENT 'V1.00'
    
      DO I=1,64
          IF(I.LE.NCONST)THEN
                  RCONST(I)=CONSTIN(I)
                  CALL ISETFLG(RCONST(I),0)
              ELSE
                  RCONST(I)=0
                  CALL ISETFLG(RCONST(I),3)
              ENDIF
      ENDDO
      DO I=1,LENGTH
          IFX=0
          DO IR=1,NIN
              IF(PIN(IR).LT.512)THEN
                  IFRQ(PIN(IR))=FRQIN(IR)
                  DO IFR=1,FRQIN(IR)    
                         IFX=IFX+1
                     IRAW(IFR,PIN(IR))=IFIX(DIN(I,IFX))
                      ENDDO
                  ELSE
                  DO IFR=1,FRQIN(IR)    
                          IFX=IFX+1
                      RDER(IFR,PIN(IR))=DIN(I,IFX)
                              CALL ISETFLG(RDER(IFR,PIN(IR)),
     &FLAGIN(I,IFX))
                      ENDDO
                  ENDIF
          ENDDO
          SELECT CASE (COMMAND)
              CASE('AIRSPD')
                  CALL C_AIRSPD(IRAW,IFRQ,RCONST,RDER)
              CASE('CNC')
                  CALL C_CNC(IRAW,IFRQ,RCONST,RDER)
              CASE('DRS')
                  CALL C_DRS(IRAW,IFRQ,RCONST,RDER)
              CASE('GENEAS')
                  CALL C_GENEAS(IRAW,IFRQ,RCONST,RDER)
              CASE('HEIMAN')
                  CALL C_HEIMAN(IRAW,IFRQ,RCONST,RDER)
              CASE('INS1')
                  CALL C_INS1(IRAW,IFRQ,RCONST,RDER)
              CASE('LWC')
                  CALL C_LWC(IRAW,IFRQ,RCONST,RDER)
              CASE('NEPHL1')
                  CALL C_NEPHL1(IRAW,IFRQ,RCONST,RDER)
              CASE('NEVZ')
                  CALL C_NEVZ(IRAW,IFRQ,RCONST,RDER)
              CASE('OZONE1')
                  CALL C_OZONE1(IRAW,IFRQ,RCONST,RDER)
              CASE('NOX')
                  CALL C_NOX(IRAW,IFRQ,RCONST,RDER)
              CASE('SO2')
                  CALL C_SO2(IRAW,IFRQ,RCONST,RDER)
              CASE('COMR')
                  CALL C_COMR(IRAW,IFRQ,RCONST,RDER)
              CASE('PRESS1')
                  CALL C_PRESS1(IRAW,IFRQ,RCONST,RDER)
              CASE('PSAP')
                  CALL C_PSAP(IRAW,IFRQ,RCONST,RDER)
              CASE('RADAL1')
                  CALL C_RADAL1(IRAW,IFRQ,RCONST,RDER)
              CASE('GRFLUX')
                  CALL C_GRFLUX(IRAW,IFRQ,RCONST,RDER)
              CASE('RFLUX1')
                  CALL C_GRFLUX(IRAW,IFRQ,RCONST,RDER)
              CASE('RVSM')
                  CALL C_RVSM(IRAW,IFRQ,RCONST,RDER)
              CASE('SOLS')
                  CALL C_SOLS(IRAW,IFRQ,RCONST,RDER)
              CASE('GSUN')
                  CALL C_GSUN(IRAW,IFRQ,RCONST,RDER)
              CASE('SUN1')
                  CALL C_GSUN(IRAW,IFRQ,RCONST,RDER)
              CASE('TEMPS2')
                  CALL C_TEMPS2(IRAW,IFRQ,RCONST,RDER)
              CASE('TPRESS')
                  CALL C_TPRESS(IRAW,IFRQ,RCONST,RDER)
              CASE('TURB')
                  CALL C_TURB(IRAW,IFRQ,RCONST,RDER)
              CASE('TWC')
                  CALL C_TWC(IRAW,IFRQ,RCONST,RDER)
              CASE('WINDS')
                  CALL C_GWINDS(IRAW,IFRQ,RCONST,RDER)
              CASE('GWINDS')
                  CALL C_GWINDS(IRAW,IFRQ,RCONST,RDER)
              CASE('FWVS')
                  CALL C_FWVS(IRAW,IFRQ,RCONST,RDER)
          END SELECT
          IFX=0
          DO IR=1,NOUT
              DO IFR=1,FRQOUT(IR)
                      IFX=IFX+1
                      RD=RDER(IFR,POUT(IR))
                      FLAGOUT(I,IFX)=ITSTFLG(RD)
                      CALL ISETFLG(RD,0)
                  DOUT(I,IFX)=RD
              ENDDO
          ENDDO
      ENDDO
      RETURN
      END
