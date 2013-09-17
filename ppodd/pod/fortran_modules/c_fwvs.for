C
C ROUTINE          C_FWVS SUBROUTINE FORTVAX
C
C PURPOSE          Produces FWVS dewpoint 
C
C DESCRIPTION      
C
C VERSION          1.00 30/04/07 D TIDDEMAN 
C
C ARGUMENTS        IRAW(64,512) I*4 IN  Up to 64 samples for up to 512 DRS pars
C                  IFRQ(512)    I*4 IN  Sample rate of each DRS par (0-64)
C                  RCONST(64)   R*4 IN  Inputs constants (none used by C_CNC)
C                  RDER(64,1024)R*4 OUT Output array of up to 64 samples for
C                                       each of 1024 parameters
C
C CHANGES          
C
********************************************************************************
      SUBROUTINE C_FWVS(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.00'
      IMPLICIT NONE
      INTEGER*4 IRAW(64,512)   !Raw data array (64 - need only be 1 here)
      INTEGER*4 IFRQ(512)      !Raw data frequency (always 1 in this case)
      REAL*4    RCONST(64)     !Constants array (64 - need only be 1 here)
      REAL*4    RDER(64,1024)  !Derived data array, treated as integer

      RDER(1,573)=REAL(IRAW(9,230))/10.0
      IF((RDER(1,573).GT.150).AND.(RDER(1,573).LT.350))THEN
         CALL ISETFLG(RDER(1,573),0)
      ELSE
         CALL ISETFLG(RDER(1,573),3)
      ENDIF

      RETURN
      END
