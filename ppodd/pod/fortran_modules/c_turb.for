!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         ROUTINE      C_TURB
!
!         PURPOSE      To calibrate and apply designated correction factors to
!                      angle of attack (AOA), angle of sideslip (AOSS) and the
!                      centre-static differential pressure (to derive TAS)).
!
!         DESCRIPTION  Calibration of AOA and AOSS is assumed to be of the form:
!                      
!                      PA/q = a0(M) + a1(M)*alpha
!                      PB/q = b0(M) + b1(M)*beta
!                      where q is the pitot(dynamic) pressure.
!                      Calculations follow the scheme described in BAES doc
!                      ADE-46S-R-463-34 1233 (Page 78 of 116).
!                      Initial value of pitot pressure is taken from RVSM and
!                      used to calculate first guess AOA and AOSS. These are
!                      to derive corrections to the centre-port along with 
!                      separate calculation of static position error in the
!                      centre-port measurement. AOA and AOSS are recalculated
!                      with iteration continuing until specified tolerance is
!                      achieved or max.iteration count exceeded. Corrected
!                      centre-port pressure is then used to calculate TAS
!                      (currently only the dry value) using:
!
!                      TAS = Corrtn.fac * 340.294*M*SQRT(T/288.15)
!
!         VERSION      1.01   Phil Brown 24/5/2004
!
!         CHANGES      1.02   Phil Brown 11/6/2004
!                             Logic changed to reproduce PVWAVE test version
!                             MRFB:[BROWN.PVWAVE]TURB.PRO at this date
!                      1.03   Phil Brown 28/6/2004
!                             Check flags and values following return of calls
!                             to S_MACH. Unacceptable causes C_TURB to return
!                             its default values of output parameters (flag 3)
!                      1.04   Phil Brown 2/7/04
!                             Uses G_MACH routine to calculate Mach no. and
!                             avoid complications due to flagging.
!                      1.05   Phil Brown 08/07/04
!                             Uses simpler Mach-dependent PE.Corrtn derived
!                             empirically from B001-012 s&l legs.
!                      1.06   Phil Brown 09/07/04
!                             No position error correction currently applied
!                             to P0 differential pressure.
!                      1.07   Phil Brown 26/08/04
!                             Change sign of AOSS. Cals were done against INS
!                             drift angle (-ve for +ve AOSS).
!                      1.08   Phil Brown 27/8/04
!                             AOSS calcs revert to original, but assumed to use
!                             new fit coefficients for B0 and B1
!                      1.09   26/01/06 Phil Brown
!                             Min/max limits provided for AoA, AoSS and TAS for
!                             flagging purposes.
!                      1.10   20/06/06 Phil Brown
!                             Takes additional input of non-deiced temp, used as
!                             alternative when de-iced is flagged 2 or more.
!                      1.11   24/10/06 Phil Brown
!                             Fix bug setting flag on TTND to zero before use.
!                             Define 4 additional flight constants to apply
!                             fudge factors to the calculated AoA / AoSS
!                             These have the form:
!                             AoA_new = AoA * ALPH1 + ALPH0
!                             AoSS_new= AoSS * BET1 + BET0
!                      1.12   08/10/2010 Axel Wellpott
!                             added line "DATA TAS/-9999./"
!                             Missing TAS data values were set to -999.
!                             and not to -9999. as specified in the netcdf
!                             files.
!
!         SUBROUTINES: S10_PECORR, ITSTFLG, ISETFLG, G_MACH
!
!         FILES        
!
!         REFERENCES 
!
!         CONSTANTS 
!           RCONST(1-3) Coefficients of 2nd order polynomial in Mach to 
!                       calculate AOA offset, A0 
!           RCONST(4-6) Coefficients of 2nd order polynomial in Mach to 
!                       calculate AOA sensitivity, A1 
!           RCONST(7-9) Coefficients of 2nd order polynomial in Mach to 
!                       calculate AOSS offset, B0 
!           RCONST(10-12) Coefficients of 2nd order polynomial in Mach to 
!                       calculate AOSS sensitivity, B1
!           RCONST(13)  Tolerance for AOA/AOSS iteration
!           RCONST(14)  True Airspeed correction factor (fudge factor to 
!                       remove residual along-heading wind errors).
!           RCONST(15-16) Coefficients of linear correction to calculated AoA
!           RCONST(17-18) Coefficients of linear correction to calculated AoSS
!  
!         INPUT PARAMETERS
!           516  IAS   32Hz
!           520  TTDI  32Hz
!           525  TTND  32Hz
!           576  SPR   32Hz
!           577  PSP   32Hz
!           578  PHGT  32Hz
!           773  TBP0  32Hz
!           774  TBPA  32Hz
!           775  TBPB  32Hz
!           776  TBPC  32Hz
!           777  TBPD  32Hz
!
!         OUTPUT PARAMETERS
!           
!           548  AOA   32Hz  deg
!           549  AOSS  32Hz  deg
!           779  TASD  32Hz  ms-1
!           780  TASW  32Hz  ms-1
!           781  TPSP  32Hz  mb
! 
!         TURBULENCE PROBE CONSTANT KEYWORDS
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE C_TURB(IRAW,IFRQ,RCONST,RDER)
CDEC$ IDENT 'V1.12'
      IMPLICIT NONE
      INTEGER*4 IRAW(64,512),IFRQ(512),I,J
      INTEGER*4 ITSTFLG, ITTDIFLG, ITTNDFLG, ISPRFLG, IPSPFLG, IPHGTFLG
      INTEGER*4 IP0FLG, IPAFLG, IPBFLG, ICAFLG, ICBFLG, IMACHFLG
      INTEGER*4 IASFLG, IFLAG
      REAL*4    RCONST(64),RDER(64,1024)
      REAL*4    AIAS,TTDI,TTND,SPR,PSP,PHGT,TP0,TPA,TPB,CA,CB
      REAL*4    AOA, AOANEW, AOSS, AOSSNEW, TOL
      REAL*4    DCP_S10, DCPA, DCPB, P0, Q
      REAL*4    AMACH, A0, A1, B0, B1, DAOA, DAOSS, TAS
      DATA TAS/-9999./                   ! set all TAS values to -9999.
!
      REAL*4    AIASMIN,TASMIN,TASMAX
      DATA AIASMIN,TASMIN,TASMAX/50.0,50.,250./
!
      REAL*4    AOAMIN,AOAMAX,AOSSMIN,AOSSMAX
      DATA AOAMIN,AOAMAX,AOSSMIN,AOSSMAX/0.,15.0,-5.0,5.0/
!
      INTEGER*4 ITER, ITERMAX, MAXITER
      DATA ITER,ITERMAX,MAXITER/0,5,0/   ! iteration counters, within each
!                                        ! sample and each second
!
      DO I=1,32                         ! set placeholder values and flags
        RDER(I,548)=-99.9                !Set AoA to 0
        CALL ISETFLG(RDER(I,548),3)
        RDER(I,549)=-99.9                !Set AoSS to 0
        CALL ISETFLG(RDER(I,549),3)
        RDER(I,779)=-9999.                !Set dry turbulence probe airspeed
        CALL ISETFLG(RDER(I,779),3)
        RDER(I,780)=-9999.                !Set wet turbulence probe airspeed
        CALL ISETFLG(RDER(I,780),3)
        RDER(I,781)=-950.                !Set turbulence probe pitot-static
        CALL ISETFLG(RDER(I,781),3)
      END DO

! now do the real calculations
      MAXITER = 0

      DO I=1,32                         ! outer loop over 32 samples
        AIAS = RDER(I,516)
        IASFLG   = ITSTFLG(AIAS)        ! preserve flags from all inputs params
        CALL ISETFLG(AIAS,0)            ! and reset flag to zero
        TTDI = RDER(I,520)
        ITTDIFLG = ITSTFLG(TTDI)
        CALL ISETFLG(TTDI,0)
        TTND = RDER(I,525)
        ITTNDFLG = ITSTFLG(TTND)
        CALL ISETFLG(TTND,0)                   ! V1.11 bug fix here
        SPR  = RDER(I,576)
        ISPRFLG  = ITSTFLG(SPR)
        CALL ISETFLG(SPR,0)
        PSP  = RDER(I,577)
        IPSPFLG  = ITSTFLG(PSP)
        CALL ISETFLG(PSP,0)
        PHGT = RDER(I,578)
        IPHGTFLG = ITSTFLG(PHGT)
        CALL ISETFLG(PHGT,0)
        TP0   = RDER(I,773)
        IP0FLG   = ITSTFLG(TP0)
        CALL ISETFLG(TP0,0)
        TPA   = RDER(I,774)
        IPAFLG   = ITSTFLG(TPA)
        CALL ISETFLG(TPA,0)
        TPB   = RDER(I,775)
        IPBFLG   = ITSTFLG(TPB)
        CALL ISETFLG(TPB,0)
        CA   = RDER(I,776)
        CB   = RDER(I,777)

!        IF(AIAS.LE.AIASMIN) THEN
!          PRINT *,'Inputs'
!          PRINT *,AIAS,TTDI,SPR,PSP,PHGT,TP0,TPA,TPB,CA,CB
!          PRINT *,'RDER(515-520)'
!          PRINT *,(RDER(I,J),J=515,1024)
!        ENDIF

! Proceed only with acceptable flag settings and IAS > 50m/s
        IF(ISPRFLG.LE.1.AND.IPSPFLG.LE.1.AND.IPHGTFLG.LE.1.AND.
     -     IP0FLG.LE.1.AND.IPAFLG.LE.1.AND.IPBFLG.LE.1.AND.
     -     AIAS.GT.AIASMIN) THEN

! Mach number from RVSM pitot pressure
!        CALL ISETFLG(SPR,0)
!        CALL ISETFLG(PSP,0)
        Q = PSP
        CALL G_MACH(SPR, Q, AMACH)
! Check values returned by Mach number calculation
        IF(AMACH.GT.0.0.AND.AMACH.LT.1.0) THEN
!        PRINT *,SPR,PSP,AMACH

! First guess AOA and AOSS
        A0 = RCONST(1)+AMACH*(RCONST(2)+AMACH*RCONST(3))
        A1 = RCONST(4)+AMACH*(RCONST(5)+AMACH*RCONST(6))
        B0 = RCONST(7)+AMACH*(RCONST(8)+AMACH*RCONST(9))
        B1 = RCONST(10)+AMACH*(RCONST(11)+AMACH*RCONST(12))

!        PRINT *,'A0,A1,B0,B1',A0,A1,B0,B1,AMACH

        AOA  = (TPA/Q - A0)/A1
        AOSS = (TPB/Q - B0)/B1

! Calculate position error in S10 static pressure.
!        PRINT *,P0,PHGT,PSP,AOA,AOSS
        CALL S10_PECORR(DCP_S10,AMACH)

! Calculate and apply flow angle corrections to derive true pitot pressure
! from centre-port measurement.
        DCPA = 0.0273+ AOA*(-0.0141+ AOA*(0.00193- AOA*5.2E-5))
        DCPB = 0.0   +AOSS*(0.0    + AOSS*7.6172E-4)

! Apply corrections to measured differential pressure
!        P0 = TP0+(DCPA+DCPB+DCP_S10)*Q
        P0 = TP0+(DCPA+DCPB)*Q
        Q = P0
!        PRINT *,'P0 = ',P0

! Recalculate Mach number
!        CALL ISETFLG(SPR,0)
        CALL ISETFLG(Q,0)
        CALL G_MACH(SPR,Q,AMACH)
! Check values returned by Mach number calculation
        IF(AMACH.GT.0.0.AND.AMACH.LT.1.0) THEN
!        PRINT *,SPR,Q,AMACH

        ITER=0
! Recalculate AOA/AOSS
100     ITER=ITER+1
        A0 = RCONST(1)+AMACH*(RCONST(2)+AMACH*RCONST(3))
        A1 = RCONST(4)+AMACH*(RCONST(5)+AMACH*RCONST(6))
        B0 = RCONST(7)+AMACH*(RCONST(8)+AMACH*RCONST(9))
        B1 = RCONST(10)+AMACH*(RCONST(11)+AMACH*RCONST(12))

        AOANEW  = (TPA/Q - A0)/A1
        AOSSNEW = (TPB/Q - B0)/B1

        DAOA = AOANEW-AOA
        DAOSS= AOSSNEW-AOSS
        TOL  = RCONST(13)

        AOA = AOANEW
        AOSS= AOSSNEW

! Recalculate position error correction to S10
        CALL S10_PECORR(DCP_S10,AMACH)

! Recalculate flow-angle corrections to centre-port.
        DCPA = 0.0273+ AOA*(-0.0141+ AOA*(0.00193- AOA*5.2E-5))
        DCPB = 0.0   +AOSS*(0.0    + AOSS*7.6172E-4)

! Apply corrections to measured pressure
!        P0 = TP0+(DCPA+DCPB+DCP_S10)*Q
        P0 = TP0+(DCPA+DCPB)*Q
        Q  = P0

! Recalculate Mach number
!        CALL ISETFLG(SPR,0)
        CALL ISETFLG(Q,0)
        CALL G_MACH(SPR,Q,AMACH)
! Check values returned by Mach number calculation
        IF(AMACH.GT.0.0.AND.AMACH.LT.1.0) THEN

! Test flow angles changes wrt tolerance
        IF((ABS(DAOA).GT.TOL.OR.ABS(DAOSS).GT.TOL)
     *     .AND.ITER.LT.ITERMAX) GOTO 100

! Calculate dry (and later also wet) TAS from Mach number and temperature
!        PRINT *,'AMACH / TTDI', AMACH, TTDI
        IF(ITTDIFLG.LE.1) THEN     ! de-iced OK
          TAS = RCONST(14) * 340.294 * AMACH * SQRT(TTDI/288.15)
        ELSE
          TAS = RCONST(14) * 340.294 * AMACH * SQRT(TTND/288.15)
        ENDIF

! Apply linear corrections to calculated AoA and AoSS, derived from Al Rodi
! analysis - minimization of residual vertical wind during yawing orbits.
        IF((ITSTFLG(RCONST(15)).EQ.0.).AND.
     &     (ITSTFLG(RCONST(16)).EQ.0.).AND.
     &     (ITSTFLG(RCONST(17)).EQ.0.).AND.
     &     (ITSTFLG(RCONST(18)).EQ.0))THEN
             AOA = AOA*RCONST(16) + RCONST(15)
             AOSS= AOSS*RCONST(18) + RCONST(17)
        ENDIF

! Check data flagging and output parameters. 

        RDER(I,548) = AOA
        IFLAG=0
        IF(AOA.LT.AOAMIN.OR.AOA.GT.AOAMAX) IFLAG=2
        IFLAG=MAX(IFLAG,IPAFLG)
        CALL ISETFLG(RDER(I,548),IFLAG)

        RDER(I,549) = AOSS
        IFLAG=0
        IF(AOSS.LT.AOSSMIN.OR.AOSS.GT.AOSSMAX) IFLAG=2
        IFLAG=MAX(IFLAG,IPBFLG)
        CALL ISETFLG(RDER(I,549),IPBFLG)

        RDER(I,779) = TAS
        IFLAG=0
        IF(TAS.LT.TASMIN.OR.TAS.GT.TASMAX) IFLAG=2
        IFLAG=MAX(IFLAG,IP0FLG)
        CALL ISETFLG(RDER(I,779),IP0FLG)

        RDER(I,780) = -99.9
        CALL ISETFLG(RDER(I,780),3)
        RDER(I,781) = Q
        CALL ISETFLG(RDER(I,781),IP0FLG)

        ENDIF   ! test on third Mach number calculation
        ENDIF   ! test on second Mach number calculation
        ENDIF   ! test on first Mach number calculation

        ENDIF ! end of test on flag settings

! track maximum iteration count in this 1-second sample
        IF(ITER.GT.MAXITER) MAXITER=ITER

      ENDDO ! end of calculation loop
!      PRINT *,'Max. iteration count this second =',MAXITER

      RETURN
      END
!
      SUBROUTINE S10_PECORR(DCP_S10,AMACH)
CDEC$ IDENT 'V1.00'
!
! PURPOSE  To calculate values of the S10 static pressure position error  
!          as a function of Mach number, derived from B001-B012 calibrations
!
! AUTHOR   Phil Brown
!
! VERSION  1.00 08/07/2004
!
      IMPLICIT NONE
      REAL*4 DCP_S10, AMACH, A0,A1,A2
      DATA A0,A1,A2/-0.011482, -0.148295, 0.407040/

      DCP_S10 = A0 + AMACH*(A1 + AMACH*A2)

      RETURN
      END
