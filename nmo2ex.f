      SUBROUTINE NMO2EX(BUF,LBUF,IBUF,SCR,LSCR,ISCR, nmonum )
C     NMOEX IS THE EXECUTION PHASE OF THE SEISMIC REFLECTION PROCESS NMO (NORMAL
C  MOVE OUT).  THE NMO PARAMETERS MUST BE ON DISC FILE nunit AND THE TRACE MUST
C  BE IN MEMORY LOCATION BUF.
C     NMOEX CALCULATES THE NMO IN THE AP.  THE OUTPUT OF THE AP IS ACTUALLY THE T0
C  INDEXES SO THAT THE NMO RESULT IS THE T0 ARRAY. I.E. THE EQUATION
C  REALLY BEING SOLVED HERE IS  TX=SQRT(T0+X**2/V**2).  T0 IS ANY ARRAY OF TIME
C  VALUES EACH SEPARATED BY THE SAMPLE INTERVAL, X IS THE SHOT-RECEIVER DISTANCE
C  OF THE TRACE, AND V IS THE USER'S VELOCITY FUNCTION.  THUS, TX IS WHERE
C  THE DATA IS BEFORE NMO.
C     SUBROUTINE NMOED CONTAINS THE EXPLAINATION OF THE USER PARAMETERS AND THE
C  ORDER OF THE USER PARAMETERS ON DISC.
C
C  ARGUMENTS:
C  BUF    - THE TRACE TO BE NMOD, INCLUDING THE TRACE HEADER.  THE FIRST
C           DATA SAMPLE MUST BE AT TIME DELAY.  THIS IS THE FLOATING
C           POINT (REAL) TRACE ARRAY.
C  LBUF   - THE LONG INTEGER TRACE ARRAY.  THIS IS REALLY THE SAME AS BUF, BUT
C           PRIME FORTRAN DOESN'T ALLOW EQUIVALENCING ANYTHING TO AN ARGUMENT.
C  IBUF   - THE SHORT INTEGER TRACE ARRAY.  NEEDED FOR 16 BIT TRACE HEADER
C           ADDRESSES.
C  SCR    - A SCRATCH ARRAY FOR READING THE PARAMETERS.  THEREFORE, SCR MUST
C           BE AT LEAST 56 32BIT WORDS BIG.  SCR MAY BE DESTROYED BY THE CALLING
C           ROUTINE.
C  LSCR   - THE SAME SCRATCH ARRAY BECAUSE OF THE EQUIVALENCING PROBLEM.
C  ISCR   - THE SAME ARRAY BECAUSE OF THE EQUIVALENCING PROBLEM.
C
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, APRIL 1980
C
c  mod 26 May 1993 - Spatial interpolation, vintpl 1, was wrong when
c                 the velocity decreased spatially.
c  mod 26 Nov 93 - Spatial variation was bad when first trace of the job
c                 was not a control point.
c  mod May 1995 - Add numnmo
c  mod Nov 1995 - add VTRKWB gmk
c  mod 24 Aug. 96 - Print the first water depth when vtrkwb is given.
c  mod 16 Apr. 97 - Do the print when vtkwb is GE 0 since preset is -9999.
c  mod 21 May 97 - NPARS increased by 1 for VMUL
c  mod 21 Jul 97 - Allow for NMO3
c                - NPARS increased by 1 for VADD
c  mod 11 Nov. 97 - Add opath/lunvtp and nsegyfile
c  mod 17 Feb 98 (Nathan Bangs) - bad index on TNEXT - was scr(i+10) but
c                 should be scr(i+npars+1)
c  mod 16 Mar. 98 - Install VINTPL 3
c  mod 2 Nov. 98 - Add XMO (type 4), parameter NEWX
c  mod 18 Jan 00 - When vtrkwb and depth decreases start reading the velocity
c                  from the beginning.
c  mod 28 Mar 01 - interpolation was wrong on last pair when vintpl 1 and "thinning"
c  mod 17 Jul 02 - Add parameter dstretch
c                - Modify nmonap to return the real value of TX
c  mod 6 Oct 02 - Add water depth to the warning message when Centerbeam
c                 differs from last.
c  mod 10 Mar 03 - Increase NPARS because nmoed did.
c  mod Apr 03 - Do interval to rms velocity conversion.
c             - Add vintpl 4 to interpolate by order in vtp (not iso anything).
c             - Extrapolation of the last vtp wasn't happening.
c  mod 24 May 08 - vtrkwb with shallowing water caused ap buffering problems
c  mod 12 Apr 10 - Use start & end mute in mils (ihdr(56) & 57) rather than 
c                  seconds (reals 47 & 48)
c  mod 24 May 10 - Use lbuf(16) rather than buf(50) for water depth
c  mod 27 Jun 11 - Add parameter HIRES
c                - Always use different array for input and output in nmoapp - helps inversion problem
c  mod 9 May 12 - Call to polint in HIRES was bad (good thing hires was never documented!
c  mod 18 Jul 12 - Redo DSTRETCH so that it's (tx-t0) / t0
C
      PARAMETER ( NPARS = 18 )
      PARAMETER (MAXVTP=50)                                             ! THE MAXIMUM NUMBER OF ELEMENTS THE USER ARRAY CAN BE
      PARAMETER (NWRDS=MAXVTP+NPARS)                                    ! THE LENGTH OF EACH PARAMETER LIST
      PARAMETER (NTERPOLATE = 4)     ! number of elements in polynomial for interpolation
      PARAMETER (N2 = NTERPOLATE / 2)
      DIMENSION BUF(111),LBUF(111),IBUF(111),SCR(111),LSCR(111),
     *          ISCR(111)
      INTEGER*2 IBUF,ISCR
      INTEGER*4 VINTPL, CURFNO, OLDFNO
      REAL*4 VTRKWB, WBDEPTH, WBDEPTHO, DIFFWBD
      DIMENSION OLDVTP(MAXVTP),CURVTP(MAXVTP)
      DIMENSION  S(4), txtimes(NTERPOLATE)
      EQUIVALENCE (S(1),DELAY),(S(3),SI),(S(2),RANGE)
      COMMON /NMOCOM/ nunit(3), nlists(3), nsegyfile(3)
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP
      COMMON /APMEM/A(5000000)
      COMMON /READT/ ILUN,NUMHDR, numdat
      INTEGER FNO
      LOGICAL FIRST, firstap
      SAVE
      DATA FIRST /.TRUE./, llnum/0/, ispat/0/, firstap/.TRUE./,
     &       wbdeptho/0/
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      IF(IBUF(15).EQ.2) RETURN                                          ! IS IT A DEAD TRACE
      ICORE=0
c      NSAMPS=IBUF(58)                                                   !  GET THE NUMBER OF DATA SAMPLES FROM THE TRACE HEADER
c**** watch byte swap
      nsamps = numdat
      IF( IN .NE. 0 ) THEN
          IF( IUSEAP .NE. 0 ) THEN                                      ! =0 MEANS NO AP
              CALL APWR                                                 ! WAIT FOR ANY PROCESSING IN THE AP TO FINISH
              CALL APGET(BUF(NUMHDR+1),IN,NSAMPS,2)                     ! START THE DATA IN FROM THE AP
          ELSE
              DO 3 I=1,NSAMPS
    3         BUF(NUMHDR+I)=A(IN+I-1)
          ENDIF
      ELSE
          CALL INAP(BUF(NUMHDR+1),NSAMPS)                               ! MAKE SURE THE AP ASSIGNMENT HAS A TRACE
      ENDIF
   10     CONTINUE                                                      ! GET THE FIRST PARAMETER LIST INT0 MEMORY ARRAY SCR
      IF( FIRST ) THEN
c          WDEPTH = BUF(54)
          wdepth = FLOAT(lbuf(16))
          IF (LBUF(7).NE.0) THEN                                        ! PRESET PREVIOUS SHOT/CDP GATHER, FIRST FNO
           OLDFNO = LBUF(6)
          ELSE
           OLDFNO = LBUF(3)
          ENDIF
          CALL PODISC(nunit(nmonum),1,0)
          CALL RDDISC(nunit(nmonum),SCR,NWRDS,ISTAT)                    ! READ THE FIRST PARAMETER LIST
          FNO=LSCR(1)
          LNO=LSCR(2)
          VTRKWB=SCR(8)                                                 ! ALLOWABLE GATHER TO GATHER CENTERBEAM DEPTH CHANGE
c          IF( vtrkwb .GE. 0. ) PRINT *,' Depth is: ',buf(54)
          lunvtp = lscr(11)
          newx = lscr(12)
          xfactor = scr(16)
          hires = scr(17)
          MLISTS=1
          ICORE=1
          IF( firstap ) THEN
              IVELAD=NEXTAD                                             ! THE AP ADDRESS OF WHERE THE VTP CAN GO
              NEXTAD=NEXTAD+NSAMPS
              IF( NEXTAD+3 .GE. LAPSIZ ) THEN
                  PRINT *,'***  TOO MUCH AP BEING REQUESTED - NMO.'
                  STOP
              ENDIF
              firstap = .FALSE.
          ENDIF
          FIRST=.FALSE.
          llnum = 0
      ENDIF
      LNUM=LBUF(3)                                                      ! IS THE DATA ON TAPE SORTED BY SHOT
      SI=BUF(49)                                                        ! THE SAMPLE INTERVAL IN SECONDS
      RANGE=LBUF(10)                                                    ! THE SHOT-RECEIVER DISTANCE
      IF( xfactor .NE. 1. ) range = range * xfactor
      DELAY=BUF(46)                                                     ! THE DEEP WATER DELAY IN SECONDS
      IDELAY=DELAY/SI
c**** set old water depth to the first good one we see
c      IF( wbdeptho .EQ. 0 .AND. buf(54) .GT. 20 ) wbdeptho = buf(54)
      IF( wbdeptho .EQ. 0 .AND. lbuf(16) .GT. 20 ) wbdeptho = lbuf(16)
C     SHIPBOARD PROCESSING, USE WATER DEPTH TO INTERPOLATE STACKING VELOCITIES
      IF( VTRKWB .NE. -9999. ) THEN
          IF (LBUF(7).NE.0) THEN                                        ! CURRENT SHOT/CDP GATHER
              CURFNO = LBUF(6)
              itrcno = lbuf(7)
          ELSE
              CURFNO = LBUF(3)
              itrcno = lbuf(4)
          ENDIF
          IF (CURFNO.EQ.OLDFNO) GOTO 20                                 ! SAME FNO, NO NEW WBDEPTH, CONTINUE
c          WBDEPTH=BUF(54)                                               ! CURRENT CENTERBEAM WATER-BOTTOM DEPTH
          wdepth = FLOAT(lbuf(16))
          DIFFWBD = ABS(WBDEPTH-WBDEPTHO)                               ! DIFFERENCE IN DEPTHS   
          IF( DIFFWBD .GT. VTRKWB .AND. wbdeptho .NE. 0) THEN
              WBDEPTH=WBDEPTHO
              WRITE(*,*) '*** WARNING *** CENTERBEAM DEPTH JUMPED BY: ',
     &           DIFFWBD, ' Meters, using', wbdepth                      ! IF DEPTH JUMPS BY VTRKWB, USE OLD WBDEPTH
          ENDIF
      IF(((wbdeptho .EQ. 0).OR.(IAND(lprint,4).NE.0)).AND.itrcno .EQ. 1)
     *        PRINT *,' Water depth is:  ',wbdepth, wbdeptho
c****     Damn.  Force finding the velocity function from scratch if
c****     the water is shallowing.  
          IF( wbdepth .LT. wbdeptho ) THEN
              wbdeptho = wbdepth
              first = .TRUE.
              GOTO 10
          ENDIF
          WBDEPTHO=WBDEPTH                                              ! SET CURRENT WBDEPTH TO OLD FOR NEXT GATHER
          OLDFNO=CURFNO                                                 ! SET CURRENT FNO TO OLD FOR NEXT GATHER   
      ENDIF
   20 CONTINUE
C****
C****   THE VELOCITY FUNCTION IN THE AP CAN BE REUSED ON SUCCESSIVE TRACES
C****   PROVIDING THAT NOT MUCH HAS CHANGED.  THIS ASSUMPTION ASSUMES THAT
C****   THE SAME VELOCITY APPLIES TO ALL OF A SHOT OR RP!
C****
      IF(LBUF(7).NE.0) LNUM=LBUF(6)                                     !  OR BY RP
      IF(VTRKWB.NE.-9999.0) LNUM=NINT(WBDEPTH)                          !  OR BY WATER DEPTH, SHIPBOARD PROCESSING
      IF(LNUM.EQ.LLNUM.AND.DELAY.EQ.ODELAY.AND.NSAMPS.EQ.MSAMPS
     *    .AND.IADDWB.EQ.0) GO TO 2120
      IF( ISPAT .NE. 0 ) THEN                                           ! HAS CUR BEEN CLOBBERED BY SPATIALLY VARYING?
          ITEMP=-NWRDS                                                  ! GET VTP FROM DISC SINCE CURVTP HAS A SPATIALLY VARIED VTP
          CALL PODISC(nunit(nmonum),2,ITEMP)                            ! BACKUP A LIST
          CALL RDDISC(nunit(nmonum),SCR,NWRDS,ISTAT)
          FNO=LSCR(1)
          LNO=LSCR(2)
          newx = lscr(12)
          ICORE=1
      ENDIF
   70 IF(LNUM.GE.FNO.OR.MLISTS.EQ.1) GO TO 100                          ! IS THIS SHOT BEFORE THIS PARAMTER LIST
      IF(MLISTS.EQ.1) GO TO 500                                         ! IS IT BEFORE THE FIRST LIST
      IF(LNUM.LE.LASTNO) GO TO 10                                       ! IS IT IN OR BEFORE THE LAST LIST
      GO TO 500                                                         ! IT MUST BE BETWEEN THE 2 LISTS
  100 CONTINUE                                                          !  THE CURRENT SHOT (RP) IS >= LNO
      IF(LNUM.LE.LNO) GO TO 500                                         ! USE THE PARAMETERS OF THIS LIST
      IF(MLISTS.LT.nlists(nmonum)) GO TO 110                            ! ANY MORE USER PARAM LISTS ON DISC
      LNO=32768
      IF(ICORE.EQ.0) GO TO 2000
      GO TO 500
C****
C****   GET ANOTHER USER PARAMETER LIST FROM DISC
C****
  110 CONTINUE
      IF( ICORE .EQ. 0 ) THEN                                           ! IS THERE A PARAMETER LIST IN SCR
          NOVTPS=NVTPS
          LASTNO=LNO
          DO 113 I=1,MAXVTP
  113     OLDVTP(I)=CURVTP(I)
      ELSE
          DO 120 I=1,MAXVTP                                             ! SAVE THE CURRENT PARMETER SET
  120     OLDVTP(I)=SCR(I+npars)
          NOVTPS=LSCR(npars)
          LASTNO=LNO
          NVTPS=LSCR(npars)
      ENDIF
      CALL RDDISC(nunit(nmonum),LSCR,NWRDS,ISTAT)
      ICORE=1
      FNO=LSCR(1)
      LNO=LSCR(2)
      MLISTS=MLISTS+1
c      IF( vtrkwb .GE. 0. ) PRINT *,' Water depth is: ',buf(54)
      GO TO 70
C****
C****     SPATIALLY VARY THE VELOCITY FUNCTION IF NECESSARY
C****
  500 CONTINUE
      IF(ICORE.EQ.0) THEN
          CALL podisc( nunit(nmonum), 2, -nwrds )
          CALL rddisc( nunit(nmonum), lscr, nwrds, istat )
          fno = lscr(1)
          lno = lscr(2)
          icore = 1
      ENDIF
      STRETC=SCR(3)
      ITYPE=LSCR(4)
      IADDWB=LSCR(5)
      LPRINT=LSCR(6)
      VINTPL=LSCR(7)
      dstretch = scr(14) / 100.
      xfactor = scr(16)
      hires = scr(17)
      NVTPS=LSCR(npars)
      DO 510 I=1,NVTPS
  510 CURVTP(I)=SCR(I+npars)
      ISPAT=0
      IF(LNUM.GE.FNO.AND.LNUM.LE.LNO) GO TO 2000
      IF(MLISTS.EQ.1) GO TO 2000                                        ! IF THE FIRST LIST, DON'T SPATIAL VARY
C****
C****     THE VELOCITY VARIATION IS BY ISO-VELOCITY
C****      (FIND EQUAL VELOCITIES THEN INTERPOLATE THE TIMES)
C****
C**** ALLOWS USER TO SPECIFY VELOCITY INTERPOLATION
      rfno = fno
      rlastno = lastno
      rlnum = lnum
      ratio = (rlnum - rlastno) / (rfno - rlastno)
      IF( vintpl .EQ. 4 ) THEN
          DO i = 1, novtps
             curvtp(i) = (scr(npars+i) - oldvtp(i)) * ratio + oldvtp(i)
          ENDDO
      ENDIF
      IF( VINTPL .EQ. 1 .OR. vintpl .EQ. 3 ) THEN
        IF( novtps .GE. nvtps .OR. vintpl .EQ. 1 ) THEN
c****   "thinning" - going from more vtps to less vtps.
          DO 660 I=1,NOVTPS,2                                           !  FIND THE VELOCITY IN THE OLDVTP LIST
             VL=OLDVTP(I)
             TL=OLDVTP(I+1)
             DO 640 J=1,NVTPS,2                                         ! FIND THE VELOCITY IN THE CURVTP LIST
                VNEXT=SCR(J+npars)
                TNEXT=SCR(J+npars+1)
                IF( VL .EQ. VNEXT ) GOTO 650                            ! IS THE VELOCITY GIVEN BY THE USER
                IF( VL .LE. VNEXT ) THEN                                ! MAYBE THE VELOCITY IS YET TO COME
                  IF(J.EQ.1) GO TO 645
                  TNEXT = (VL-SCR(J+npars)) / 
     &                    (scr(j+npars-2)-SCR(J+npars)) * 
     &                    (scr(j+npars-1)-SCR(J+npars+1))+SCR(J+npars+1)
                  GO TO 650
                ENDIF
  640        CONTINUE
  645        VL=(VNEXT-VL) * ratio + VL                                   ! INTERPOLATE THE LAST VELOCITY
  650        CONTINUE
             CURVTP(I)=VL
             CURVTP(I+1)=(TNEXT-TL) * ratio + TL
  660     CONTINUE
c****   removed the next line 28 Mar 01
c          NVTPS=NOVTPS
        ELSE
c****     "thickening" - going from fewer vtps to more vtps.
          DO i = 1, nvtps, 2
             vl = scr(i+npars)
             tl = scr(i+npars+1)
             DO j = 1, novtps, 2
                vnext = oldvtp(j)
                tnext = oldvtp(j+1)
                IF( vl .EQ. vnext ) GOTO 690
                IF( vl .LT. vnext ) THEN
                    IF( j .EQ. 1 ) GOTO 685
                    tnext = (vl-oldvtp(j)) / (oldvtp(j-2)-oldvtp(j)) *
     &                      (oldvtp(j-1)-oldvtp(j+1)) + oldvtp(j+1)
                    GOTO 690
                ENDIF
             ENDDO
  685        vl = (vnext-vl) * ratio + vl
  690        CONTINUE
             curvtp(i) = vl
             curvtp(i+1) = (tl-tnext) * ratio + tnext
           ENDDO
        ENDIF
        ISPAT=1  
        IF( vintpl .EQ. 3 ) THEN
            curvtp(1) = (scr(1+npars)-oldvtp(1))/(rfno-rlastno)
     &                 * (rlnum-rlastno) + oldvtp(1)
            curvtp(2) = (scr(2+npars)-oldvtp(2))/(rfno-rlastno)
     &                 * (rlnum-rlastno) + oldvtp(2)
        ENDIF
C****
C****                INTERPOLATION BY TUPLE
C****
      ELSE IF (VINTPL.EQ.2) THEN
        IF (NOVTPS.NE.NVTPS .AND. lnum .NE. fno .AND. lnum .NE. lastno )
     &      THEN
           PRINT *,' NON-EQUAL NUMBER OF VELOCITY-TIME PAIRS',
     &             ' TUPLE INTERPOLATION IS NOT VALID'
          print  *, 'lnum = ',lnum,' fno = ',fno,' lastno = ',lastno
          print  *, 'novtps = ',novtps,' nvtps = ',nvtps
        ENDIF
        IF( lnum .EQ. lastno ) THEN
            DO i = 1, novtps
               curvtp(i) = oldvtp(i)
            ENDDO
        ELSEIF ( lnum .EQ. fno ) THEN
c****       since it is the first trace of the first shot
            DO i = 1, novtps
               curvtp(i) = scr(npars+i)
            ENDDO
        ELSE
            DO I = 1, NOVTPS, 2
               VL = OLDVTP(I)
               TL = OLDVTP(I+1)
               VNEXT = SCR(I+npars)
               TNEXT = SCR(I+npars+1)
               CURVTP(I)   = (VNEXT-VL) * RATIO + VL
               CURVTP(I+1) = (TNEXT-TL) * RATIO + TL
            ENDDO
        ENDIF
c
        ISPAT = 1
      ENDIF
C****
C****      BUILD A VELOCITY FUNCTION SO THAT THERE IS A VELOCITY FOR 
C****      EVERY TIME SAMPLE
C****
 2000 CONTINUE
      IF( IADDWB .NE. 0 ) THEN
          WBT=BUF(50)                                                   ! THE WATER BOTTOM TIME OF THIS TRACE IN SECONDS
          DO I = 2,NVTPS,2
             CURVTP(I)=CURVTP(I)+WBT
          ENDDO
      ENDIF
      LLNUM=LNUM
      IF( NVTPS .EQ. 2 ) THEN                                           ! IF ONLY 1 PAIR, IT'S A CONSTANT VELOCITY
          CURVTP(3) = CURVTP(1)                                         ! IVELT DEMANDS 2 VTP PAIRS
          CURVTP(4) = 100.
          NVTPS = 4
      ENDIF
c****
c****  Convert interval velocities to rms using Dix
c****
      IF( itype .EQ. 5 ) THEN
          CALL int2rms( curvtp, a(nextad), nvtps )
c          print *,' ivtp:',(curvtp(i),i=1,nvtps)
c          print *,' vtp:',(a(i),i=nextad,nextad+nvtps-1)
          a(nextad+nvtps+1) = -1.
          a(nextad+nvtps+2) = -1.
          a(nextad+nvtps+3) = -1.
          CALL ivelt( a(nextad), scr, delay, si, nsamps )
      ELSE
          ITEMP = NVTPS
          CURVTP(ITEMP+1)=-1.                                           ! IVELT WANTS VTP'S TO END WITH A -1
          CURVTP(ITEMP+2)=-1.
          CURVTP(ITEMP+3)=-1.
          CALL IVELT(CURVTP,SCR,DELAY,SI,NSAMPS)                        ! BUILD IT INTO ARRAY SCR
      ENDIF
      IF(IUSEAP.NE.0) CALL APPUT(SCR,IVELAD,NSAMPS,2)                   !  SEND VELS TO AP LOCATION IVELAD
      IF( IAND(LPRINT,2) .NE.0 ) THEN
          IF( itype .NE. 5 ) THEN
              PRINT 2110,LNUM,RANGE,(CURVTP(I),I=1,NVTPS)
          ELSE
              PRINT 2110,LNUM,RANGE,(a(i),i=nextad,nextad+nvtps-1)
          ENDIF
 2110 FORMAT(' THE VELOCITIES AT ',I10,' RANGE ',F10.4,' ARE:',/,
     *      50(1X,F10.4))
          IF( lunvtp .NE. 0 ) THEN
              IF( nsegyfile(nmonum) .EQ. 0) THEN
                  WRITE( lunvtp, 2132 ) lnum, range
 2132             FORMAT(' Velocities at ',I8,' range ',I8,' are:')
                  DO i = 1, nsamps
                     WRITE( lunvtp, '(F14.6)' ) scr(i)
                  ENDDO
              ELSEIF( nsegyfile(nmonum) .EQ. 1 ) THEN
                  CALL wrdisc( lunvtp, buf, 60)
                  CALL wrdisc( lunvtp, scr, nsamps )
              ENDIF
          ENDIF
      ENDIF
      IF( IUSEAP .EQ. 0 ) THEN
          J = IVELAD-1
          DO 2115 I=1,NSAMPS                                            ! NMONAP WANTS THE SQUARE OF THE VELOCITY HELD
 2115     A(J+I) = SCR(I)*SCR(I)                                        ! SAVE THE VELOCITY SQUARED FOR NMONAP
      ENDIF
C****
C****     CALCULATE AND APPLY THE NMO - THE CALCULATION IS IN THE AP AND
C****  THE TRACE AND THE APPLICATION OF NMO IS IN HOST MEMORY
C****
 2120 CONTINUE
c  TYPE   - The type of travel time correction to apply.
c         =1,  Normal MoveOut or NMO: T(0) = SQRT(T(X)**2 - X**2/V(T)**2)
c         =2,  MoveIn or denmo: T(X) = SQRT(T(0)**2 + X**2/V(T)**2)
c         =3,  Slant MoveOut or SMO:  T(0) = T(X) + X/V
c         =4,  XMO
c         =5,  NMO with interval velocities
      jtype = itype
      IF( jtype .EQ. 4 .OR. jtype .EQ. 5 ) jtype = 1
 2121 CONTINUE
      RANGE=RANGE*RANGE                                                 !  NMOVFC WANTS THE RANGE SQUARED
      MSAMPS=NSAMPS
      ODELAY=DELAY
      IF( IUSEAP .NE. 0 ) THEN                                          ! =0 MEANS NO AP
          S(4) = SI/2.+SI                                               ! FAKE NMOVFC INTO CONVERTING TIMES TO INDEXES!
          CALL APPUT(S,NEXTAD,4,2)
          IT2 = NEXTAD+10
          CALL APWD                                                     ! WAIT FOR DATA TRANSFER TO FINISH
          CALL NMOVFC(IT2,NEXTAD,IVELAD,IN,IN,NSAMPS)                   ! COMPUTE THE NMO
      ELSE
          IF( IAND(LPRINT,2) .NE. 0 ) 
     *         PRINT 2131, RANGE,DELAY,SI,NSAMPS,IVELAD
 2131          FORMAT(' ARGUMENTS TO NMONAP:',/,3(1X,F15.6),2(1X,I8))
          CALL NMONAP(RANGE,DELAY,SI,NSAMPS,IVELAD,LSCR, jtype, 
     &         a(nextad) )
      ENDIF
C****
C****    WHILE WE'RE WAITING FOR THE CALCULATIONS, CORRECT THE MUTE
C****  TIMES FOR NMO
C****
      LLNUM=LNUM
c      SMUTE=BUF(47)                                                     ! THE START MUTE TIME IN SECONDS
c      EMUTE=BUF(48)                                                     !  THE END MUTE TIME IN SECONDS
      smute = ibuf(56) / 1000.
      emute = ibuf(57) / 1000.
      IF(SMUTE.LE.DELAY) GO TO 2150                                     ! IF IT IS ALREADY BEFORE START OF DATA
      CALL FINDV(SMUTE,CURVTP,NVTPS,V)
      TEMP=SMUTE*SMUTE-RANGE/(V*V)                                      ! RANGE IS ALREADY SQUARED
      IF(TEMP.LT.0.) TEMP=0.                                            ! SQRT CAN'T BE <0
      SMUTE=SQRT(TEMP)
      IF(SMUTE.LT.DELAY) SMUTE=DELAY
      IBUF(56)= NINT( SMUTE*1000. )     ! CONVERT TO MILS
 2150 IF(EMUTE.LE.DELAY) GO TO 2160                                     ! DON'T DO IT IF NOT END MUTE!!
      CALL FINDV(EMUTE,CURVTP,NVTPS,V)
      TEMP=EMUTE*EMUTE-RANGE/(V*V)
      IF(TEMP.LT.0.) TEMP=0.
      EMUTE=SQRT(TEMP)
      IF(SMUTE.LT.DELAY) EMUTE=DELAY
c      BUF(48)=EMUTE
      IBUF(57)=NINT( EMUTE*1000. )
 2160 CONTINUE
      ISTRET=STRETC/SI
c****
C****    NOW APPLY THE NMO
C****
      IF( IUSEAP .EQ. 1 ) THEN
          CALL APWR                                                     ! WAIT FOR THE COMPUTATIONS TO FINISH
          CALL APGET(LSCR,IN,NSAMPS,0)                                  ! GET THE INDEXES
          CALL APWD                                                     ! WAIT FOR THE INDEXES TO FINISH TRANSFERING FROM THE AP
      ENDIF
c**** NMONAP returned the REAL TX array in a(nextad)  and the indices in LSCR
c          CALL NMONAP(RANGE,DELAY,SI,NSAMPS,IVELAD,LSCR, jtype, a(nextad) )
c  save the input in scratch and grab the output from scr - inversion might not be as bad
      indata = nextad + nsamps
      DO i = 1, nsamps
         a(indata+i) = buf(numhdr+i)
      ENDDO
      IF( hires .EQ. 0 ) THEN
c****     CALL nmoapp( datain, dataout, indices, start index, 
          CALL nmoapp( a(indata+1), buf(numhdr+1), lscr, 1, nsamps,
     &       istret, mute, jtype )
      ELSE
          indextx = indata + nsamps
          txfirst = delay * si
c      PARAMETER (N2 = NTERPOLATE / 2)
          txlast = txfirst + (nsamps-nterpolate)*si
          DO i = 1, nsamps
             a(indextx+i) = FLOAT(lscr(i)-1) * si + delay
          ENDDO
          DO i = 1, n2
             buf(numhdr+i) = 0.
          ENDDO
          DO i = n2+1, nsamps-n2
             IF( a(indextx+i) .GT. txlast ) THEN
                 buf(numhdr+i) = 0.
             ELSEIF( a(indextx+i) .LT. txfirst ) THEN
                 buf(numhdr+i) = 0.
             ELSE
c  a(indextx) has the time of the nearest sample of the TX we want
c  a(nextad) has the times we really want
c  a(indata) has the trace
                 DO j = 1, nterpolate
                    txtimes(j) = a(indextx+i) + (j-1) * si + delay
                 ENDDO
                 IF( a(nextad+i-n2) .GT. txtimes(1) ) THEN
                     CALL polint( txtimes, a(indata+lscr(i)), nterpolate, 
     &                 nterpolate, a(nextad+i-n2), 
     &                    buf(numhdr+i-n2), temp )
                 ELSE
                     DO j = 1, nterpolate
                        txtimes(j) = a(indextx+i-1) + (j-1) * si + delay
                     ENDDO
                     CALL polint( txtimes, a(indata+lscr(i-1)),
     &                 nterpolate, a(nextad+i-n2), 
     &                    buf(numhdr+i-n2), temp)
                 ENDIF
             ENDIF
          ENDDO
          IF( stretc .LT. delay + nsamps*si ) THEN
              DO i = 1, nsamps
                 IF( (a(nextad+i) - (delay + i*si)) .GT. stretc )
     &                buf(numhdr+i) = 0.
              ENDDO
          ENDIF
      ENDIF
c****
c**** Do DSTRETCH here
c****
      IF( dstretch .NE. 0 ) THEN
          DO i = 1, nsamps
             tx = FLOAT(lscr(i)-1) * si + delay
             t0 = delay + (i-1)*si
             IF( ((tx-t0) / t0) .GT. dstretch ) buf(numhdr+i) = 0.
          ENDDO
      ENDIF
c
c**** itype 4 is nmo followed by move-in, so after nmo change itype to
c*** move-in and change the range to the new range and do it over!
      IF( itype .EQ. 4 .AND. jtype .EQ. 1 ) THEN
          jtype = 2
          range = newx
          lbuf(10) = newx
          GOTO 2121
      ENDIF
      MUTE=MUTE+IDELAY
      TEMP=MUTE*SI                                                      ! TAKE CARE OF THE MUTE FROM STRETCH
      IF( TEMP .GT. EMUTE ) THEN
c          BUF(48)=TEMP                                                      ! JUST PUT IT IN THE HEADER
          IBUF(57)=MUTE                                                     ! NMOAPP ACTUALLY ZEROED THE DATA!!!
      ENDIF
      in=0                                                              ! tell the next process that the data is not in the ap
      IF( IADDWB .NE. 0 ) THEN
          DO 2200 I=2,NVTPS,2
 2200     CURVTP(I)=CURVTP(I)-WBT
      ENDIF
      RETURN
      END
