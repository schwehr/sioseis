      SUBROUTINE WEIGEX(BUF,LBUF,IBUF,SCR,LSCR)
C     WEIGEX IS THE EXECUTION PHASE OF THE SEISMIC PROCESS WEIGHT, WHICH WEIGHTS
C  (SCALAR MULTIPLIES) A TRACE.  THE WEIGHT PARAMETERS MUST BE ON DISC FILE MUNIT
C  AND THE TRACE (WITH HEADER) MUST BE IN MEMORY LOCATION BUF.
C     SUBROUTINE WEIGED CONTAINS THE EXPLANATION OF THE USER PARAMETERS AND THE
C  ORDER OF THE USER PARAMETERS ON DISC.
C
C  ARGUMENTS:
C  BUF    - THE TRACE TO BE WEIGHTED, INCLUDING THE TRACE HEADER.  THE FIRST
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
C
C  COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, NOVEMBER 1980
c
c mod 21 Feb 96 - record weight didn't work when ap assigned but data not in ap.
c mod 12 Aug 92 - multiple lists didn't work correctly when skipping records
c mod 7 March 91 to add parameter weight (record weight - all traces)
c modified 27 nov 89 - fill rather than multiply when weight is 0., because
c                      NaN * weight = NaN
c mod 22 Sep 99 - Add IHDR, LHDR, HDR, INVERSE
c mod Apr 06 - Fedora refuses to GOTO into the middle of another block
C
C
      PARAMETER (MAXTWP=200)                                            ! THE MAXIMUM NUMBER OF ELEMENTS THE USER ARRAY CAN BE
      DIMENSION BUF(111),LBUF(111),IBUF(111),SCR(111),LSCR(111)
      INTEGER*2 IBUF
      DIMENSION OLDTWP(MAXTWP),CURTWP(MAXTWP)
      COMMON /WEIGR/ MUNIT,NLISTS, npars, nwrds
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP
      COMMON /APMEM/ A(32766)
      COMMON /READT/ ILUN,NUMHDR
      INTEGER FNO, hdr
      LOGICAL FIRST
      SAVE
      DATA FIRST /.TRUE./, lastno/0/
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      ISIG=0
      IF(IBUF(15).EQ.2) RETURN                                          ! IS IT A DEAD TRACE
   10 CONTINUE                                                      ! GET THE FIRST PARAMETER LIST INT0 MEMORY ARRAY SCR
      IF(  FIRST ) THEN
          FIRST=.FALSE.
          CALL PODISC(MUNIT,1,0)
          CALL RDDISC(MUNIT,SCR,NWRDS,ISTAT)
          ISIG=1
          FNO=LSCR(1)
          LNO=LSCR(2)
          IADDWB=LSCR(3)
          NTWPS=LSCR(4)
          LTYPE=LSCR(5)
          lprint=LSCR(6)
          rweight1 = scr(7)
          ihdr = lscr(8)
          lhdr = lscr(9)
          hdr = lscr(10)
          inverse = lscr(11)
          itype = lscr(12)
          IF( ntwps .GT. 0 ) THEN
              DO I = 1, ntwps
                 CURTWP(I) = SCR(I+npars)
              ENDDO
              IF( inverse .EQ. 1 ) THEN
                  DO i = 2, ntwps, 2
                     IF( curtwp(i) .NE. 0. ) curtwp(i) = 1. / curtwp(i)
                  ENDDO
              ENDIF
          ENDIF
          IF( inverse .EQ. 1 .AND. rweight1 .NE. 0. )
     &        rweight1 = 1. / rweight1
          MLISTS=1
      ENDIF
      LNUM=LBUF(3)                                                      !  IS THE DATA ON TAPE SORTED BY SHOT
      TRNO=LBUF(4)                                                      ! THE TRACE NUMBER WITHIN THE SHOT
      IF( LBUF(7) .NE. 0 ) THEN
          TRNO=LBUF(7)                                                  ! THE TRACE NUMBER WITH AN RP
          LNUM=LBUF(6)                                                  !  OR BY RP
      ENDIF
   70 CONTINUE
      IF(LNUM.GE.FNO) GO TO 100                                         ! IS THIS SHOT BEFORE THIS PARAMETER LIST
      IF(MLISTS.EQ.1) RETURN                                            ! IS IT BEFORE THE FIRST LIST
      IF(LNUM.LE.LASTNO) THEN                                           ! IS IT IN OR BEFORE THE LAST LIST
         FIRST = .TRUE.
         GOTO 10
      ENDIF
      RETURN                                                            ! IF THE SHOT IS NOT SPECIFIED, LEAVE IT ALONE
  100 CONTINUE                                                          !  THE CURRENT SHOT (RP) IS >= LNO
      IF(LNUM.LE.LNO) GO TO 500                                         ! USE THE PARAMETERS OF THIS LIST
      IF(MLISTS.GE.NLISTS) RETURN                                       ! ANY MORE USER PARAM LISTS ON DISC
C****
C****   GET ANOTHER USER PARAMETER LIST FROM DISC
C****
      DO 120 I=1,MAXTWP                                                 ! SAVE THE CURRENT PARAMETER SET
  120 OLDTWP(I)=SCR(I+npars)
      LASTNO=LNO
      NOTWPS=LSCR(4)
      ISIG=1
      MLISTS=MLISTS+1
      CALL RDDISC(MUNIT,LSCR,NWRDS,ISTAT)
      FNO=LSCR(1)
      LNO=LSCR(2)
      IADDWB=LSCR(3)
      NTWPS=LSCR(4)
      LTYPE=LSCR(5)
      lprint=LSCR(6)
      rweight1 = scr(7)
      ihdr = lscr(8)
      lhdr = lscr(9)
      hdr = lscr(10)
      inverse = lscr(11)
      IF( ntwps .GT. 0 ) THEN
          DO I = 1, ntwps
             CURTWP(I) = SCR(I+npars)
          ENDDO
          IF( inverse .EQ. 1 ) THEN
              DO i = 2, ntwps, 2
                 IF( curtwp(i) .NE. 0. ) curtwp(i) = 1. / curtwp(i)
              ENDDO
          ENDIF
      ENDIF
      IF( inverse .EQ. 1 .AND. rweight1 .NE. 0. )
     &    rweight1 = 1. / rweight1
      GO TO 70
C****
C****    NOW FIND THE WEIGHT FOR THE  TRACE NUMBER (OR RANGE)
C****
  500 IF( ISIG .NE. 0 ) THEN                                            ! IS A LIST IN SCR
          IADDWB=LSCR(3)
          NTWPS=LSCR(4)
          LTYPE=LSCR(5)
          lprint=LSCR(6)
          rweight1 = scr(7)
          ihdr = lscr(8)
          lhdr = lscr(9)
          hdr = lscr(10)
          inverse = lscr(11)
          itype = lscr(12)
          IF( ntwps .GT. 0 ) THEN
              DO I = 1, ntwps
                 CURTWP(I) = SCR(I+npars)
              ENDDO
              IF( inverse .EQ. 1 ) THEN
                  DO i = 2, ntwps, 2
                     IF( curtwp(i) .NE. 0. ) curtwp(i) = 1. / curtwp(i)
                  ENDDO
              ENDIF
          ENDIF
          IF( inverse .EQ. 1 .AND. rweight .NE. 0. )
     &        rweight = 1. / rweight
      ENDIF
      rweight = rweight1
      IF(LNUM.LT.FNO.OR.LNUM.GT.LNO) RETURN                             ! DOUBLE CHECK THAT WE GOT THE RIGHT LIST
      NSAMPS=IBUF(58)
      IF( ihdr .NE. 0 ) THEN
          temp = REAL(ibuf(ihdr))
          IF( inverse .EQ. 1 ) temp = 1. / temp
          rweight = rweight * temp
      ENDIF
      IF( lhdr .NE. 0 ) THEN
          temp = FLOAT(lbuf(lhdr))
          IF( inverse .EQ. 1 ) temp = 1. / temp
          rweight = rweight * temp
      ENDIF
      IF( hdr .NE. 0 ) THEN
          temp = buf(hdr)
          IF( inverse .EQ. 1 ) temp = 1. / temp
          rweight = rweight * temp
      ENDIF
c****
c****   Do the Statistical stuff first
c****
      IF( itype .EQ. 1 ) THEN
          IF( in .EQ. 0 ) THEN
              CALL moment( buf(numhdr+1), nsamps,
     &             ave, adev, sdev, var, skew, curt )
          ELSE
              CALL moment( a(in), nsamps,
     &             ave, adev, sdev, var, skew, curt )
          ENDIF
          IF( IAND(lprint,16) .NE. 0 ) THEN
              PRINT *,' Record',lnum,' trace',trno,' statistics are:',
     &        ' ave=',ave,' adev=',adev,' sdev=',sdev,' var=',var,
     &        ' skew=',skew,' curt=',curt,' weight=',1./sdev
          ENDIF
          IF( ave .NE. 0 ) rweight = rweight / sdev
      ENDIF
c****
c****  Do the parameter WEIGHT (rweight = record weight = all traces)
c****
      IF( rweight .NE. 1. ) THEN
          IF( rweight .EQ. 0. ) ibuf(15) = 2
          IF( iuseap .EQ. 0 ) THEN                                      ! =0 if no ap
              IF( in .EQ. 0 ) THEN                                      ! = 0 if not in ap simulator or ap
                  IF( rweight .NE. 0. ) THEN
                      DO 502 i = 1, nsamps
  502                 buf(numhdr+i) = buf(numhdr+i) * rweight
                  ELSE
                      DO 503 i = 1, nsamps
  503                 buf(numhdr+i) = 0.
                  ENDIF
              ELSE
                  j = in -1
                  IF( rweight .NE. 0. ) THEN
                      DO 504 i = 1, nsamps
  504                 a(j+i) = a(j+i) * rweight
                  ELSE
                      DO 505 i = 1, nsamps
  505                 a(j+i) = 0.
                  ENDIF
              ENDIF
          ELSE
              CALL INAP(BUF(NUMHDR+1),NSAMPS)
              CALL APWR
              CALL APPUT(RWEIGHT,NEXTAD,1,2)
              CALL APWD
              CALL VSMUL(IN,1,NEXTAD,IN,1,NSAMPS)
          ENDIF
      ENDIF
c****
c****    Take care of the trace weights (TWP and XWP)
c****
      weight = 1.
      IF( ltype .EQ. 0 ) GOTO 590
      I=1
      IF(LTYPE.EQ.2) GO TO 507
      TEMP=LBUF(10)                                                     ! TRNO=ABS(LBUF(10) DOESN'T WORK!!!
      TRNO=ABS(TEMP)                                                    ! SET THE TRACE NO TO THE RANGE IF XWP WAS GIVEN
  507 CONTINUE
  510 IF(TRNO.EQ.CURTWP(I)) GO TO 550
C**** WATCH OUT FOR ROUND OFF PROBLEMS ON THE RANGE
      IF(TRNO.GT.CURTWP(I)-.0001.AND.TRNO.LT.CURTWP(I)+.0001) GO TO 550
      I=I+2
      IF(I.LE.NTWPS) GO TO 510
      GOTO 590
  550 WEIGHT=CURTWP(I+1)
      IF(WEIGHT.EQ.0.) IBUF(15)=2
      IF(IUSEAP.NE.0.AND.IASGND.NE.0) GO TO 580
      IF(IN.NE.0.AND.IUSEAP.EQ.0) GO TO 570
      IF( weight .NE. 0. ) THEN
          DO 560 ii = 1, nsamps
  560     BUF(NUMHDR+II) = BUF(NUMHDR+II)*WEIGHT
      ELSE
          DO 565 ii = 1, nsamps
  565     buf(numhdr+ii) = 0.
      ENDIF
      GO TO 590
  570 J=IN-1                                                            ! THE DATA IS IN APMEM
      IF( weight .NE. 0. ) THEN
          DO 575 II=1,NSAMPS
  575     A(J+II)=A(J+II)*WEIGHT
      ELSE
          DO 576 ii = 1, nsamps
  576     a(j+ii) = 0.
      ENDIF
      GO TO 590
  580 CALL INAP(BUF(NUMHDR+1),NSAMPS)
      CALL APWR
      CALL APPUT(WEIGHT,NEXTAD,1,2)
      CALL APWD
      CALL VSMUL(IN,1,NEXTAD,IN,1,NSAMPS)
  590 IF( IAND(lprint,8) .NE. 0 ) THEN
          x = 1.
          IF( weight .NE. 1. ) x = weight
          IF( rweight .NE. 1. ) x = rweight
          IF( weight .NE. 1. .AND. rweight .NE. 1.) x = weight * rweight
          IF( x .NE. 1. ) PRINT 600,LNUM,TRNO, x
  600    FORMAT(' RECORD ',I10,' TRACE ',F9.3,' IS MULTIPLIED BY ',G8.3)
      ENDIF
      RETURN
      END
