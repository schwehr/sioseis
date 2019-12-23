      SUBROUTINE SHFTEX(BUF,LBUF,IBUF,SCR,LSCR)
C     SHFTEX IS THE EXECUTION PHASE OF THE SEISMIC PROCESS SHIFT, WHICH SHIFTS
C  A TRACE IN TIME.  THE SHIFT PARAMETERS MUST BE ON DISC FILE MUNIT
C  AND THE TRACE (WITH HEADER) MUST BE IN MEMORY LOCATION BUF.
C     SUBROUTINE SHFTED CONTAINS THE EXPLAINATION OF THE USER PARAMETERS AND THE
C  ORDER OF THE USER PARAMETERS ON DISC.
C
C  ARGUMENTS:
C  BUF    - THE TRACE TO BE SHIFTED, INCLUDING THE TRACE HEADER.  THE FIRST
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
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, DECEMBER 1980
C  ALL RIGHTS RESERVED BY THE AUTHOR.
C
c  19 Jan 93 - Correct reduction velocity shift when the shift < delay
c  23 June 1993 - Add lag parameters (time break shift)
c  20 Dec 93 - Correct the sign of the shift in the header when reduce yes
c  23 Oct 96 - Do datum correction shifts
c  23 Jan 97 - ajh/pch, bad header value when non integral mils.
c  28 Jan. 97 - Add interp for XSP only
c  4 Aug. 98 - Add indices
c  Feb. 99 - lprint not initialized when first trace is dead
c  26 Jun 01 - Allow spatial variation of rshift and tsp/xsp/gsp
c  28 May 04 - lshift wasn't working right.
c  20 May 05 - Add SEGY "Datum elevation at receiver" and "Datum elevation
c              at shot" when doing datum corrections.
c            - Honor the SEGY elevation scalar.
c  4 Feb 10 - Change an lprint statement
c  11 Feb 10 - datum correction had the sign backwards
c   mod 17 May 10 - Comment out creating real mute times in header words 47 & 48
c  18 Feb 11 - scalar in datum correction waqs wrong
c            - also the Feb 10 mod was wrong
C
      PARAMETER (MAXTSP=2600)                                           ! THE MAXIMUM NUMBER OF ELEMENTS THE USER ARRAY CAN BE
      PARAMETER ( NPARS = 19 )                                          ! THE LENGTH OF EACH PARAMETER LIST
      DIMENSION BUF(1111),LBUF(1111),IBUF(1111),SCR(1111),LSCR(1111)
      INTEGER list1 (NPARS), list2 (NPARS)
      REAL rlist1 (NPARS), rlist2(NPARS)
      EQUIVALENCE (list1(1),rlist1(1)), (list2(1),rlist2(1))
      INTEGER*2 IBUF
      DIMENSION CURTSP(MAXTSP)
      DIMENSION tspnew(maxtsp), tspnew1(maxtsp), tspnew2(maxtsp)
      COMMON /SHIFT/ MUNIT,NLISTS
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP
      COMMON /APMEM/ A(32766)
      COMMON /READT/ ILUN,NUMHDR
      INTEGER fno1, fno2
      REAL lshift
      SAVE
      LOGICAL FIRST
      INTEGER indices(20)
      DATA FIRST /.TRUE./, lastno/0/, fno2/9999999/
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      ISIG=0
      IF(IBUF(15).EQ.2) RETURN                                          ! IS IT A DEAD TRACE
      IF( .NOT. FIRST ) GOTO 200
c****
c**** Get the first parameter list into rlist2 and variables.
c****
      FIRST = .FALSE.
      rshift = 0.
      tshift = 0.
      ntsps = 0
      CALL PODISC(MUNIT,1,0)                                        ! REWIND THE PARAMETER IDSC FILE
      CALL RDDISC(MUNIT, rlist2, npars, istat )
      nnew = list2(5)
      nindices = list2(17)
      IF( nnew .NE. 0 ) CALL rddisc( munit, tspnew2, nnew, istat)
      IF( nindices .GT. 0 ) CALL rddisc( munit, indices, 20, istat )
      MLISTS=1
c****
c**** copy the second list to the first and try to get another.
c****
  100 CONTINUE
      lshift = rlist2(1)
      rsnew = rlist2(2)
      fno1 = list2(3)
      lno1 = list2(4)
      nnew1 = list2(5)
      ltnew = list2(6)
      lprint = list2(7)
      redvel = rlist2(8)
      appvel = rlist2(9)
      velh2o = rlist2(10)
      ireduce = list2(11)
      laga = list2(12)
      lagb = list2(13)
      datume = rlist2(14)
      datumv = rlist2(15)
      interp = list2(16)
      nindices = list2(17)
      smult = rlist2(18)
      DO i = 1, npars
         rlist1(i) = rlist2(i)
      ENDDO
      DO i = 1, nnew
         tspnew1(i) = tspnew2(i)
         tspnew(i) = tspnew2(i)
      ENDDO
      IF( mlists .GE. nlists ) THEN
          fno2 = 9999999
          IF( interp .EQ. 1 ) lno1 = 9999999
          GOTO 200
      ENDIF
      CALL rddisc( munit, rlist2, npars, istat )
      fno2 = list2(3)
      lno2 = list2(4)
      nnew2 = list2(5)
      IF( list2(5) .NE. 0 ) CALL rddisc( munit, tspnew2, nnew2, istat )
      IF( list2(17) .NE. 0 ) CALL rddisc( munit, indices, 20, istat )
      mlists = mlists + 1
c****
c****  Get the shot/rp number from the trace header
c****
  200 CONTINUE
      LNUM=LBUF(3)                                                      !  IS THE DATA ON TAPE SORTED BY SHOT
      TRNO=LBUF(4)                                                      ! THE TRACE NUMBER WITHIN THE SHOT
      IF(LBUF(7).NE.0) TRNO=LBUF(7)                                     ! THE TRACE NUMBER WITH AN RP
      IF(LBUF(7).NE.0) LNUM=LBUF(6)                                     !  OR BY RP
      IF( lnum .EQ. lastno ) GOTO 506
      rshift = 0.
      ntsps = 0
      IF( lnum .LT. fno1 .AND. interp .EQ. 0 ) GOTO 506
      IF( lnum .LE. lno1 ) GOTO 500
      IF( lnum .LT. fno2 .AND. interp .EQ. 0 ) RETURN
      IF( lnum .LT. fno2 .AND. interp .EQ. 1 ) GOTO 300
c**** it must be GE. fno2, so move list2 to list1 and use it
      GOTO 100
c****
c****  If it's between the two lists, spatially interpolate
c****
  300 CONTINUE
      IF( lnum .LT. fno2 .AND. interp .EQ. 1 ) THEN
c****    this shot/rp is between two lists, so interpolate
          ratio = FLOAT(lnum - lno1) / FLOAT(fno2 - lno1)
          rsnew = ratio * (rlist2(2) - rlist1(2)) + rlist1(2)
          IF( nnew .NE. 0) THEN
              DO i = 1, nnew
                 tspnew(i) = ratio * (tspnew2(i) - tspnew1(i))
     &                     + tspnew1(i)
              ENDDO
          ENDIF
      ELSE
          rsnew = list1(2)
          nnew = list1(5)
          IF( nnew .NE. 0) THEN
              DO i = 1, nnew
                 tspnew(i) = tspnew1(i)
              ENDDO
          ENDIF
      ENDIF
C****
C****    NOW FIND THE SHIFT FOR THE  TRACE NUMBER (OR RANGE)
C****
  500 CONTINUE
      lastno = lnum
      tshift = 0.
      rshift = rsnew
      ntsps = nnew
      ltype = ltnew
      DO 505 i = 1, ntsps
  505 curtsp(i) = tspnew(i)
  506 tshift = 0.
      IF( ntsps .EQ. 0 ) GOTO 550
      IF( LTYPE .EQ. 1) THEN                                            ! 1=XSP, 2=TSP, 3=gsp
         TEMP=LBUF(10)                                                  ! TRNO=ABS(LBUF(10) DOESN'T WORK!!!
         TRNO=ABS(TEMP)                                                 ! SET THE TRACE NO TO THE RANGE IF XSP WAS GIVEN
      ENDIF
      IF( ltype .EQ. 1 ) THEN
          IF( interp .EQ. 0 ) THEN
              DO i = 1, ntsps, 2
                 IF( trno .EQ. curtsp(i) ) THEN
                     tshift = curtsp(i+1)
                     GOTO 550
                 ENDIF
              ENDDO
          ELSE
              DO i = 1, ntsps, 2
                 tshift = curtsp(i+1)
                 IF( trno .LE. curtsp(i) ) GOTO 550
                 IF( i+1 .EQ. ntsps ) GOTO 550
                 IF( trno .LT. curtsp(i+2) ) THEN
                     temp = (trno - curtsp(i)) /(curtsp(i+2)-curtsp(i))
                     tshift = temp*(curtsp(i+3)-curtsp(i+1))+curtsp(i+1)
                     GOTO 550
                 ENDIF
              ENDDO
          ENDIF
      ENDIF
      IF( ltype .EQ. 2 ) THEN
          DO 510 i = 1, ntsps, 2
             IF( trno .EQ. curtsp(i) ) THEN
                 tshift = curtsp(i+1)
                 GOTO 550
             ENDIF
  510     CONTINUE
      ENDIF
      IF( ltype .EQ. 3 ) THEN                                           !  GSP
          DO 520 i = 1, ntsps, 2                                        ! find this trace number in the gsp listw
             tshift = curtsp(i+1)
             IF( trno .LE. curtsp(i) ) THEN
                 IF( i .NE. 1 ) THEN
                    tshift = (trno-curtsp(i-2))/(curtsp(i)-curtsp(i-2))*
     *                  (curtsp(i+1)-curtsp(i-1))+curtsp(i-1)           ! interpolate
                 ENDIF
                 GOTO 550
             ENDIF
  520    CONTINUE
      ENDIF
  550 CONTINUE
C****
C****  Take care of the reduction velocity here
C****
      IF( ltype .EQ. 4 ) THEN
          delay = buf(46)
          range = ABS(FLOAT(lbuf(10)))
          redshift = range / redvel
          IF( redshift .GE. delay ) THEN
              tshift = tshift - redshift + delay
              buf(46) = 0.
          ELSE
              tshift = 0.
              buf(46) = delay - redshift
          ENDIF
          ibuf(55) = NINT(buf(46)*1000.)
          IF( IAND(lprint,2) .NE. 0 ) PRINT *,' shot',lnum,' range',
     &        range,' redvel',redvel,' shift',redshift,' tshift=',
     &        tshift,' (46)=',buf(46)
      ENDIF
c****
c****  Take care of "projection" shift  (appvel and velh2o)
c****
      IF( appvel .GT. 0.) THEN
          xsin = velh2o / appvel
          xcos = SQRT(1. - xsin*xsin)
          range = lbuf(10)
          wdepth = FLOAT(lbuf(16))
          rangecor = (xsin/xcos) * wdepth
          IF( range .GT. 0. ) lbuf(10) = lbuf(10) - NINT(rangecor)
          IF( range .LT. 0. ) lbuf(10) = lbuf(10) + NINT(rangecor)
          timecor = -wdepth / (velh2o * xcos )
          tshift = tshift + timecor
          IF( IAND(lprint,2) .NE. 0 ) PRINT *,' shot',lnum,' range',
     &        range,' rangecor',rangecor,' shift',timecore
      ENDIF
c****
c****   LAG shifts here
c****
      IF( laga .NE. 0 ) tshift = tshift - REAL(ibuf(53)) / 1000.
      IF( lagb .NE. 0 ) tshift = tshift - REAL(ibuf(54)) / 1000.
c****
c****   DATUM shifts here.
c****   receiver = (group elevation - datum elevation) / datum velocity
c****   source = ((source elevation - source depth) - datum elevation) / datumv
c****
      IF( datume .NE. -99999. .AND. datumv .NE. -99999. ) THEN
          scalar = 1.
          IF( ibuf(35) .NE. 0 ) scalar = REAL(ibuf(35))
          IF( scalar .LT. 0. ) scalar = -1. / scalar
          relev = FLOAT(lbuf(11)) * scalar
          selev = FLOAT(lbuf(12)) * scalar
          sdepth = FLOAT(lbuf(13)) * scalar
          rdatum = FLOAT(lbuf(14)) * scalar
          sdatum = FLOAT(lbuf(15)) * scalar
          receiver = - (relev - datume) / datumv
          source = - ((selev - sdepth) - datume)/datumv
          tshift = tshift + source + receiver
          IF( IAND(lprint,4) .NE. 0 ) THEN
              PRINT *,' shot',lbuf(3),' tr',lbuf(4),' rp',lbuf(6),' tr',
     &                lbuf(7)
              PRINT *,' source leg=',source,' receiver leg=',receiver,
     &                 ' total=',tshift

              PRINT *,(lbuf(i),i=11,15),datume
          ENDIF
          lbuf(14) = datume                                             ! Datum elevation at receiver group.
          lbuf(15) = datume                                             ! Datum elevation at source
      ENDIF
c****
c****   Shift by the sum of SEG-Y header locations/values
c****
      IF( nindices .GT. 0 ) THEN
          DO i = 1, nindices, 2
             IF( indices(i) .EQ. 1 ) tshift = tshift +
     &           REAL(ibuf(indices(i+1))) * smult
             IF( indices(i) .EQ. 2 ) tshift = tshift +
     &           FLOAT(lbuf(indices(i+1))) * smult
             IF( indices(i) .EQ. 3 ) tshift = tshift +
     &           buf(indices(i+1)) * smult
          ENDDO
      ENDIF
C****
C****  COMPUTE THE SHIFT IN MICROSECONDS TO PREVENT FLOATING POINT INACCURACIES`~
C****
      SHIFT = tshift + RSHIFT + LSHIFT
      IF(SHIFT.EQ.0) RETURN
      SI=BUF(49)
      SI2=SI*.5
      LSI = IBUF(59)                                                    ! MAKE THE SAMPLE INTERVAL INTEGER*4 (IN MICROSECONDS ALREADY)
      SHFT = (SHIFT+SIGN(SI2,SHIFT))*1000000.
      LSHFT = SHFT                                                      ! CONVERT TO I*4
      LSHFT = LSHFT/LSI                                                 ! CONVERT TO SAMPLES
      ISHIFT = LSHFT                                                    ! CONVERT TO I*2
      NSAMPS = IBUF(58)
      M = IABS(ISHIFT)
      N = NSAMPS - M
      IF( N.LE. 0) THEN
          PRINT 580,LBUF(3),LBUF(4),LBUF(6),LBUF(7),ISHIFT,NSAMPS
  580 FORMAT(' ***  WARNING  *** SHOT ',I6,' TRACE ',I6,' RP ',I6,
     *  ' TRACE ',I6,' IS BEING KILLED BY SHIFT.',/,'  THE SHIFT IS ',
     *   I6,' SAMPLES AND THE DATA IS ONLY ',I6,' SAMPLES LONG.')
         IBUF(15)=2                                                     ! KILL IT IN THE HEADER
         GOTO 2020
      ENDIF
      IF(IASGND.NE.0.AND.IUSEAP.NE.0) GO TO 1000                        ! IS THE DATA IN THE AP OR IN HOST MEMORY
C****
C****      THE TRACE IS IN HOST MEMORY - SHIFT IT!!
C****
      IF(ISHIFT.GT.0) GO TO 700                                         ! IS IT A LEFT OR RIGHT SHIFT
      J=NUMHDR+M                                                        ! IT'S A LEFT SHIFT - MOVE IT TO A SMALLER TIME
      IF(IUSEAP.EQ.0.AND.IN.NE.0) GO TO 620
	 IF( IAND(lprint,8).NE.0) PRINT *,' shift of ',m,' done in mem'
      DO 600 I=1,N
  600 BUF(NUMHDR+I)=BUF(J+I)
      J=NUMHDR+N
      DO 610 I=1,M                                                      ! ZERO FILL THE BACK END OF THE SHIFTED TRACE
  610 BUF(J+I)=0.
      GO TO 2000
  620 J=IN-1
      K=J+M
	 IF( IAND(lprint,8) .NE. 0 ) PRINT *,' shift of ',m,' in apsim'
      DO 630 I=1,N
  630 A(J+I)=A(K+I)
      DO 640 I=1,M
  640 A(N+I)=0.
      GO TO 2000
  700 CONTINUE                                                          ! A POSITIVE OR RIGHT SHIFT
      IF(IUSEAP.EQ.0.AND.IN.NE.0) GO TO 730
      J=NUMHDR+NSAMPS+1
      K=J-ISHIFT
      IF( IAND(lprint,8).NE.0) PRINT *,' shift of ',m,' done in mem',
     &   '(',m*si,' secs)'
      DO 710 I=1,N
  710 BUF(J-I)=BUF(K-I)
      DO 720 I=1,ISHIFT                                                 ! ZERO FILL THE FRONT END
  720 BUF(NUMHDR+I)=0.
      GO TO 2000
  730 J=NSAMPS+1                                                        ! THE DATA IS IN APMEM
      K=J-ISHIFT
	 IF( IAND(lprint,8) .NE. 0 ) PRINT *,' shift of ',m,' in apsim'
      DO 740 I=1,N
  740 A(J-I)=A(K-I)
      DO 750 I=1,ISHIFT
  750 A(I)=0.
      GO TO 2000
C****
C****   THE DATA IS IN THE AP
C****
 1000 CONTINUE
      IF( IAND(lprint,8) .NE. 0 ) PRINT *,' shift of ',m,' done in ap'
      CALL INAP(BUF(NUMHDR+1),NSAMPS)
      IF(ISHIFT.GT.0) GO TO 1100                                        ! IS IT A LEFT OR RIGHT SHIFT
      J=IN+M                                                            ! A LEFT OR NEGATIVE SHIFT - THE DATA IS AT AP ADDRESS IN
      CALL VMOV(J,1,IN,1,N)                                             ! MOVE THE DATA TO AN EARLIER TIME
      J=IN+N
      CALL VCLR(J,1,M)                                                  ! ZERO FILL THE BACK END OF THE TRACE
      GO TO 2000
 1100 CONTINUE                                                          ! A POSITIVE SHIFT IN THE AP
      J=IN+NSAMPS-1                                                     ! THE BACK END OF THE INPUT TRACE
      K=J-ISHIFT                                                        ! THE NEW BACK END
      CALL VMOV(K,-1,J,-1,N)                                            ! MOVE IT BACKWARDS
      CALL VCLR(IN,1,ISHIFT)                                            ! ZERO FILL THE FRONT OF THE NEW TRACE
      GO TO 2000
C****
C****    CLEANUP BY MODIFYING THE HEADER MUTE TIMES, THEN PRINT THE SHIFT
C****
 2000 CONTINUE
      shift = FLOAT(ishift) * si                                       ! Convert integer shift in samples to real seconds
      mshift = NINT(shift*1000.)                                       ! convert to integer mils
      IF( IBUF(56) .NE. 0 ) THEN
          IBUF(56)=IBUF(56)+mshift
          IF(IBUF(56).LT.0) IBUF(56)=0
c          BUF(47)=REAL(IBUF(56))/1000.
      ENDIF
      IF( IBUF(57) .NE. 0 ) THEN
          IBUF(57)=IBUF(57)+mshift
          IF(IBUF(57).LT.0) IBUF(57)=0
c          BUF(48)=REAL(IBUF(57))/1000.
      ENDIF
 2020 CONTINUE
      IF( IAND(LPRINT,2) .NE. 0) PRINT 2030,LNUM,TRNO,SHIFT,ISHIFT
 2030 FORMAT(' RECORD ',I10,' TRACE ',F9.3,' IS SHIFTED ',
     *   F10.4,' SECONDS (',I6,' SAMPLES).')
c****
c****   If we are REDUCING, change the delay and the number of samples
c****
      IF( ireduce .EQ. 1 ) THEN
          buf(46) = buf(46) - shift                                     ! delay in seconds
          ibuf(55) = ibuf(55) - mshift                                  ! delay in mils
          ibuf(58) = ibuf(58) + ishift                                  ! number of samples
      ENDIF
c****
c****    Add the shift in to the gmt!
c****
      itemp = shift
      mil = shift*1000. - itemp*1000
c****  ibuf(84) = 1 or 2 means it is being used as the type of time
c****  (local or gmt).  Sometimes peoples use 84 to hold the millisecond.
      IF( ibuf(84) .NE. 1 .AND. ibuf(84) .NE. 2 ) THEN                  ! do the milliseconds
          ibuf(84) = ibuf(84) + mil
          IF( ibuf(84) .GT. 999 ) THEN
              ibuf(83) = ibuf(83) + 1
              ibuf(84) = ibuf(84) - 1000
          ENDIF
          IF( ibuf(84) .LT. 0 ) THEN
              ibuf(84) = ibuf(84) + 1000
              ibuf(83) = ibuf(83) - 1
          ENDIF
      ENDIF
      ibuf(83) = ibuf(83) + itemp                                       ! add in the seconds
      IF( ibuf(83) .GT. 59 ) THEN
          ibuf(82) = ibuf(82) + 1
          ibuf(83) = ibuf(83) - 60
      ENDIF
      IF( ibuf(83) .LT. 0 ) THEN
          ibuf(82) = ibuf(82) - 1
          ibuf(83) = ibuf(83) + 60
      ENDIF
      IF( ibuf(82) .GT. 59 ) THEN                                       ! check on minute of hour
          ibuf(81) = ibuf(81) + 1
          ibuf(82) = ibuf(82) - 60
      ENDIF
      IF( ibuf(82) .LT. 0 ) THEN                                        ! check on minute of hour
          ibuf(81) = ibuf(81) - 1
          ibuf(82) = ibuf(82) + 60
      ENDIF
      IF( ibuf(81) .GT. 23 ) THEN                                       ! check on hour
          ibuf(80) = ibuf(80) + 1
          ibuf(81) = ibuf(81) - 24
      ENDIF
      IF( ibuf(81) .LT. 0 ) THEN                                        ! check on hour
          ibuf(80) = ibuf(80) - 1
          ibuf(81) = ibuf(81) + 24
      ENDIF
      RETURN
      END
