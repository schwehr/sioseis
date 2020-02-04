      SUBROUTINE MUTEEX( BUF, LBUF, IBUF, SCR, LSCR )
C     MUTEEX IS THE EXECUTION PHASE OF THE SEISMIC PROCESS MUTE, WHICH MUTES OR
C  ZEROS A PORTION OF A TRACE.  THE MUTE PARAMETERS MUST BE ON DISC FILE MUNIT
C  AND THE TRACE (WITH HEADER) MUST BE IN MEMORY LOCATION BUF.  THE MUTE TIMES
C  FOR TRACES BETWEEN THOSE SHOTS OR RPS DESCRIBED BY THE USER ARE CALCULATED
C  BY LINEAR INTERPOLATION.  LIKEWISE, TIMES FOR TRACES NOT SPECIFIED BY THE
C  USER OBTAINED THROUGH EXTRAPOLATION OR INTERPOLATION.  DEEP WATER DELAYS ARE
C  HONORED - I.E. MUTE TIMES ARE RELATIVE TO TIME ZERO REGARDLESS OF THE TIME
C  OF THE FIRST DATA SAMPLE.
C     SUBROUTINE MUTEED CONTAINS THE EXPLANATION OF THE USER PARAMETERS AND THE
C  ORDER OF THE USER PARAMETERS ON DISC.
C
C  ARGUMENTS:
C  BUF    - THE TRACE TO BE MUTED, INCLUDING THE TRACE HEADER.  THE FIRST
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
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, APRIL 1980
C
C
c  mod 13 Nov 01 - increase MAXTTP from 50 to 500
c  mod 28 Jul 06 - Honor water depth scalar when using wbdvel
c  mod 14 Aug 07 - g95 arg to FLOAT must be int*4, so let it do the float
c  mod 1 Apr 10 - Get rid of arithmetic IF statement in two places
c               - Use NINT to convert time to samples.
c               - Use the first list if before the first list (extrapolate)
c   mod 17 May 10 - Comment out creating real mute times in header words 47 & 48
c  mod 13 Jul 12 - bad mute when the trace/range was < the first given.
c                  (1 Apr 10 change was bad).
c
      PARAMETER (MAXTTP=500)                                            ! /* THE MAXIMUM NUMBER OF ELEMENTS THE USER ARRAY CAN BE
      PARAMETER (NWRDS=MAXTTP+7)                                        ! /* THE LENGTH OF EACH PARAMETER LIST
      DIMENSION BUF(111),LBUF(111),IBUF(111),SCR(111),LSCR(111)
      INTEGER*2 IBUF
      SAVE
      DIMENSION OLDTTP(MAXTTP),CURTTP(MAXTTP)
      COMMON /MUTER/ MUNIT,NLISTS
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP
      COMMON /APMEM/ A(32766)
      COMMON /READT/ ILUN,NUMHDR
      INTEGER FNO
      LOGICAL FIRST
      DATA FIRST /.TRUE./, lastno/0/, dlast/0/
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      IF(IBUF(15).EQ.2) RETURN                                          ! /* IS IT A DEAD TRACE
      ISIG=0
      IF(.NOT.FIRST) GO TO 50
      FIRST=.FALSE.
   10 CONTINUE                                                          ! /* GET THE FIRST PARAMETER LIST INT0 MEMORY ARRAY SCR
      CALL PODISC(MUNIT,1,0)
      CALL RDDISC(MUNIT,SCR,NWRDS,ISTAT)
      ISIG=1
      FNO=LSCR(1)
      LNO=LSCR(2)
      MLISTS=1
   50 CONTINUE
      LNUM=LBUF(3)                                                      ! /*  IS THE DATA ON TAPE SORTED BY SHOT
      TRNO=LBUF(4)                                                      ! /* THE TRACE NUMBER WITHIN THE SHOT
      IF(LBUF(7).NE.0) TRNO=LBUF(7)                                     ! /* THE TRACE NUMBER WITH AN RP
      IF(LBUF(7).NE.0) LNUM=LBUF(6)                                     ! /*  OR BY RP
   70 IF(LNUM.GE.FNO) GO TO 100                                         ! /* IS THIS SHOT BEFORE THIS PARAMETER LIST
      IF(MLISTS.EQ.1) GO TO 500                                         ! /* IS IT BEFORE THE FIRST LIST
      IF(LNUM.LE.LASTNO) GO TO 10                                       ! /* IS IT IN OR BEFORE THE LAST LIST
      GO TO 500                                                         ! /* IT MUST BE BETWEEN THE 2 LISTS
  100 CONTINUE                                                          ! /*  THE CURRENT SHOT (RP) IS >= LNO
      IF(LNUM.LE.LNO) GO TO 500                                         ! /* USE THE PARAMETERS OF THIS LIST
      IF(MLISTS.GE.NLISTS) GO TO 500                                    ! /* ANY MORE USER PARAM LISTS ON DISC
C****
C****   GET ANOTHER USER PARAMETER LIST FROM DISC
C****
      CALL RDDISC( MUNIT, LSCR, NWRDS, ISTAT )
      LASTNO=LNO
      DO 120 I=1,MAXTTP                                                 ! /* SAVE THE CURRENT PARAMETER SET
  120 OLDTTP(I) = curttp(i)
      MLISTS=MLISTS+1
      ISIG=1
      FNO=LSCR(1)
      LNO=LSCR(2)
      GO TO 70
C****
C****    NOW FIND THE MUTE TIME FOR THE  TRACE NUMBER (OR RANGE)
C****
  500 IF(ISIG.EQ.0) GO TO 506                                           ! /* IS A LIST IN SCR
      IADDWB=LSCR(3)
      NOTTPS=LSCR(4)
      LTYPE=LSCR(5)
      LPRINT=LSCR(6)
      wbdvel = scr(7) / 2.
      DO 505 I=1,MAXTTP
  505 CURTTP(I)=SCR(I+7)
  506 IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' lnum=',lnum,' isig=',isig
          PRINT *,' fno',fno,' lno',lno,' iaddwb',iaddwb,' ltype',ltype
          PRINT *,' nottps=',nottps,' curttp',(curttp(i),i=1,nottps)
          PRINT *,' oldttp',(oldttp(i),i=1,nottps)
      ENDIF
      IF( ltype .EQ. 1 ) THEN  ! if xtp, get the range
          TEMP=LBUF(10)    ! TRNO=ABS(LBUF(10) DOESN'T WORK!!!
          TRNO=ABS(TEMP)   ! SET THE TRACE NO TO THE RANGE IF XTP WAS GIVEN
      ENDIF
c  510 IF(TRNO-CURTTP(I))530,540,520
c  520 I=I+2
c      IF(I.LE.NOTTPS) GO TO 510
c      TIME=CURTTP(NOTTPS)      ! USE THE LAST TIME IN THE ARRAY
c      GO TO 600
c  530 CONTINUE         !  THE TRACE NUMBER IS LESS THAN TTP(I)
c      IF(I.NE.1) GO TO 550
c  540 TIME=CURTTP(I+1)
c      GO TO 600
c  550 CONTINUE    !  THE TRACE IS BETWEEN TTP(I-2) & TTP(I)
      i = 1
      IF( nottps .EQ. 2 ) THEN
          time = curttp(2)
          GOTO 600
      ENDIF
c****  use the first time if the trace/range is <= first trace/range
      IF( trno .LE. curttp(1) ) THEN
          time = curttp(2)
          GOTO 600
      ENDIF
  510 CONTINUE
      IF( trno .EQ. curttp(i) ) THEN
          time = curttp(i+1)
          GOTO 600
      ENDIF
      IF( trno .GT. curttp(i) ) THEN
          IF( i+2 .GE. nottps ) THEN
              time = curttp(i+1)
              GOTO 600
          ENDIF
          i = i + 2
          GOTO 510
      ENDIF
      T1=CURTTP(I-1)
      T2=CURTTP(I+1)
      TR1=CURTTP(I-2)
      TR2=CURTTP(I)
      TIME=(TRNO-TR2)/(TR1-TR2)*(T1-T2)+T2   ! INTERPOLATE BETWEEN TRACES
C****
C****     SPATIALLY VARY THE TIME IF BETWEEN 2 LISTS
C****
  600 CONTINUE
      IF( lnum .LT. fno .AND. mlists .EQ. 1 ) GOTO 2000   ! if before the first list, use the first list
      IF(LNUM.GE.FNO) GO TO 2000    ! IF IN THIS LIST THEN DO NOT VARY IT
      I=1         !  NOW FIND THE TIME FOR THIS TRACE IN THE PREVIOUS LIST
c  610 IF(TRNO-OLDTTP(I)) 630,640,620
c  620 I=I+2
c      IF(I.LE.NOTTPS) GO TO 610
c      OTIME=OLDTTP(NOTTPS)    ! USE THE LAST TIME IN THE ARRRAY
c      GO TO 700
c  630 CONTINUE    ! THE TRACE NUMBER IS SMALLER
c      IF(I.NE.1) GO TO 650
c  640 OTIME=OLDTTP(I+1)
c      GO TO 700
  610 CONTINUE
      IF( trno .EQ. oldttp(i) ) THEN
          otime = oldttp(i+1)
          GOTO 700
      ENDIF
      IF( trno .LT. oldttp(i) ) THEN
          IF( i .NE. nottps ) GOTO 650
          otime = oldttp(i+1)
          GOTO 700
      ENDIF
      i = i + 2
      IF( i .LE. nottps ) GOTO 610
      otime = oldttp(nottps)
      GOTO 700
  650 CONTINUE    !  TRACE IS BETWEEN OLDTTP(I-2) & OLDTTP(I)
      T1=OLDTTP(I-1)
      T2=OLDTTP(I+1)
      TR1=OLDTTP(I-2)
      TR2=OLDTTP(I)
      OTIME=(TRNO-TR2)/(TR1-TR2)*(T1-T2)+T2
  700 CONTINUE    ! NOW INTERPOLATE SPATIALLY
      T1=FNO      !  FLOAT THE SHOT (RP) NUMBER
      T2=LNUM     !  THE CURRENT SHOT (RP) NUMBER
      T3=LASTNO   ! THE LAST SHOT (RP) NUMBER OF THE PREVIOUS LIST
      X=(T2-T3)/(T1-T3)*(TIME-OTIME)+OTIME
      TIME=X
	print *,' x (time) =',x
C****
C****   NOW APPLY THE MUTE
C****
 2000 CONTINUE
      SI=BUF(49)    ! THE SAMPLE INTERVAL IN SECONDS
      DELAY=BUF(46) ! THE DEEP WATER DELAY IN SECONDS
      IF( IADDWB .EQ. 1 ) THEN    ! ADD IN THE WATER BOTTOM TIME
          IF( wbdvel .NE. 0. ) THEN
c     SIO SeaBeam used to put the depth in 107
c              IF( ibuf(107) .NE. 0 ) THEN
c                  depthl = ibuf(107)
c              ELSE
              depth = lbuf(16)
              IF( ibuf(35) .GT. 0 ) depth = depth * ibuf(35)
              IF( ibuf(35) .LT. 0 ) depth = -depth / ibuf(35)
              IF( depth .EQ. 0 ) depth = dlast
              dlast = depth
              time = time + depth / wbdvel
          ELSE
              time = time + buf(50)
          ENDIF
      ENDIF
      IS=1     !  ASSUME THIS IS A FRONT END MUTE (TTP OR XTP)
      IE=NINT((TIME-DELAY)/SI)+1   ! THE END INDEX OF THE MUTE
      IF(IE.LT.IS) IE=IS
      IF(IS.EQ.IE) RETURN    ! DON'T DO ANY MUTING IF NONE TO DO!
      NSAMPS=IBUF(58)        ! 16 BIT NUMBER OF SAMPLES PER TRACE
      IF(IE.GT.NSAMPS) IE=NSAMPS
      IF(IASGND.NE.0.AND.IUSEAP.NE.0) GO TO 2100    ! DATA IS IN THE AP IF IN<>0
      IF(IN.NE.0.AND.IUSEAP.EQ.0) GO TO 2020
      CALL MUTE(BUF(NUMHDR+1),IS,IE,SI,NSAMPS)      ! THE IN MEMORY MUTE
      GO TO 2200
 2020 CALL MUTE(A(IN),IS,IE,SI,NSAMPS)
      GO TO 2200
 2100 CONTINUE     ! THE TRACE IS IN THE AP
      CALL INAP(BUF(NUMHDR+1),NSAMPS)
      IF(IE.NE.NSAMPS) GO TO 2150    ! ARE WE MUTEING THE WHOLE TRACE
      CALL VCLR(IN,1,NSAMPS)         ! ZERO THE WHOLE TRACE
      IBUF(15)=2                     ! SET THE DEAD TRACE FLAG
      GO TO 2200
 2150 N=5                            !  THE LENGTH OF THE TAPER
      CALL APWR                      ! MAKE SURE WE DON'T CLOBBER ANYTHING IN PROGRESS IN NEXTAD
      TEMP=.2                        ! THE INCREMENT OF THE TAPER BETWEEN POINTS OF THE TAPER
      IF(IE.LE.N) N=0
      IF(IE-IS.LT.N+N) N=0           ! WATCH OUT FOR VERY SHORT MUTES
      CALL APPUT(TEMP,NEXTAD,1,2)    ! PUT THE INCREMENT IN AP LOCATION NEXTAD
      IIS=IS+IN-1                    ! ADD IN THE AP ADDRESS OF THE BEGINNING OF THE TRACE
      IIE=IE+IN-1
      CALL APWD                      ! WAIT FOR THE AP DATA TRANSFER TO COMPLETE
      CALL MUTEAP(IIS,IIE,N,NEXTAD,NSAMPS)
 2200 CONTINUE                       !  NOW PUT THE MUTE TIMES IN THE TRACE HEADER
      IF( IE .GE. NSAMPS ) THEN
         IBUF(15)=2                  ! SET THE DEAD TRACE FLAG
         PRINT *,' ***  WARNING  ***  Mute is killing the trace'
      ENDIF
      NMILS=IBUF(59)/1000.           ! TIMES ARE IN MILS IN THE HEADER
      NDELAY=DELAY*1000.
      IBUF(56)=0                     ! MUTE START TIME IS AT DELAY OR TIME 0
      IBUF(57)=(IE-1)*NMILS+NDELAY   ! MUTE END TIME IN MILS
c      BUF(47)=0.                     ! MUTE START TIME IN SECONDS
c      BUF(48)=(IE-1)*SI+DELAY        !  MUTE END TIME IN SECONDS
      IF(LPRINT.GT.1) PRINT 2210,LNUM,TRNO,ibuf(56),ibuf(57),is,ie
 2210 FORMAT(' SHOT ',I6,' TRACE ',F6.1,' IS MUTED FROM ',I4,' TO ',
     *    I4,' (',2I8,')')
      RETURN
      END
