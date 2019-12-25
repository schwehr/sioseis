      SUBROUTINE FILTEX(BUF,LBUF,IBUF,SCR,LSCR)
C     FILTEX IS THE EXECUTION PHASE OF THE SEISMIC REFLECTION PROCESS FILTER
C  (TIME VARYING ZERO PHASE BANDPASS FILTER).  THE USER'S PARAMETERS MUST BE IN
C  DISC FILE MUNIT (IN COMMON /FILT/) AND THE TRACE WITH TRACE HEADER
C  MUST BE IN MEMORY ARRAY BUF.  FILTER WINDOW TIMES FOR TRACES BETWEEN
C  THOSE SHOTS OR RPS DESCRIBED BY THE USER ARE CALCULATED BY LINEAR
C  INTERPOLATION.
C
C  ARGUMENTS:
C  BUF    - THE TRACE TO BE FILTERED, INCLUDING THE TRACE HEADER.  THE FIRST
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
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, MAY 1980
c  modified July 1989 by pch to remove the awful code for spatial variation.
c           Aug 23, 89 to correct the above to allow only 1 list to work!
c  mod 8 Nov 90 for the OSU frequency domain stuff (parameters ftype and
c           dbdrop )
c  mod 28 Sept 96 - Add windowing
c  mod 7 May 97 - Correct so inf loop doesn't occur when > 1 list and
c                 first list doesn't start with first shot.
c  mod 9 May 97 - Add parameter INTERP
C
C
      PARAMETER (MAX=10)                                                ! THE MAXIMUM NUMBER OF ELEMENTS OF THE USER ARRAY SETS
      PARAMETER (MWRDS=40)                                              ! THE NUMBER OF 32 BIT WORDS IN EACH DISC PAR LIST
      PARAMETER (MAXDOW=1000)                                           ! THE MAXIMUM NUMBER OF POINTS THAT CAN BE SAVED IN WINDOW
      PARAMETER ( isetsptr = 5 )                                        ! the index of the sets within the param list
      PARAMETER ( ilevsptr = 15 )                                       ! the index of the levs within the param list
      PARAMETER ( ipassptr = 20 )                                       ! the index of the pass within the param list
      PARAMETER ( infptptr = 30 )                                       ! the index of the nfpts within the param list
      PARAMETER (MAXPTS=500)                                            ! THE MAXIMUM NUMBER OF FILTER POINTS OF ALL FILTERS
      DIMENSION BUF(111),LBUF(111),IBUF(111),SCR(111),LSCR(111)
      INTEGER*2 IBUF
      DIMENSION list1(mwrds), list2(mwrds), rlist1(mwrds), rlist2(mwrds)
      EQUIVALENCE (list1(1), rlist1(1) ), (list2(1), rlist2(1))
      DIMENSION INDXS(MAX),FILPTS(MAXPTS),OPASS(MAX), windo(maxdow)
      REAL levs(max/2), pass(max), sets(max)
      INTEGER nfpts(max/2)
      COMMON /FILT/ MUNIT,NLISTS
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP
      COMMON /APMEM/ A(98304)
      COMMON /READT/ ILUN,NUMHDR
      INTEGER fno1, fno2, addwb, ftype
      SAVE
      DATA mlists/0/, lastno/0/, lastnw/0/
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      IF(IBUF(15).EQ.2) RETURN                                          ! IS IT A DEAD TRACE
   10 IF( mlists .EQ. 0 ) THEN
          CALL podisc( munit, 1, 0 )                                    ! get the first parameter list from disk
          CALL rddisc( munit, list1, mwrds, istat )
          mlists = mlists + 1
          DO 20 i = 1, mwrds
   20     list2(i) = list1(i)
          IF( mlists .LT. nlists ) THEN                                 ! get the next list if there is one
              CALL rddisc( munit, list2, mwrds, istat )
              mlists = mlists + 1
          ENDIF
          fno1 = list1(1)
          lno1 = list1(2)
          fno2 = list2(1)
          lno2 = list2(2)
          addwb = list1(3)
          lprint = list1(4)
          dbdrop = rlist1(35)
          ftype = list1(36)
          minpha = list1(37)
          winlen = rlist1(38)
          iwindow = list1(39)
          interp = list1(40)
      ENDIF
   30 CONTINUE
      lnum = lbuf(3)                                                    !  IS THE DATA ON TAPE SORTED BY SHOT
      IF( lbuf(7) .NE. 0 ) lnum = lbuf(6)                               !  OR BY RP
      IF( lnum .EQ. lastno .AND. interp .NE. 0 ) GOTO 1000              ! is this the same shot/rp as the last time?
      IF( nlists .EQ. 1 ) GOTO 100
      IF( lnum .LE. lno1 ) THEN                                         ! is this before the last of list1?
          IF( lnum .LT. fno1 .AND. mlists .GT. 2) THEN                  ! is it before fno of list1?
              mlists = 0
              GOTO 10                                                   ! start from the beginning
         ENDIF
         GOTO 100                                                       ! use list1
      ENDIF
      IF( lnum .LT. fno2 ) GOTO 500                                     ! spatially vary the sucker
      DO 70 i = 1, mwrds                                                ! move list2 to list1
         list1(i) = list2(i)
   70 CONTINUE
      fno1 = list1(1)
      lno1 = list1(2)
      addwb = list1(3)
      lprint = list1(4)
      dbdrop = rlist1(35)
      ftype = list1(36)
      minpha = list1(37)
      winlen = rlist1(38)
      iwindow = list1(39)
      interp = list1(40)
      IF( mlists .LT. nlists ) THEN                                     ! get the next list if there is one
          CALL rddisc( munit, list2, mwrds, istat )
          mlists = mlists + 1
          fno2 = list2(1)
          lno2 = list2(2)
          GOTO 30
      ENDIF
c****
c****   Use list1
c****
  100 CONTINUE                                                          !  THE CURRENT SHOT (RP) IS >= LNO
      IF( interp .EQ. 0 ) THEN
          IF( lnum .LT. fno1 ) RETURN
          IF( lnum .GT. lno1 .AND. mlists .EQ. nlists ) RETURN
      ENDIF
      npasss = 0
      DO 150 i = 1, max
         pass(i) = rlist1( ipassptr+i-1 )
         IF( pass(i) .NE. 0 ) npasss = npasss + 1                       ! count the passbands given
         sets(i) = rlist1( isetsptr+i-1 )
  150 CONTINUE
      npasss = npasss / 2
      m = 0
      DO 140 i = 1, max/2
         levs(i) = rlist1( ilevsptr+i-1 )
         nfpts(i) = rlist1( infptptr+i-1 )
         IF( i .LE. npasss ) m = m + nfpts(i)
  140 CONTINUE
      IF( m .GT. maxpts ) THEN
          PRINT *,' Too many filter points - 500 max'
          STOP
      ENDIF
      GOTO 1000
C****
C****      SPATIALLY VARY THE FILTER WINDOW TIMES
C****
  500 CONTINUE
      IF( interp .EQ. 0 ) THEN
          IF( lnum .GT. lno1 .AND. lnum .LT. fno2 ) RETURN
      ENDIF
      RATIO = FLOAT(lnum-lno1) / FLOAT(fno2-lno1)
      DO 510 i = 1, max
  510 sets(i) = ratio *
     $ (rlist2(isetsptr+i-1)-rlist1(isetsptr+i-1))+rlist1(isetsptr+i-1)
C****
C****       SETUP THE INDEXES
C****
 1000 CONTINUE
      lastno = lnum
      NSAMPS=IBUF(58)                                                   ! THE NUMBER OF DATA SAMPLES IN THE TRACE
      DELAY=BUF(46)                                                     ! THE FLOATING POINT DEEP WATER DELAY IN SECONDS
      IF( delay .LT. 0. ) THEN
          PRINT *,' ***  ERROR  ***  FILTER can not do negative delays.'
          STOP
      ENDIF
      SI=BUF(49)                                                        ! THE FLOATING POINT SAMPLE INTERVAL IN SECONDS
      IOUT=1                                                            ! LEAVE THE DATA IN THE AP
      CALL INAP(BUF(NUMHDR+1),NSAMPS)                                   ! PUT THE DATA IN THE AP
      IF(NEXTAD+NSAMPS.LT.LAPSIZ)  GO TO 1005
      PRINT 1004
 1004 FORMAT(' ***  ERROR  ***   NOT ENOUGH AP TO PERFORM FILTER.')
      STOP
 1005 CONTINUE
      IF(IUSEAP.NE.0) CALL VMOV(IN,1,NEXTAD,1,NSAMPS)
      IF( addwb .EQ. 0 ) THEN                                           ! should we add the water bottom time in?
          wbtime = 0.                                                   ! no
      ELSE
          wbtime = buf(50)                                              !  get the water bottom time
      ENDIF
      ndows = 0                                                         ! count the number of data windows
      DO 1020 i = 1, max
          indxs(i) = 0
          IF( sets(i) .EQ. 0. .AND. i .NE. 1 ) GOTO 1020                ! a 0 time means no more
          indxs(i) = ( sets(i) + wbtime - delay ) / si + 1
          IF( indxs(i) .LT. 1 ) indxs(i) = 1                            ! always start after the beginning
          IF( indxs(i) .GT. nsamps ) indxs(i) = nsamps                  ! but don't go too far
          IF( MOD(i,2) .EQ. 0 ) THEN                                    ! is it the end of a window?
              ndows = ndows + 1
              IF( indxs(i) .LE. indxs(i-1) ) THEN                       ! is the end before the start?
                  indxs(i-1) = 0                                        ! drop the whole window
                  indxs(i) = 0
                  ndows = ndows -1
              ENDIF
          ENDIF
 1020 CONTINUE
      IF( ndows .EQ. 0 ) THEN                                           ! preset to do the whole trace
          indxs(1) = 1
          indxs(2) = nsamps
          ndows = 1
      ENDIF
      IF( indxs(1) .NE. 1 ) indxs(1) = 1                                ! always start with the first data point
      IF( indxs(ndows*2) .NE. nsamps ) indxs(ndows*2) = nsamps          /* end with the end
      DO 1110 I=1,MAX
      IF(OPASS(I).NE.PASS(I)) GO TO 1120
 1110 CONTINUE
      GO TO 1136
 1120 CONTINUE
      IPOINT=1                                                          ! THE POINTER TO THE CURRENT FILTER IN THE FLPTS ARRAY
      DO 1130 I=1,NPASSS                                                ! GENERATE ALL FILTERS IN CASE THE NUMBER OF POINTS HAS CHANGED
      J=I*2-1
      CALL BPASS(PASS(J),PASS(J+1),FILPTS(IPOINT),NFPTS(I),SI,SCR)
      IPOINT=IPOINT+NFPTS(I)
 1130 CONTINUE
      DO 1135 I=1,MAX
 1135 OPASS(I)=PASS(I)
 1136 CONTINUE
      NEXT=NEXTAD+NSAMPS                                                !  THE AP ADDRESS OF THE FILTER POINTS
      IF(IUSEAP.EQ.0) GO TO 2000
C****
C****   DO THE FILTERING IN THE AP
C****
      ISCRAP=NEXT+M                                                     !  AP ADDRESS OF A 3 WORD ARRAY NEEDED BY TVFVFC
      NEXT1=ISCRAP+3                                                    ! THE ADDRESS OF A SCRATCH AREA IN THE AP
      CALL APPUT(FILPTS(1),NEXT,M,2)                                    ! PUT ALL FILTER POINTS INTO THE AP
      IPOINT=1                                                          ! RESET THE FILTER INDEX
      DO 1200 I=1,NDOWS                                                 ! NOW FILTER IT!
      J=I*2
      N=INDXS(J)-INDXS(J-1)                                             ! THE NUMBER OF DATA POINTS IN THIS WINDOW
      IINADR=NEXTAD+INDXS(J-1)-1                                        ! THE AP ADDRESS OF THE 1ST POINT TO BE FILTERED
      IOUTAD=IN+INDXS(J-1)-1                                            ! PUT THE OUTPUT WHERE THE INPUT WAS
      SCR(1)=LEVS(I)                                                    !  TAKE CARE OF THE FRONT OF THE WINDOW (UP RAMP)
      NRAMP1=0
      SCR(2)=0.
      IF(I.EQ.1) GO TO 1140
      NRAMP1=INDXS(J-1)-INDXS(J-2)                                      ! ADD IN THE FRONT RAMP
      N=N+NRAMP1
      IINADR=IINADR-NRAMP1
      IOUTAD=IOUTAD-NRAMP1
      SCR(2)=LEVS(I)/NRAMP1
 1140 CONTINUE                                                          !  TAKE CARE OF THE BACK OF THE WINDOW
      NRAMP2=0
      SCR(3)=0.
      IF(I.EQ.NDOWS) GO TO 1150
      NRAMP2=INDXS(J+1)-INDXS(J)                                        ! THE LENGTH OF THE END (DOWN) RAMP
      SCR(3)=-LEVS(I)/NRAMP2
      N=N+NRAMP2
 1150 CONTINUE
      CALL APWR                                                         ! WAIT FOR THE LAST WINDOW BEFORE PUTTING NEW RAMPS DOWN
      CALL APPUT(SCR,ISCRAP,3,2)
      IBACK=NEXTAD+INDXS(J)                                             !  THE AP ADDRESS OF THE START OF THE BACK RAMP
      CALL APWD
      IF(IAND(LPRINT,2).EQ.0) GO TO 1190
      PRINT 1180,(SCR(K),K=1,3)
 1180 FORMAT(' FILTER ARGS IN AP:',3(1X,F10.3))
      PRINT 1185,IINADR,N,IOUTAD,NEXT1,NEXT,NFPTS(I),ISCRAP,
     *  NRAMP1,IBACK,NRAMP2
 1185 FORMAT(' FILTER ARGS:',/,5(10(1X,I10)))
 1190 CONTINUE
      CALL TVFVFC(IINADR,N,IOUTAD,NEXT1, NEXT,NFPTS(I),
     *     ISCRAP,NRAMP1,IBACK,NRAMP2)
C****  WATCH OUT FOR ARITHMETIC OVERFLOW FROM THE AP - LARGE LEVS OR LONG FILTERS
      NEXT=NEXT+NFPTS(I)
 1200 CONTINUE
      RETURN
C****
C****   Time domain zero phase convolutional filters done here
C****
 2000 CONTINUE
      IF( ftype .EQ. 99 ) THEN
          IF(IAND(LPRINT,2).NE.0) PRINT 2090,IN,NEXTAD,NEXT,NFPTS(1),
     *       LEVS(1), INDXS(1),INDXS(2),NDOWS,NSAMPS
 2090        FORMAT(' TVFILT ARGS:',/,4I10,1X,F12.5,4I10)
          CALL TVFILT(A(IN),A(NEXTAD),A(NEXT),FILPTS(1),NFPTS(1),
     *    LEVS(1),INDXS(1),NDOWS,NSAMPS)
          J=IN-1
          K=NEXTAD-1
          DO 2100 I=1,NSAMPS                                            ! PUT THE FILTERED TRACE INTO AP LOCATION IN
 2100     A(J+I)=A(K+I)
          RETURN
      ENDIF
c****
c****  Frequency domain filters done here
c****  Window the data first
c****
      IF( iwindow .NE. 5 .AND. winlen .GT. 0. ) THEN
          nwindow = winlen / si
          IF( nwindow .NE. lastnw .OR. iwindow .NE. lastiw )
     &        CALL window( iwindow, windo, nwindow, temp )
          lastnw = nwindow
          lastiw = iwindow
          nwo2 = (nwindow+1) / 2
          DO i = 1, nwo2
             a(in+i-1) = a(in+i-1) * windo(i)
             a(in+nsamps-i) = a(in+nsamps-i) * windo(nwindow+1-i)
          ENDDO
          return
      ENDIF
c****
c****  John Shay's filters
c****
      IF( ftype .LT. 10 ) THEN
          idummy = 0
          srate = 1. / si
          idbdrop = NINT(dbdrop)
          IF( IAND(lprint,2) .NE. 0 ) THEN
              PRINT *,' nsamps=',nsamps,' in=',in,' srate=',srate,
     &          ' pass=',pass(1),pass(2),' dbdrop=',dbdrop
              PRINT *,' ftype=',ftype,' minpha=',minpha
          ENDIF
c****   BEWARE   *****
c****   a patially zeroed trace before filter might cause the zeroed
c****   portion to be nonzero after filter.  ie  smute filter might
c****   yield nonzeroes after filters, and then agc will make them
c****   big again and you'll wonder why smute isn't working!!!!
          CALL filters( nsamps, a(in), srate, pass(1), pass(2), idbdrop,
     &         ftype, minpha, idummy )
          RETURN
      ENDIF
c****
c****  Warren Wood's bandpass and notch filters here
c****
      IF( ftype .EQ. 20 .OR. ftype .EQ. 23 ) THEN
          nfft = 8
          DO i = 1, 15
             IF( nsamps .GT. nfft ) nfft = nfft + nfft
          ENDDO
          DO i = nsamps, nfft-1                                         ! zero fill
             a(in+i) = 0.
          ENDDO
c         islope meanings: 1 = cosine, 2 = db/octave, < 0 = notch
          islope = 2
          IF( dbdrop .EQ. 0. ) islop = 1
          IF( ftype .EQ. 23 ) islope = -islope                          ! a notch filter
          IF( IAND(lprint,2) .NE. 0 ) PRINT *,' wood nfft=',nfft,
     &       ' si=',si,' dbdrop=',dbdrop,' pass=',pass(1),pass(2),
     &       ' islope=',islope
          CALL woodfilt( a(in), nfft, si, 1, dbdrop, pass(1), pass(2),
     &         dbdrop, islope )
          RETURN
      ENDIF
c****
c****    Low Pass Butterworth done here
c****
      IF( ftype .EQ. 10 ) THEN
          srate = 1. / si
          CALL lpbut3p( pass(1), srate, nsamps, a(in) )
          RETURN
      ENDIF

      END
