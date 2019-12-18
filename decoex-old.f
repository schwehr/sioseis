      SUBROUTINE SDECEX(BUF,LBUF,IBUF,SCR,LSCR)
C     SDECEX IS THE EXECUTION PHASE OF THE SEISMIC REFLECTION PROCESS SDECON
C  (TIME VARYING SPIKING DECONVOLUTION).  THE USER'S PARAMETERS MUST BE IN
C  DISC FILE MUNIT (IN COMMON /SDEC/) AND THE TRACE WITH TRACE HEADER
C  MUST BE IN MEMORY ARRAY BUF.  DECON WINDOW TIMES FOR TRACES BETWEEN
C  THOSE SHOTS OR RPS DESCRIBED BY THE USER ARE CALCULATED BY LINEAR
C  INTERPOLATION.
C
C  ARGUMENTS:
C  BUF    - THE TRACE TO BE PROCESSED, INCLUDING THE TRACE HEADER.  THE FIRST
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
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, JUNE 1980
c  MODIFICATIONS:
C  mod 4 Feb 11 - Add igap and pass to subroutine DECON
c  mod 16 Jul 10 - Pass lprint to subroutine DECON
c  mod 7 May 97 - Correct so inf loop doesn't occur when > 1 list and
c                 first list doesn't start with first shot.
c  mod    Jan 97 (mwh)
c      1) Correctly handle case where application window goes off end of
c         trace but design window still overlaps end of trace.
c      2) Preserve length of ramp for ramp between next to last and
c         last windows.
c      3) Give decon subroutine correct length of design window.
c      4) Compute length of ramp correctly.  Was one sample too long
c         resulting in each ramp shifting the subsequent data downby one sample.
c      5) Allow for application windows one sample long.
c  mod    Aug 95 (mwh) Ensure that space for ramp is at least as long as
c                number of samples between application windows.
c  mod 13 Nov 90 to add zeroes to the input trace if the autocorrelation
c         plus prediction length plus filter length is greater than the 
c         trace length.
c mod 28 nov 89 to fix the july mod, when there is only 1 list fno2 is ???
c mod 28 nov to fix multiwindow - iapp(j+1) should be iapp(i+i+1)  etc.
c  modified July 1989 by pch to remove the awful code for spatial variation.
c  17 Apr 89 - Added a parameter DOUBLE for double precision autocorrection
C
C
      PARAMETER (MAXS=10)                                               ! THE MAXIMUM NUMBER OF ELEMENTS OF THE USER ARRAY SETS
      PARAMETER (MWRDS=37)                                              ! THE LENGTH OF EACH PARAMETER LIST
      PARAMETER ( idsgptr = 13 )                                        ! the index of dsg within the parameter list
      PARAMETER ( infptptr = idsgptr+MAXS )                             ! the index of nfpts within the parameter list
      PARAMETER ( iappptr = infptptr+5 )                                ! the index of app within the parameter list
      DIMENSION BUF(111),LBUF(111),IBUF(111),SCR(111),LSCR(111)
      INTEGER*2 IBUF
      DIMENSION list1(mwrds), list2(mwrds), rlist1(mwrds), rlist2(mwrds)
      EQUIVALENCE (list1(1), rlist1(1) ), (list2(1), rlist2(1))
      DIMENSION nfpts(maxs/2), app(maxs),iapp(maxs),dsg(maxs),idsg(maxs)
      COMMON /SDEC/ MUNIT,NLISTS
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP
      COMMON /APMEM/ A(32766)
      COMMON /READT/ ILUN,NUMHDR
      INTEGER fno1, fno2, addwb
      SAVE
      DATA IERR/0/, mlists/0/, lastno/0/
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      IF(IBUF(15).EQ.2) RETURN                                          ! IS IT A DEAD TRACE
   10 IF( mlists .EQ. 0 ) THEN
          CALL podisc( munit, 1, 0 )                                    ! get the first parameter list from disk
          CALL rddisc( munit, list1, mwrds, istat )
          mlists = mlists + 1
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
          prewhi = rlist1(5)
          predic = rlist1(6)
          vel = rlist1(7)
          pdist = rlist1(8)
          ipaddw = list1(9)
          double = rlist1(10)
          igap = NINT(rlist1(11))
          nzcross = list1(12)
      ENDIF
   30 CONTINUE
      lnum = lbuf(3)                                                    !  IS THE DATA ON TAPE SORTED BY SHOT
      IF( lbuf(7) .NE. 0 ) lnum = lbuf(6)                               !  OR BY RP
      IF( lnum .EQ. lastno ) GOTO 1000                                  ! is this the same shot/rp as the last time?
      IF( nlists .EQ. 1 ) GOTO 100
      IF( lnum .LE. lno1 ) THEN                                         ! is this before the last of list1?
          IF( lnum .LT. fno1 .AND. mlists .GT. 2) THEN                  ! is it before fno of list1?
              mlists = 0
              GOTO 10                                                   ! start from the beginning
         ENDIF
         GOTO 100                                                       ! use list1
      ENDIF
      IF( nlists .EQ. 1 ) GOTO 100
      IF( lnum .LT. fno2 ) GOTO 500                                     ! spatially vary the sucker
      DO 70 i = 1, mwrds                                                ! move list2 to list1
         list1(i) = list2(i)
   70 CONTINUE
      fno1 = list1(1)
      lno1 = list1(2)
      addwb = list1(3)
      lprint = list1(4)
      prewhi = rlist1(5)
      predic = rlist1(6)
      vel = rlist1(7)
      pdist = rlist1(8)
      ipaddw = list1(9)
      double = rlist1(10)
      igap = NINT(rlist1(11))
      nzcross = list1(12)
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
      maxfil = 0
      DO 140 i = 1, maxs/2
         nfpts(i) = NINT(rlist1( infptptr+i-1 ) / buf(49 ) )
         maxfil= MAX0(maxfil,nfpts(i))
  140 CONTINUE
      maxfil = maxfil + predic                                          ! longest filter 
      DO 150 i = 1, maxs
         app(i) = rlist1( iappptr+i-1 )
         dsg(i) = rlist1( idsgptr+i-1 )
  150 CONTINUE
      GOTO 1000
C****
C****      SPATIALLY VARY THE FILTER WINDOW TIMES
C****
  500 CONTINUE
      ratio = FLOAT(lnum-lno1) / FLOAT(fno2-lno1)
      DO 510 i = 1, maxs
         app(i) = ratio * 
     $ (rlist2(iappptr+i-1)-rlist1(iappptr+i-1))+rlist1(iappptr+i-1)
         dsg(i) = ratio * 
     $ (rlist2(idsgptr+i-1)-rlist1(idsgptr+i-1))+rlist1(idsgptr+i-1)
  510 CONTINUE
C****
C****       SETUP THE INDEXES AND THE AP ARRAYS
C****
 1000 CONTINUE
      lastno = lnum
      NSAMPS=IBUF(58)                                                   ! THE NUMBER OF DATA SAMPLES IN THE TRACE
      DELAY=BUF(46)                                                     ! THE FLOATING POINT DEEP WATER DELAY IN SECONDS
      SI=BUF(49)                                                        ! THE FLOATING POINT SAMPLE INTERVAL IN SECONDS
      IPRED = NINT(PREDIC)                                              ! JUST CONVERT TO INTEGER
      IF( PDIST .GE. 0) THEN
        TEMP=PDIST
        IF( IPADDW .EQ. 1 ) TEMP = PDIST + BUF(50)                      ! ADD IN THE WATER BOTTOM
        IPRED = NINT(TEMP / SI)
      ENDIF
      IF( addwb .EQ. 0 ) THEN                                           ! should we add the water bottom time in?
          wbtime = 0.                                                   ! no
      ELSE
          wbtime = buf(50)                                              !  get the water bottom time
      ENDIF
      ndows = 0                                                         ! count the number of data windows
      ndows2 = 0
      DO i = 1, maxs
          idsg(i) = 0
          iapp(i) = 0
      ENDDO
      DO 1020 i = 1, maxs
          IF( dsg(i) .NE. 0. .OR. i .EQ. 1 ) THEN
              time = dsg(i) + wbtime                                    ! the design time
              IF( vel .NE. 0. ) THEN
                  x = lbuf(10)                                          ! float the range
                  time = SQRT(time*time+x*x/(vel*vel))
              ENDIF
              idsg(i) = NINT( ( time - delay ) / si ) + 1
              if (mod(i,2) .eq. 1) then
                  IF( idsg(i) .LT. 1 ) idsg(i) = 1                      ! always start after the beginning
              ELSE
                  IF( idsg(i) .GT. nsamps ) idsg(i) = nsamps            ! but don't go too far
              ENDIF
              IF( MOD(i,2) .EQ. 0 ) THEN                                ! is it the end of a window?
                  ndows = ndows + 1
                  IF( idsg(i) .LT. idsg(i-1) ) THEN                     ! is the end before the start?
                      idsg(i-1) = 0                                     ! drop the whole window
                      idsg(i) = 0
                      ndows = ndows -1
                      GOTO 1021                                         ! skip the application window too.
                  ENDIF
              ENDIF
          ENDIF
          IF( app(i) .NE. 0. .OR. i .EQ. 1 ) THEN                       ! a 0 time means no more
              iapp(i) = NINT(( app(i) + wbtime - delay ) / si ) + 1
              if (mod(i,2) .eq. 1) then
                  IF( iapp(i) .LT. 1 ) iapp(i) = 1                      !  always start after the beginning
              ELSE
                  IF( iapp(i) .GT. nsamps ) iapp(i) = nsamps            ! but don't go too far
              ENDIF
              IF( MOD(i,2) .EQ. 0 ) THEN                                ! is it the end of a window?
                  ndows2 = ndows2 + 1
                  IF( iapp(i) .LT. iapp(i-1) ) THEN                     ! is the end before the start?
                      iapp(i-1) = 0                                     ! drop the whole window
                      iapp(i) = 0
                      ndows2 = ndows2 -1
                      GOTO 1021
                  ENDIF
              ENDIF
          ENDIF
 1020 CONTINUE
 1021 CONTINUE
      IF( ndows .EQ. 0 ) THEN                                           ! preset to do the whole trace
          idsg(1) = 1
          idsg(2) = nsamps
          ndows = 1
      ENDIF
      IF( ndows2 .EQ. 0 ) THEN                                          ! preset to do the whole trace
          iapp(1) = 1
          iapp(2) = nsamps
          ndows2 = 1
      ENDIF
      IF( ndows .NE. ndows2 ) THEN
          IF( lprint .NE. 0 ) PRINT *,' ***  WARNING  *** ',
     &      'Different number of design and application windows.'
          ndows = min0 (ndows, ndows2)
      ENDIF
      IF( iapp(1) .NE. 1 ) iapp(1) = 1                                  ! always start with the first data point
      IF( iapp(ndows*2) .NE. nsamps ) iapp(ndows*2) = nsamps            ! end with the end
      maxdow = 0                                                        ! find the largest app window
      maxrmp = 0                                                        ! find the largest ramp between windows
      IF( ndows .GT. 1 ) THEN
          DO 1030 i = 2, ndows
c              maxdow = MAX0(maxdow,(iapp(i*2)-iapp(i*2-1)))
c              maxrmp = MAX0(maxrmp,(IAPP((i*2)-1)-IAPP((i-1)*2)))
              itemp = iapp(i*2)-iapp(i*2-1)
              maxdow = MAX(maxdow,itemp)
              itemp = IAPP((i*2)-1)-IAPP((i-1)*2)
              maxrmp = MAX(maxrmp,itemp)
 1030     CONTINUE
      ENDIF
C****
C****    FIGURE OUT THE AP MEMORY ALLOCATION
C****
      IOUT=1                                                            ! LEAVE THE DATA IN THE AP
      CALL INAP(BUF(NUMHDR+1),NSAMPS)                                   ! PUT THE DATA IN THE AP IF NOT THERE
      IAUTO=NEXTAD                                                      ! THE AUTO AND CROSS CORRELATION ADDRESS IN THE AP
C****   WHICH ARE EACH FILTER LENGTH + PREDICTION DISTANCE LONG
      IFILT=IAUTO+(MAXFIL+IPRED)*2                                      ! THE FILTER ADDRESS IN THE AP
      IERRAD = IFILT + MAXFIL + IPRED                                   ! THE ERROR OPERATOR ADDRESS IN THE AP
      IF( NDOWS .EQ. 1 ) THEN
          IFOUT = IN                                                    ! PUT THE OUTPUT ON TOP OF THE INPUT
          IWHITE = IERRAD + MAXFIL + IPRED
      ELSE
          IFOUT = IERRAD + MAXFIL + IPRED                               ! THE FILTERED OUTPUT AP ADDRESS
          IWHITE = IFOUT + nsamps                                       ! THE AP ADDRESS OF THE THREE WORD ARRAY
      ENDIF
      IF( IUSEAP .NE. 0 ) THEN
          ISCR1 = IWHITE + 4
          IRINC = ISCR1 - 1                                             ! PUT THE RAMP INCREMENT INTO THE 4TH WORD OF IWHITE
          ISCR2 = ISCR1
          IRAMP = ISCR2+MAXDOW                                          ! THE AP ADDRESS OF THE MERGE ZONE RAMPS
          ISCR3 = IRAMP + MAXDOW
          IF( ISCR3 .GT. LAPSIZ ) THEN                                  ! DOES IT ALL FIT IN THE AP
              PRINT *,' ***  ERROR  ***  ',
     $         ' NOT ENOUGH DATA MEMORY IN THE AP for decon.', iscr3
              STOP
          ENDIF
          SCR(1)=PREWHI
          SCR(2)=1.
          CALL APWR                                                     !  MAKE SURE SCR IN THE AP CAN BE USED!!
          CALL APPUT(SCR,IWHITE,2,2)                                    ! PUT THE IWHITE ARRAY IN THE AP
      ELSE
          iramp = iwhite + maxdow
c          iscr2 = iramp + MAX0(maxrmp,maxdow)
          iscr2 = iramp + MAX(maxrmp,maxdow)
          IF( ndows .EQ. 1 ) THEN
              iansw = in
          ELSE
              iansw = iscr2 + nsamps
          ENDIF
      ENDIF
      isaveans = iansw
      iscratch = iscr2 + nsamps
      IF( IAND(lprint,4) .NE. 0 ) THEN
          PRINT *,' iauto=',iauto,' gap=',igap,' nzcross=',nzcross
	  PRINT *,' ifilt=',ifilt
	  PRINT *,' ierrad=',ierrad
	  PRINT *,' ifout=',ifout
	  PRINT *,' iwhite=',iwhite
	  PRINT *,' iramp=',iramp
	  PRINT *,' iscr2=',iscr2
	  PRINT *,' iansw=',iansw
      ENDIF
C****
C****      DECONVOLVE EACH WINDOW
C****
      DO 1500 I=1,NDOWS
      J=I*2
      J1=J-1
      IDESGN=IDSG(J1)+in-1                                              ! FIND THE BEGINNING OF THE DESIGN WINDOW
      NDPTS=IDSG(J)-IDSG(J1) + 1                                        ! THE NUMBER OF POINTS IN THE DESIGN WINDOW
      ITRACE=IAPP(J1)+in-1                                              ! THE AP ADDRESS OF THE APPLICATION WINDOW
      NPTS=IAPP(J)-IAPP(J1)+1                                           ! THE NUMBER OF POINTS IN THE APPLICATION WINDOW
      IF( I .NE. 1 ) THEN
          ITRACE = IAPP(J-2) + in                                       ! START WITH THE LAST MERGE ZONE
          NPTS = NPTS + IAPP(J1) - IAPP(J-2) - 1
      ENDIF
      IF( I .NE. NDOWS ) NPTS = NPTS + IAPP(J+1) - IAPP(J) - 1
      IRESUL = IFOUT
      IF( IAND(LPRINT,2) .NE. 0 )
     $    PRINT *, ' PDECON: ',IDESGN,NDPTS,ITRACE,NPTS,IWHITE,
     $   NFPTS(I),IAUTO,IPRED,IFILT,IERRAD,IRESUL,ISCR2
c****    zero pad after the data trace if the autocoreelation + number
c****    of lags is more than the data length
      IF( idesgn+ndpts+(nfpts(i)+ipred) .GT. in+nsamps-1 ) THEN
          nzero = idesgn+ndpts+(nfpts(i)+ipred) - (in+nsamps-1)
          IF( nsamps + nzero .GT. nsamps + nsamps ) THEN
              PRINT *,' ***  ERROR  ***  The decon design window plus ',
     &         'the filter length plus the prediction distance'
              PRINT *,' is too large.'
              STOP
          ENDIF
          IF( iuseap .EQ. 0 ) THEN
              DO 1300 j = 1, nzero
 1300         a(in+nsamps+j-1) = 0.
          ELSE
              CALL vclr( in+nsamps, 1, nzero )
          ENDIF
      ENDIF
c****
c****  spiking decon = unit predeiction - it different in that the preditiction error is used.
c****  The data are in the ap simulator - array a.  No aps anymore, so remove that code.
c****
      IF( ipred .LE. 1 ) THEN
	print *,' input=',(a(ii),ii=idesgn,idesgn+10)
          IF( ipred .EQ. 0 ) ipred = 1
	print *,' ipred=',ipred
          CALL dconvo( 2, a(idesgn), ndpts, a(idesgn), ndpts, a(iauto), 
     &         nfpts(i)+ipred )
          a(iauto) = a(iauto) + a(iauto) * prewhi
        print *,' auto=',(a(j),j=iauto,iauto+20)
        print *,' right=',(a(j),j=iauto+1,iauto+10)
	print *,' wiener(',nfpts(i),a(iauto), a(iauto+ipred)
          CALL wiener( nfpts(i), a(iauto), a(iauto+ipred), a(ifilt), 
     &         a(ierrad), 1, ierr )
        print *,' lpf=',nfpts(i)
      print *,' pf=',(a(ifilt+j-1),j=1,nfpts(i))
      print *,' err=',(a(ierrad+j-1),j=1,nfpts(i))
	print *,' dconvo=',a(itrace), npts, a(ierrad), nfpts(i)+ipred,
     &   npts+nfpts(i)
	print *,' itrace=',itrace,' ierrad=',ierrad,' iresul=',iresul
          DO ii = 0, nfpts(i)-1
             a(ifilt+ii) = a(ierrad+ii) ! predik says "for spiker, grab the p.e.f. directly"
          ENDDO
      print *,' trace=',(a(itrace+j-1),j=1,200)
      print *,' filt=',(a(ifilt+j-1),j=1,nfpts(i))
          CALL convo( -1, a(itrace), npts, a(ifilt), nfpts(i), 
     &         a(iscratch), npts )
c      CALL convo( -1, b, lb, c, lf, d, lb+lf-1 )
	print *,' scratch=',(a(j),j=iscratch,iscratch+5)
          IF( ndows .EQ. 1 ) THEN
              DO j = 0, npts-1
                 a(itrace+j) = a(iscratch+j)
              ENDDO
          ENDIF
      ELSE
          CALL DECON( IDESGN,NDPTS,ITRACE,NPTS,PREWHI,NFPTS(I),IAUTO,
     *                IPRED,IFILT,IERRAD,IRESUL,ISCR2,double,lprint,
     &                igap )
      ENDIF
      IF( NDOWS .EQ. 1 ) RETURN
C****
C****   MULTIPLY THE FRONT OF THE APPLICATION WINDOW WITH THE LAST WINDOW'S
C****   REAR MERGE ZONE RAMP.  THE RAMP IS STILL IN THE AP.  ADD THE MERGE ZONE
C****   TO THE MERGE ZONE THAT IS ALREADY IN THE OUTPUT BUFFER.
C****
      IF( I .NE. 1 ) THEN
c****     n is set on the first app window and is the whole app window
          IF( IUSEAP .NE. 0 ) THEN
              CALL VMA(IRESUL,1,IRAMP,1,IANSW,1,IANSW,1,N)
          ELSE
              J = IRESUL - 1                                           ! DO THE MERGING IN THE HOST
              K = IRAMP - 1
              M = IANSW - 1
              DO 1456 II = 1, N
1456          A(M+II) = A(M+II) + A(J+II) * A(K+II)
          ENDIF
          IRESUL = IRESUL + N
          IANSW = IANSW + N
          N = NPTS - N
      ENDIF
C****
C****    MOVE THE APPLICATION WINDOW TO OUTPUT BUFFER
C****
      IF(I.EQ.1) N=NPTS
      IF(I.NE.NDOWS) N=N-(IAPP(i+i+1)-IAPP(i+i)-1)
      IF( IUSEAP .NE. 0 ) THEN
          CALL VMOV(IRESUL,1,IANSW,1,N)
      ELSE
 1465     J = IRESUL - 1
          K = IANSW - 1
          DO 1470 II = 1, N
 1470     A(K+II) = A(J+II)
      ENDIF
 1475 IF( I .EQ. NDOWS ) GOTO 1500
      IRESUL = IRESUL + N
      IANSW = IANSW + N
C****
C****  BUILD A RAMP FOR THE MERGE ZONE, AND APPLY IT TO THE BACK END MERGE
C****  AREA AND PUT THIS BACK END MERGE ZONE INTO THE OUTPUT ARRAY
C****
      N = IAPP(i+i+1) - IAPP(i+i) - 1
      TEMP = 1./(N+1)
      IF( IUSEAP .NE. 0 ) THEN
          CALL APPUT(TEMP,IRINC,1,2)                                    ! PUT THE RAMP INC INTO THE AP
          CALL APWD
          CALL VRAMP(IRINC,IRINC,IRAMP,1,N)
          ITEMP=IRAMP+N-1                                               ! FIND THE END OF THE UPWARD RAMP
          CALL VMUL(IRESUL,1,ITEMP,-1,IANSW,1,N)                        ! APPLY THE DOWN RAMP
      ELSE
          J = IRAMP - 1                                                 ! BUILD THE RAMP IN THE HOST
          DO 1485 II = 1, N
 1485     A(J+II) = FLOAT(II) * TEMP
          K = IRESUL - 1
          J = IRAMP + N
          M = IANSW - 1
          DO 1490 II = 1, N
 1490     A(M+II) = A(K+II) * A(J-II)
      ENDIF
 1500 CONTINUE
c**** 
c****  if not in the ap and multiwindow, the data is in a scratch area,
c****  move it to the input area
c****
      DO 1600 i=0, nsamps-1
 1600 a(in+i) = a(isaveans+i)
      RETURN
      END
