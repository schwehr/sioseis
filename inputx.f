      SUBROUTINE INPUTX(BUF1,IBUF1,LBUF1,BUF2,ISTOP)
C   INPUTX IS THE TAPE INPUT EXECUTE PHASE OF THE SIO SEISMIC PROCESSING SYSTEM.
C  INPUTX RETURNS A FLOATING POINT TRACE WITH HEADER IN ARRAY BUF1.  INPUTX
C  READS TAPE USING DOUBLE BUFFERING TECHNIQUES, THUS I/O IS BEING PERFORMED
C  INTO BUF2 AFTER INPUTX RETURNS TO THE CALLING PROGRAM.
C     INPUTX IS CONTROLLED BOTH BY THE USER'S PARAMETERS AND THE COMPUTER
C  OPERATOR.  THE PARAMETER LISTS MUST BE IN DISC FILE IPARUN.
C  DATA IS READ FROM TAPE USING THE VARIABLES IN THE FIRST PARAMETER LIST UNTIL
C  THE LIST IS EXHAUSTED, THEN ANOTHER LIST IS READ FROM DISK AND TAPE READING
C  IS DONE ACCORDING THE NEW VAIABLES.  THIS CONTINUES UNTIL THE LAST PARAMETER
C  SET IS EXHAUSTED OR THE OPERATOR ANSWERS A REEL CHANGE WITH A -1 AND THEN
C  INPUTX SETS ARGUMENT ISTOP TO 1 TO INDICATE THAT THERE IS NO MORE DATA TO
C  BE INPUT.
C      INPUTX ASSUMES THAT SHOT NUMBERS OR RP NUMBERS INCREASE ON TAPE.  NON
C  ASSENDING TAPES MAY WORK, BUT I DON'T GUARANTEE IT!
C
C  THE PARAMETER LIST MUST BE AS FOLLOWS.
C  1)  IREELN - 16 BIT INTEGER - INPUT REEL NUMBER.  IF IREELN CHANGES BETWEEN
C                                LISTS, A REEL CHANGE IS DONE.
C  2)  IUNIT  - 16 BIT INTEGER - THE INPUT TAPE UNIT NUMBER. (DRIVE NUMBER)
C  3)  LTYPE  - 32 BIT INTEGER - A 4 CHARACTER WORD SPECIFYING THE TYPE OF
C                                DATA BEING INPUT.  'SHOT' OR 'BYRP'.
C  4)  FIS    - 32 BIT INTEGER - THE FIRST SHOT OR RP NUMBER OF THIS LIST. FIS<0
C                                MEANS THAT FIS IS BEFORE (ON TAPE) THE PRESENT
C                                POSITION ON TAPE.  (SEARCH REVERSE FOR FIS)
C  5)  LIS    - 32 BIT INTEGER - THE LAST SHOT OR RP NUMBER OF THIS LIST.
C  6)  SINC   - 32 BIT INTEGER - THE INCREMENT BETWEEN FIS AND LIS.  SINC<0
C                                IMPLIES PROCESSING REVERSE.
C  7)  FTR    - 32 BIT INTEGER - THE FIRST TRACE OF THE SHOT OR RP TO READ.
C  8)  LTR    - 32 BIT INTEGER - THE LAST TRACE OF THE SHOT OR RP TO READ.
C  9)  TRINC  - 32 BIT INTEGER - THE INCREMENT BETWEEN FTR AND LTR. TRINC<0 MEANS THAT FTR>LTR.
C 10)  ISI    - 16 BIT INTEGER - THE SAMPLE INTERVAL IN MICROSECONDS (OVERRIDE)
C 11)  IDELAY - 16 BIT INTEGER - THE DEEP WATER DELAY IN MILLISECONDS (OVERRIDE)
C 12)  NTRGAT - 16 BIT INTEGER - THE NUMBER OF TRACES PER GATHER.
C 13)  TIME1  - REAL - THE PACKED START DAY-GMT
C 14)  TIME2  - REAL - THE PACKED END DAY-GMT
C
C  ARGUMENTS:
C  BUF1  - THE REAL ARRAY THAT WILL CONTAIN THE TRACE (HEADER AND DATA) READ
C          FROM TAPE.
C  IBUF1 - THE SAME ARRAY BUT THE 16 BIT INTEGER VERSION.  PRIME FORTRAN
C          DOES NOT ALLOW EQUIVALENCING OF ANYTHING TO AN ARGUMENT.
C  LBUF1 -  THE SAME ARRAY BUT THE 32 BIT INTEGER VERSION.
C  BUF2  - THE NEXT INPUT BUFFER.  DATA WILL BE READING INTO BUF2 AFTER
C          INPUTX RETURNS.
C  ISTOP - A SIGNAL WHEN SET TO 1 BY INPUTX INDICATES THAT THERE IS NO INPUT
C          TRACE LEFT ON TAPE.  THE OPERATOR OR THE USER'S PARAMETERS INDICATED
C          A HALT IN PROCESSING.  REMEMBER THAT BUF1 DOES NOT CONTAIN DATA!
C
C  COMMON REQUIREMENTS:
C       COMMON /INPUT/ IPARUN,NLISTS
C   IPARUN - THE FORTRAN UNIT NUMBER OF THE PARAMETER LIST FILE.  (NOT PRIMOS
C            UNIT NUMBER!)
C
C  COPYWRITED BY:
C   PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, JANUARY 1980
C  ALL RIGHTS RESERVED BY THE AUTHOR.
C
c   mod 17 May 10 - Comment out creating real mute times in header words 47 & 48
c  mod 14 Aug 07 - g95 IAND args must be same sized.
c  mod 19 Oct 02 - Change the temporary file stuff for file IN/in.
c  mod 31 Jul 00 - The new delay was wrong when the start time of SET was not
c            an integer number of seconds (e.g. set 1.5 3.5 had a delay od 1)
c  mod 22 Dec 97 - Change MIN0 to MIN
c  mod 24 May 96 - Change trace id from 100 (GPR) to 1 (seismic)
c  mod 6 Dec 95 - Set ltr if not given and itrindex is given.
c  mod 15 Nov 95 - add parameter REWIND, set it on every list for
c                  subroutine rdtrc, which sets it to 1 after a tape
c                  change.
c  mod 20 Jul 95 - tr index stuff was wrong (see 20 mar 95)
c  mod 23 Mar 95 - Add allno and alltr parameters
c  mod 20 Mar 95 - Add NO and noindex; Add TR and trindex
c  mod 18 Nov 93 - Increase nwrds and maxwrd from 30,000 to 32,767
c                - Toss out dummy traces (ibuf(15) = trace id = 3)
c  6 Oct 93 - Change a check from lbuf(7) to ltype .EQ. 'SHOT'
c  mod 28 Sep 93 - Increase nwrds and maxwrd from 20,000 to 30,000
c  mod 14 Feb 93 - Save LDGO shots with 0 shot number, but set the dead
c                  trace flag (undo it with process header).  It looks
c                  like shot 0 uses the shot from 2 shots ago (like in
c                  double buffering.  Change the shot number from 0 to
c                  what it should have been.  I just saw 2 hours of data
c                  with shot = 0, but the data looked okay, just off by 
c                  2 shots?
c  mod 11 Feb 93 - LDGO Ewing clock does weird things with the day 
c                  occasionally, which screws up reading by GMT.
c  mod 14 Oct 92 - subtract 1 from the number of traces per shot on LDGO
c  mod 5 Oct 92 - add parameter set
c  mod 6 May 92 - secs and stime caused crash on LDGO trace 0 (id=28) 
c  mod 11 Feb 92 to allow fis/lis on rp sorted tapes and clean up 
c           fis/lrp. frp/lrp, fno/lno presets and error messages.
c  mod 22 July 91 by pch - Add jform = 5; meaning Western/USGS bastardized
c                          SEGY format
c  mod 27 Apr 91 by pch - nis didn't work (only valid with fis 99999 anyway!)
c  mod 13 Apr 91 by pch - add jform 4 for LDGO DSS240 id=28, trace 0
c     has some goodies in it.
c  mod 4 Mar 91 by pch - delete file in if IN doesn't exist
c                      - add stime parameter.
c  mod 23 Mar 90 by pch to add the UTIG 16 bit "floating point" iformat
c  mod 20 Mar 90 by pch to get rid of the old logic about sample inervals
c                and depth data.
c  mod 20 Mar 90 by pch to do the sample interval override between calls
c                to rdtrc (wait for I/O completion and unpack the data).
c  mod 19 Mar 90 by pch to correct logic for forgat given and ltr given
c  mod 10 Jan 90 - ah shit, VMS went bananas in inputx when it saw something bad
c      in word 50 (water bottom time).  Any selfrespecting computer would have
c      NaN or 0., but no, not VAX, it crapped out!  see the setwbt kludge!
c  mod 15 Dec 89 for forgat 1 and multiple lists
c  mod 14 Nov 89 for the SSC PHXV format
c  mod 23 oct89 to toss out non-seismic data (ibuf(15) must=0,1,2,3,6)
      LOGICAL FIRST
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      COMMON /readt/ ilun, numhdr, numdat, iunhdr, ireeln, intrcs,
     *               ifmt, nskip, secs, idummy, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr,
     *               nrskip, nfiles, rewindi
      INTEGER rewind, rewindi
c  jform = 1; 
c        = 2; SSC Phoenix I
c        = 3; SSC Phoenix II
c        = 4; LDGO/Digicon DSS-240
c        = 5; Western/USGS modified SEGY
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD
      COMMON /TAPES/ NMILS,NTRIES,NPAR,NWREAD
      COMMON /INPUT/ IPARUN,NLISTS
      COMMON /WRITET/ounit,NSAMPS,OREEL,POSAFT,OFMT,NTRCS,LFOR,ONUMTR,
     &       nfskipo, rewindo, newfile, itrace0, ounit2
      COMMON /WRITET/ IWRITE(5),NOTRCS
      COMMON /inputdev/inputdev
      CHARACTER*80 inputdev
      DIMENSION BUF1(1000),IBUF1(1000),LBUF1(1000),BUF2(1000), set(2)
      INTEGER lsilly, allno, alltr
      REAL silly
      EQUIVALENCE (lsilly,silly)
      INTEGER*2 IBUF1
      INTEGER FIS,SINC,FTR,TRINC, forgat, setwbt, rshift
      CHARACTER*4 LTYPE
      CHARACTER*10 token
      INTEGER*2 ldgodss(5), i15
      SAVE                                                              ! JUST SAVE EVERYTHING!!!!
      DATA FIRST/.TRUE./, i15/15/
      DATA NWRDS/32767/, MAXWDS/32767/
      DATA NSHOTS/0/, NTRCN1/0/, istat/0/, lastsh/-1/, iforcnt/0/
      DATA setwbt/1/, lastday/0/, ibad/0/, lastshot/0/
C
C
      NOTRCS=INTRCS                                                     ! SET THE NUMBER OF OUTPUT TRACES
      ISTOP=0                                                           ! SET THE STOP SIGNAL TO NO STOP
      IF(.NOT.FIRST) NUMDAT=0                                           ! TELL RDTRC TO READ ALL THE DATA
      IF(.NOT.FIRST) GO TO 100
      CALL GETFIL(2,ITEMP,token,ISTAT)                                  ! Just get a unit number
      IF(ICOMPT.EQ.1) ITEMP=ITEMP+4                                     ! IS IT A PRIME COMPUTER?
      OPEN(UNIT=ITEMP,FILE='IN',STATUS='UNKNOWN')                       ! TRY TO OPEN FILE IN
      CLOSE(UNIT=ITEMP,STATUS='DELETE')                                 ! DELETE FILE IN
      OPEN(UNIT=ITEMP,FILE='in',STATUS='UNKNOWN')                       ! TRY TO OPEN FILE IN
      CLOSE(UNIT=ITEMP,STATUS='DELETE')                                 ! DELETE FILE IN
      IF(ICOMPT.EQ.1) ITEMP=ITEMP-4
      CALL FREFIL(1,ITEMP,ISTAT)                                        ! FREE THE UNIT NUMBER FOR GETFIL
C****
C****   GET THE FIRST PARAMETER LIST, THEN GET THE FIRST TRACE
C****
      REWIND IPARUN                                                     ! START FROM THE BEGINNING OF THE PARAMETER FILE
      READ(IPARUN,END=10) IREELN,ILUN,LTYPE,FIS,LIS,SINC,FTR,LTR,TRINC,
     *   ISI,IDELAY,NTRGAT,TIME1,TIME2,ISRCF,lprint, forgat, iorder,
     &   stime, inputdev, set, lrenum, notype, noindex, itrtype,
     &   itrindex, allno, alltr, rewind
      GO TO 30                                                          ! EVERYTHING OK ON THE READ
   10 PRINT 20
   20 FORMAT(' ***  ERROR  ***   NO INPUT PARAMETERS GIVEN.')
      STOP
   30 continue
      ireel=ireeln
      ltrcno=ftr
      IDIR=1                                                            ! THE DIRECTION OF THE TAPE SEARCH (+1=FORWARD, -1=BACKWARD)
      IF( fis .LT. 0 ) THEN
          idir = -1
          fis = -fis
      ENDIF
      idir = idir * iorder                                              ! the first shot needs to honor the sign of fis
      LCSHOT=FIS                                                        ! THE SHOT TO LOOK FOR
      IF( ltype .EQ. 'SHOT' .AND. lbuf1(7) .NE. 0 ) PRINT *,
     &    '***  WARNING  ***  Reading an RP sorted tape as a SHOT tape.'
C****
C****     WHEN DOUBLE BUFFERING, WAIT FOR THE I/O COMPLETION, THEN CHECK STATUS
C****    AND DO THE DATA CONVERSION IF NECESSARY.
C****
  100 CONTINUE
      IF( .NOT. FIRST ) THEN
          CALL RDTRC(BUF1,IBUF1,LBUF1,ISTAT,3)                          ! WAIT ON I/O COMPLETION
          IF( ILUN .LT. 0 ) THEN
              ISTOP=-1                                                  ! THE OPERATOR IS STOPPING THE JOB AND NO TRACE IS READY
              RETURN
          ENDIF
      ELSE
c****     Check for the Western/USGS format on the first trace only.
          IF( lbuf1(3) .LT. 0 .AND. lbuf1(4) .GT. 0 .AND. 
     &        lbuf1(7) .NE. 0 ) jform = 5
      ENDIF
      IF( ibuf1(15) .EQ. 100 ) ibuf1(15) = 1                            ! Change from GPR to seismic data
c**** The Lamont DSS240 system stores stuff such as GMT in trace 0
c**** and the shot number comes from trace 0 (what's in (3) is the file
c**** number.  Trace 0 also counts in the header word for the number of
c**** traces per shot (if there are traces 0-184, then there are 185 tr)
c***   Argh.  I dug my own grave.  I decided to save trace 0 when
c***  reformating from SEGD to SEGY and didn't create the trace0
c***  exactly as done before.  Damn.  jform=4 is the pre 1996 SEG-Y
c***  Trace 0 stuff.
c****
      IF( ibuf1(15) .EQ. 28 .AND. ibuf1(79) .LT. 96 ) THEN
          IF( jform .EQ. 0 ) intrcs = intrcs - 1
          jform = 4
  110     ldgoshot = 0
          iunits = 1
          itemp = lbuf1(62)
          DO 120 i = 1, 8
             ldgoshot = ldgoshot + IAND(itemp,15) * iunits
             itemp = LRSHIFT(itemp,4)
             iunits = iunits * 10
  120     CONTINUE
          DO 130 i = 1, 4
  130     ldgodss(i) = ibuf1(134+i)
          itemp = RSHIFT(ibuf1(658),8)
          itemp1 = RSHIFT(ibuf1(659),8)
          ldgodss(5) = IAND( itemp,15) * 100
     &               + IAND(ibuf1(658),i15) * 10
     &               + IAND(itemp1,15)
c****     if the shot number and the date are zero, the data is too.
          IF( ldgoshot .EQ. 0 .AND. ldgodss(1) .EQ. 0 ) THEN
c****         shot numbers of 0 are bad, they will also screwup geom type 2
c****         because the next shot's number is big
              PRINT *,' skipping shot 0 at time ', (ldgodss(i),i=1,4)
  125         CALL magtap( ilun, lbuf1, 10000, 21, istat )
              IF( istat .EQ. 1024 ) GOTO 110
              GOTO 125
          ENDIF
          kill = 1
          IF( ldgoshot .EQ. 0 ) THEN
              IF( ibuf1(15) .EQ. 28 ) THEN
                  lastshot = lastshot + 1
                  PRINT *,' shot number 0 becoming shot ',lastshot
                  ldgoshot = lastshot
                  kill = 2
              ENDIF
          ENDIF
          lastshot = ldgoshot
c****     121 is the reel number in BCD
c****     122 is the file number in BCD
c****     123, 124 is the shot number in BCD
c****     161-230 are can depths in feet
      ENDIF
      IF( jform .EQ. 4 ) THEN
          lbuf1(3) = ldgoshot
          ibuf1(15) = kill
          ibuf1(79) = 0                                                 ! Lamont doesn't have the year or millisecond
          ibuf1(80) = ldgodss(1)
          ibuf1(81) = ldgodss(2)
          ibuf1(82) = ldgodss(3)
          ibuf1(83) = ldgodss(4)
          ibuf1(84) = ldgodss(5)
      ENDIF
c**** geez.  If ltr isn't given, setting it is a pain.  There was a time
c**** when rp trace was set, but itrindex was used.  who knows what intrcs was.
      IF( ltr .EQ. -32767 ) THEN
          IF( lbuf1(7) .EQ. 0 .AND. itrindex .EQ. 0 .AND.
     &        alltr .EQ. 0 ) ltr = intrcs
      ENDIF
      IF( ltr .GE. 0 .AND. ftr .GE. 0 ) THEN
          IF( alltr .EQ. 0 ) THEN
              intrcs = (ltr - ftr) / trinc + 1
          ELSE
              intrcs = (ltr - ftr) + 1
          ENDIF
      ENDIF
      IF(ISTAT.LT.0) GO TO 240
      NWRDS=MAXWDS                                                      ! ALWAYS READ THE MAXIMUM NUMBER OF WORDS FROM TAPE
      IF(ISI.NE.0) IBUF1(59)=ISI                                        ! OVERRIDE THE TRACE HEADER IF USER GAVE IT
      IF( ntrgat .NE. 0 ) lbuf1(51) = 0                                 ! get rid of EOG
      IF( ntrgat .LT. 0 ) lbuf1(7) = 0                                  ! make it a shot
      IF( TIME1 .EQ. 0. ) THEN
c****     The Western/USGS format has the shot number in word 50 etc.
          IF( jform .EQ. 5 ) THEN
              IF( icompt .EQ. 2.OR.icompt.EQ.4) CALL swap32(lbuf1(50),1)
              lbuf1(3) = lbuf1(50) / 10000
              lbuf1(7) = 0
              lbuf1(50) = 0
          ENDIF
          IF( LBUF1(7) .EQ. 0 .OR. ltype .EQ. 'SHOT' ) THEN             ! IS IT A RP SORTED TAPE?
              LTSHOT=LBUF1(3)                                           ! THE SHOT NUMBER OF THE TRACE JUST READ
              LTRACE=LBUF1(4)                                           ! THE TRACE NUMBER WITHIN THE SHOT
          ELSE
              LTSHOT=LBUF1(6)                                           ! THE RP NUMBER OF THE TRACE JUST READ
              LTRACE=LBUF1(7)                                           ! THE TRACE NUMBER WITHIN THE RP
          ENDIF
          IF( notype .NE. 0 ) THEN
              IF( notype .EQ. 1 ) ltshot = ibuf1(noindex)
              IF( notype .EQ. 2 ) ltshot = lbuf1(noindex)
              IF( notype .EQ. 3 ) ltshot = buf1(noindex)
          ENDIF
          IF( itrtype .NE. 0 ) THEN
              IF( itrtype .EQ. 1 ) ltrace = ibuf1(itrindex)
              IF( itrtype .EQ. 2 ) ltrace = lbuf1(itrindex)
              IF( itrtype .EQ. 3 ) ltrace = buf1(itrindex)
          ENDIF
      ELSE                                                              !  DO IT BY TIME  (DAY-GMT)
          LTRACE=LBUF1(4)
          IF(LBUF1(7).NE.0) LTRACE=LBUF1(7)
          dumb = ibuf1(80)                                              ! watch out for 16 bit arithmetic!
          TIME=dumb*10000.+IBUF1(81)*100.+IBUF1(82)
          IF( IAND(lprint,2) .NE. 0 ) PRINT 135,time,time1,time2
  135     FORMAT(' time=',7(1x,F9.0))
c****     Skip it if LDGO and more than a day away from the last
          IF( jform .EQ. 4 ) THEN
              IF( lastday .EQ. ibuf1(80) .AND. ibad .NE. 0 ) GOTO 160
              IF( ibuf1(80) .GT. lastday + 1 .AND. lastday .NE. 0 ) THEN
                  lastday = ibuf1(80)
                  ibad = 1
                  GOTO 160
              ENDIF
              lastday = ibuf1(80)
              ibad = 0
          ENDIF
          IF(TIME.LT.TIME1) GO TO 160                                   !  IS THIS SHOT BEFOR THE FIRST TO PROCESS?
          IF(TIME.LE.TIME2) GO TO 170                                   !  IS IT BEYOND THE LAST TIME TO PROCESS?
 137      READ(IPARUN,END=140) IREELN,JLUN,LTYPE,FIS,LIS,SINC,
     &        FTR,LTR,TRINC,ISI,IDELAY,NTRGAT,TIME1,TIME2,ISRCF,
     &        lprint, forgat, iorder, stime, inputdev, set, lrenum,
     &        notype, noindex, itrtype, itrindex, allno, alltr
          GO TO 165
  140     ISTOP=-1                                                          ! STOP NOW - DO NOT PROCESS THIS TRACE!!
          RETURN
      ENDIF
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' fis=',fis,' lis=',lis,' sinc=',sinc,' ftr=',ftr,
     *            ' ltr=',ltr
          PRINT *,' ltshot=',ltshot,' lcshot=',lcshot,' lastsh=',
     *    lastsh,' ltrace=',ltrace,' ltrcno=',ltrcno,' nshots=',nshots
          PRINT *,' this header is ',lbuf1(3),lbuf1(4),lbuf1(6),
     *           lbuf1(7),ibuf1(15)
          PRINT *,' notype, noindex, itrtype, itrindex: ',
     &              notype, noindex, itrtype, itrindex
          PRINT *,' allno, alltr ',allno, alltr,' rewind ',rewind
      ENDIF
c**** toss out non seismic data.  ibuf(15) has the trace id code.
c**** = 0, if not set, =1 for seismic data, =2 dead, 3= dummy, 6=sweep
      IF( ibuf1(15) .NE. 0 .AND. ibuf1(15) .NE. 1 .AND. 
     *    ibuf1(15) .NE. 2 .AND. 
c                                ibuf1(15) .NE. 3 .AND.
     *    ibuf1(15) .NE. 6 ) GOTO 165
      IF(FIS.EQ.99999) GO TO 170                                        ! DOES THE USER CARE WHAT SHOT IT IS?
      IF(LCSHOT.EQ.LTSHOT) GO TO 170
      IF( allno .EQ. 1 ) THEN
          IF(fis .NE. 99999 .AND. ltshot.GT.lis.AND.lastsh.NE.-1)THEN
c     &          GOTO 137  
             READ(IPARUN,END=150) IREELN,JLUN,LTYPE,FIS,LIS,SINC,
     &          FTR,LTR,TRINC,ISI,IDELAY,NTRGAT,TIME1,TIME2,ISRCF,
     &          lprint, forgat, iorder, stime, inputdev, set, lrenum,
     &          notype, noindex, itrtype, itrindex, allno, alltr
              GO TO 165
  150         ISTOP=-1                                                          ! STOP NOW - DO NOT PROCESS THIS TRACE!!
              RETURN
          ENDIF
          IF( ltshot .GE. fis .AND. ltshot .LE. lis ) GOTO 170
      ENDIF
  160 CONTINUE                                                          ! GET ANOTHER TRACE INTO MEMORY
      IF( IDIR*iorder .LT. 0 ) THEN
          IF( inputdev .NE. ' ' ) THEN
              PRINT *,' ***  ERROR  ***  Can not go backwards on ',
     &             inputdev 
              STOP
          ENDIF
          CALL MAGTAP(ILUN,BUF1,0,25,ISTAT)                             ! BACK UP A TAPE RECORD (OVER A TRACE)
          CALL MAGTAP(ILUN,BUF1,0,25,ISTAT)                             ! BACK UP A TAPE RECORD (OVER A TRACE)
      ENDIF
  165 CALL MAGTAP(ILUN,BUF1,NWRDS,1,ISTAT)                          ! START READ INTO BUF1
      FIRST=.FALSE.
      GO TO 100
  170 CONTINUE
      IF( alltr .EQ. 0 ) THEN                                           !  GET THE RIGHT TRACE!!
          IF( LTRCNO .NE. LTRACE ) GO TO 160
      ELSE
          IF( ltrace .LT. ftr .OR. 
     &        ( ltr .GT. 0 .AND. ltrace .GT. ltr ) ) GOTO 160
      ENDIF
      IF( LTSHOT .NE. LASTSH ) THEN
          NSHOTS = NSHOTS+1                                             ! COUNT THE NEW SHOTS/RPS
          LASTSH = LTSHOT
c         nis is only valid when fis = 99999
          IF( FIS .EQ. 99999 .AND. NSHOTS .GE. LIS ) GO TO 240          ! HAVE WE DONE ALL THE SHOTS REQUESTED
          IF( ftr .EQ. 99999 .AND. nshots .NE. 1) THEN
              IF( allno .EQ. 0 ) THEN
                  lcshot = lcshot + sinc
              ELSE
                  lcshot = ltshot
              ENDIF
              IF( lcshot .GT. lis ) THEN
                  istop = -1
                  RETURN
              ENDIF
          ENDIF
      ENDIF
C****
C****  GOT THE TRACE WE WANT, START THE NEXT ONE IN BEFORE UNPACKING THE PRESENT ONE
C****
  171 IF( LRENUM .NE. -32767 ) THEN                                     ! SHOULD WE RENUMBER THE SHOT/RP NUMBER?
          IF( LBUF1(7) .EQ. 0 ) LBUF1(3)=LRENUM                         ! CHANGE THE SHOT NUMBER
          IF( LBUF1(7) .NE. 0 ) LBUF1(6)=LRENUM                         ! CHANGE THE RP NUMBER
      ENDIF
      IF( .NOT. FIRST ) THEN                                            ! THE FIRST TRACE IS ALREADY UNPACKED
          IN=0                                                          ! SET THE AP ADDRESS
          NUMDAT=0
          CALL RDTRC(BUF1,IBUF1,LBUF1,ISTAT,4)                          ! UNPACK THE DATA
      ENDIF
C****
C****  SET THE SIO SPECIAL ENTRIES IN THE TRACE HEADER IF NOT THERE YET
C****
      first = .FALSE.
      IF(IDELAY.GT.-30000) IBUF1(55)=IDELAY                             ! THE USER'S DELAY IN MILS
      SR=REAL(IBUF1(59))/1000000.                                      ! FLOAT SOME VALUES AND PUT BACK IN THE HEADER
      IF( ibuf1(59) .EQ. 7812 ) sr = 7.8125E-03                         ! 1/128
      IF( ibuf1(59) .EQ. 488 ) sr = 1. / 2048.
      IF( ibuf1(59) .EQ. 244 ) sr = 1. / 4096.
      IF( ibuf1(59) .EQ. 122 ) sr = 1. / 8192.
      IF( ibuf1(59) .EQ. 67 ) sr = 1. / 15000.
      IF( ibuf1(59) .EQ. 63 ) sr = 1. / 16000.
      IF( ibuf1(59) .EQ. 31 ) sr = 3.125E-05                            ! 1/32000
      BUF1(46)=IBUF1(55)/1000.                                          ! THE DELAY IN SECONDS
c      BUF1(47)=IBUF1(56)/1000.                                          ! THE START MUTE TIME IN SECONDS
c      BUF1(48)=IBUF1(57)/1000.                                          ! THE END MUTE TIME IN SECONDS
  177 BUF1(49)=SR                                                       ! THE SAMPLE INTERVAL IN TIME OF DEPTH
      IF( forgat .NE. 0 ) lbuf1(50) = 0
      IF( setwbt .EQ. 1 ) THEN
          IF( BUF1(50) .LT. 0. .OR. BUF1(50) .GT. 25. ) THEN
              BUF1(50) = 0.                                             ! SET WATER BOTTOM TIME
              setwbt = 0
          ENDIF
      ENDIF
C        TO ZERO IF NOT ALREADY A REASONABLE NUMBER - SOMEONE MAY HAVE USED
C        THIS ENTRY FOR SOMETHING ELSE.
C      IF(LBUF1(51).NE.INTL(-1)) LBUF1(51)=INTL(0)                      ! SAME FOR THE END OF GATHER FLAG
      IF( NTRGAT .GT. 0 ) THEN                                          ! SHOULD WE PUT THE END OF GATHER FLAG ON?
          lbuf1(51) = 0                                                 ! zero the end of gather signal
          NTRCN1=NTRCN1+1                                               ! INCREMENT THE COUNT OF TRACES WITHIN THE RP
          IF( LBUF1(7) .EQ. 0 ) THEN                                    ! DON'T CHANGE THE RP NUMBER IF SORTED BY RPS!
              LBUF1(6)=LBUF1(3)                                         ! MAKE THE RP NUMBER THE SAME AS THE SHOT NUMBER
              LBUF1(7)=NTRCN1                                           ! MAKE THE RECORD INTO A GATHER
          ENDIF
          IF( NTRCN1 .GE. NTRGAT ) THEN
              NTRCN1 = 0
              LBUF1(51) = -1
          ENDIF
      ENDIF
c****
c****   Does the user want to throw away the front of the data?
c****  or the backend?
c****
      delay = buf1(46)
      IF( stime .GT. 0. .AND. stime .GT. delay .AND. sr .GT. 0. ) THEN
          n = NINT( (stime - delay) / sr )
          DO 179 i = 1, numdat - n
  179     buf1(numhdr+i) = buf1(numhdr+n+i)
          numdat = numdat - n
          IF( numdat .LT. 0 ) numdat = 0
          ibuf1(58) = numdat
          delay = stime
          buf1(46) = delay
          ibuf1(55) = NINT(delay * 1000.)
      ENDIF
      IF( secs .NE. 0. .AND. sr .GT. 0. ) THEN
          itemp = NINT( secs / sr ) + 1
c          numdat = MIN0(numdat,itemp)
          numdat = MIN(numdat,itemp)
          ibuf1(58) = numdat
      ENDIF
      IF( set(2) .NE. 0 ) THEN
          istart = (set(1)-delay) / sr + .5
          IF( icompt .NE. 5 ) THEN
              ibuf1(55) = set(1) * 1000. + .5                            ! reset the delay in the header
          ELSE
              ibuf1(115) = set(1) * 1000. + .5
          ENDIF
          IF( delay .GT. set(1) ) THEN
              n = NINT( (delay-set(1)) / sr )
              DO 1010 i = 1, n
 1010         buf2(i) = 0.
              DO 1020 i = 1, numdat
 1020         buf2(n+i) = buf1(numhdr+i)
              numdat = numdat + n
              DO 1030 i = 1, numdat
 1030         buf1(numhdr+i) = buf2(i)
              istart = 0
          ENDIF
          buf1(46) = set(1)
          delay = set(1)
          ibuf1(55) = NINT(delay*1000.)
          IF( set(2) .GT. delay + (numdat-1)*sr ) THEN
              n = NINT( (set(2)-delay)/sr  ) + 1 - numdat
              DO 1040 i = 1, n
 1040         buf1(numhdr+numdat+i) = 0.
          ENDIF
          numdat = NINT((set(2)-delay)/sr) + 1
          IF( icompt .NE. 5 ) THEN
              ibuf1(58) = numdat
          ELSE
              ibuf1(118) = numdat
          ENDIF
          IF( istart .GT. 0 ) THEN
              DO 1100 i = 1, numdat
 1100         buf1(numhdr+i) = buf1(numhdr+istart+i)
          ENDIF
      ENDIF
C****
C****   WHAT SHOT OR RP DO WE WANT NEXT - WHAT TRACE DO WE WANT NEXT
C****
  180 IF( ltrcno .NE. 99999 .AND. alltr .EQ. 0 ) LTRCNO=LTRCNO+TRINC
      idir = 1
      IF( lis .LT. fis .AND. fis .NE. 99999 ) idir = -1
      IF( TRINC.LT.0) THEN
          IF(LTRCNO.GE.LTR) GO TO 300
          GOTO 200
      ENDIF
      IF(LTRCNO.LE.LTR.AND.LBUF1(7).EQ.0) GO TO 300
      IF( ltrcno .EQ. 99999 .AND. ltype .EQ. 'SHOT' ) GOTO 300
C****    THE NEXT STATEMENT BECAUSE I DON'T KNOW HOW MANY TRACES ARE ON A
C****  GATHER UNTIL GATHER PUTS A -1 IN WORD 51.  THIS WRECKS BEING
C****  ABLE TO READ OTHER PEOPLE'S GATHERS!!!!
      IF(LBUF1(7).EQ.0 .AND. ntrgat .EQ. 0 .AND. forgat .EQ. 0) GOTO 200! IF NOT A GATHER TAPE SKIP AROUND
      IF(LTR.GT.0.AND.LTRCNO.GT.LTR.AND.ntrgat.LE.0) LBUF1(51)=-1       ! IF LTR IS GIVEN
      IF( LBUF1(51) .NE. -1 .OR. forgat .NE. 0 ) GOTO 300
  200 IF( allno .EQ. 0 ) THEN
          LCSHOT=LCSHOT+SINC
      ELSE
          lcshot = ltshot
      ENDIF
      LTRCNO=FTR
      IF(LRENUM.NE.-32767) LRENUM=LRENUM+1                              ! INCREMENT THE RENUMBER NUMBER
      IF( TIME .NE. 0. .OR. fis .EQ. 99999 ) GO TO 300                  ! IS IT BY GMT?
      IF( LIS .LT. FIS ) THEN
          IF( LCSHOT .GE. LIS ) GOTO 300
      ELSE
          IF(LCSHOT.LE.LIS) GO TO 300
      ENDIF
  220 READ(IPARUN,END=240) IREELN,JLUN,LTYPE,FIS,LIS,SINC,FTR,LTR,TRINC,
     *   ISI,IDELAY,NTRGAT,TIME1,TIME2,ISRCF,lprint, forgat, iorder,
     &   stime, inputdev, set, lrenum, notype, noindex, itrtype, 
     &   itrindex, allno, alltr, rewind
      rewindi = rewind
      IDIR=1                                                            ! THE DIRECTION OF THE TAPE SEARCH (+1=FORWARD, -1=BACKWARD)
      IF( FIS .LT. 0 ) THEN
          IDIR = -1 * iorder
          FIS=-FIS                                                      ! MAKE IT POSITIVE
      ENDIF
      LCSHOT = FIS                                                      ! THE SHOT TO LOOK FOR
      LTRCNO = FTR                                                      !  THE TRACE NUMBER TO LOOK FOR
      GOTO 250
  240 ISTOP=1                                                           ! we hit the end and there are no more parameter lists
      RETURN
  250 IF(IREEL.EQ.IREELN) GO TO 300                                     ! IS THERE A REEL CHANGE THRU PARAMETERS
C****
C****   TAKE CARE OF REEL CHANGES SPECIFIED THRU THE USER'S PARAMETERS
C****
      IF( inputdev .NE. ' ' ) THEN
          PRINT *,' ***  ERROR  ***  Can not go backwards on ', inputdev
          STOP
      ENDIF
      CALL MAGTAP(ILUN,BUF1,0,25,ISTAT)                                 ! BACK OVER THE TRACE
      CALL TPCHNG(ILUN,ILUN,0)                                          ! GET THE TAPE CHANGE DONE
      IF(ILUN.LT.0) GO TO 240                                           ! DID THE OPERATOR STOP THE JOB
      IF(IREELN.NE.0) PRINT 260,IREELN,ILUN
  260 FORMAT(' INPUT REEL',i6,' IS NOW ON UNIT',I3)
      IREEL=IREELN
C****
C****     BACK OVER 2 TRACES IF THE NEXT SHOT/RP IS BACKWARDS
C****  THEN INITIATE A READ ON THE NEXT TRACE
C****
  300 CONTINUE
      IF( idir*iorder .LT. 0 ) THEN
          IF( inputdev .NE. ' ' ) THEN
              PRINT *,' ***  ERROR  ***  Can not go backwards on ',
     &           inputdev
              STOP
          ENDIF
          CALL MAGTAP(ILUN,BUF1,0,25,ISTAT)                             ! BACK UP A TAPE RECORD (OVER A TRACE)
          CALL MAGTAP(ILUN,BUF1,0,25,ISTAT)                             ! BACK UP A TAPE RECORD (OVER A TRACE)
          nwrds = maxwds
      ENDIF
      CALL MAGTAP(ILUN,BUF2,NWRDS,1,ISTAT)                              ! INITIATE A READ ON THE NEXT TAPE RECORD
      IF( forgat .NE. 0 ) THEN
          lbuf1(51) = 0                                                 ! make sure the end of gather is zero
          CALL magtap( ilun, buf2, 0, 40, istat )                   ! wait for the read if forgat was given
          IF( istat .EQ. -1 ) lbuf1(51) = -1                            ! set the end of gather flag if the next trace is an EOF
          IF( ltr .NE. 0 .AND. ltrcno-1 .EQ. ltr ) lbuf1(51) = -1         ! ltr given and this trace = ltr
          IF( jform .NE. 2 .AND. jform .NE. 3 ) THEN                    ! SSC Phoenix I or SEGY format 
              silly = buf2(6)                                           ! SEGY
          ELSE
              silly = buf2(1)                                           ! SSC
          ENDIF
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap32(lsilly,1)  !DEC?
          IF( lsilly .NE. lbuf1(6) ) THEN
              lbuf1(51) = -1
          ENDIF
          IF( lbuf1(51) .EQ. -1 ) THEN
              iforcnt = iforcnt + 1                                     ! count the rps
              IF( ltshot .GE. lis .AND. fis .NE. 99999 ) THEN
                  READ(IPARUN,END=240) IREELN,JLUN,LTYPE,FIS,LIS,SINC,
     *                FTR,LTR,TRINC,ISI,IDELAY,NTRGAT,TIME1,TIME2,ISRCF,
     *                lprint, forgat, iorder, stime, inputdev, set,
     &                lrenum, notype, noindex, itrtype, itrindex, allno,
     &                alltr, rewind
                  rewindi = rewind
                  lcshot = fis
              ELSE
                  IF( fis .NE. 99999 ) THEN
                      IF( allno .EQ. 0 ) THEN
                          lcshot = lcshot + sinc
                      ELSE
                          lcshot = ltshot
                      ENDIF
                  ENDIF
              ENDIF
              ltrcno = ftr
          ENDIF
          IF( iforcnt .NE. forgat ) THEN
              lbuf1(51) = 0
          ELSE
              iforcnt = 0
           ENDIF
      ENDIF
      RETURN
      END
