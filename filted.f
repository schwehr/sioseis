      SUBROUTINE FILTED(SCR,LSCR,TRACE)
C                                PROCESS FILTER
C                                ------- ------
C
C  DOCUMENT DATE:
C
C     Process FILTER applies a frequency filter to every trace.
c  Filters available are: (see parameter FTYPE)
c     Time domain (convolutional) zero phase time varying bandpass
c     Frequency domain zero phase bandpass
c     Frequency domain minimum phase
c     Frequency domain low pass Butterworth
c     Frequency domain high pass Butterworth
c     Frequency domain notch
c
C     TIME VARYING FILTER IS PERFORMED BY APPLYING DIFFERENT FILTERS TO DIFFERENT
C  PARTS OF THE TRACE.  THE DIFFERENT PARTS OF THE TRACE ARE CALLED WINDOWS.
C  THE PORTION OF THE TRACE BETWEEN WINDOWS ARE MERGED BY RAMPING (LINEAR).
C  THE MERGE ZONE THUS CONTAINS DATA THAT HAS BEEN FILTERED BY DIFFERENT FILTERS
C  AND THEN ADDED TOGETHER AFTER BEING RAMPED.
C  THE WEIGHTS OF THE WINDOWS CAN BE DIFFERENT, HOWEVER THE MERGE ZONE WILL THE
C  CONTAIN MORE OF ONE TYPE OF FILTER THAN THE OTHER.
C     E.G.
C               F1            F2            F3
C                        ..........     ..........
C                       .          .   .
C          ..........  .            . .
C                    ..              .
C                   .  .            . .
C                  .    .          .   .
C     UP TO 5 WINDOWS MAY BE GIVEN, EACH WITH A DIFFERENT WINDOW LEVEL, AND MAY BE
C  SPATIALLY VARIED BY SHOT OR RP OR BY HANGING THE WINDOWS ON THE WATER BOTTOM.
C     ALL PARAMETERS THAT REMAIN CONSTANT FOR A SET OF SHOTS (RPS) MAY BE
C  DESCRIBED IN A PARAMETER SET FNO TO LNO.  WINDOWS BETWEEN TWO PARAMETER
C  SETS ARE CALCULATED BY LINEARLY INTERPOLATING BETWEEN LNO OF ONE SET AND FNO
C  OF THE NEXT SET.  Only the time window (sets) are spatially varied.
c  The filter (pass) remains constant even though the application window
c  (sets) varies.
C     EACH PARAMETER LIST MUST BE TERMINATED WITH THE WORD END.  THE ENTIRE SET
C  OF FILTER PARAMETERS MUST BE TERMINATED BY THE WORD END.
C
C  THE PARAMETER DICTIONARY
C  --- --------- ----------
C  FNO    - THE FIRST SHOT (OR RP) TO APPLY THE FILTER(S) TO.  SHOT (RP) NUMBERS
C           MUST INCREASE MONOTONICALLY.
C           PRESET=1
C  LNO    - THE LAST SHOT (RP) NUMBER TO APPLY THE FILTER(S) TO.  LNO MUST BE
C           LARGER THAN FNO IN EACH LIST AND MUST INCREASE LIST TO LIST.
C           DEFAULT=FNO
C  SETS   - START-END TIME PAIRS DEFINING THE WINDOWS.  TIMES ARE IN SECONDS
C           AND MAY BE NEGATIVE WHEN HANGING THE WINDOWS FROM THE WATER BOTTOM.
C           A MAXIMUM OF 5 WINDOWS MAY BE GIVEN.
C           PRESET= DELAY TO LAST TIME. E.G. SETS 0 3.0 3.3 6.0
C  PASS   - For time domain (convolution filtering or FTYPE 99):
c           A LIST OF PASSBANDS. A PASSBAND IS A SET OF TWO FREQUENCIES
C           BETWEEN WHICH THE DATA WILL BE PASSED.  FREQUENCIES OUTSIDE
C           THE PASSBAND WILL BE CUT.  PASS IS AN APPROXIMATE NUMBER BUT VERY
C           SHARP SLOPED FILTERS CAN BE OBTAINED BY INCREASING THE FILTER
C           LENGTH.  UP TO 5 PASSBANDS MAY BE GIVEN.
c         - For frequency domain bandpass filters (ftype 0 and 20)
c           The two corner frequencies of the passband.  The slope
c           of the filter are given via DBDROP.
c         - For low pass or high pass filters (ftype 1 and 2 ):
c           The cutoff frequency.
c         - For notch filters (ftype 3 and 23):
c           The two corner frequencies between which the frequencies
c           will be cut.
C           REQUIRED.   E.G. PASS 10 70 5 60
C  FILLEN - THE LENGTH OF EACH FILTER.  The number of points to use in the
c           convolution.  Up to 5 filter lengths may be given.  An odd number
c           of points should be used since the filters are symmetrical.  Short
c           filters (25 points) may run fast, but the filter shape becomes poor.
C           PRESET = 25 25 25 25 25   e.g.  fillen 39
C  LEVS   - THE AMPLITUDE LEVEL OF EACH WINDOW DESCRIBED BY SETS.  EACH WINDOW
C           MAY HAVE A DIFFERENT LEVEL.  EACH LEVEL MUST BE >0.
C           UP TO 5 LEVELS MAY BE GIVEN.
C           PRESET= 1. 1. 1. 1.
C  ADDWB  - WHEN GIVEN A VALUE OF YES, THE WINDOWS GIVEN VIA SETS AND SEATS WILL
C           BE ADDED TO THE WATER BOTTOM TIME OF THE TRACE.  (WATER BOTTOM TIMES
C           MAY BE ENTERED VIA PROCESS WBT).
C           PRESET=NO
c  FTYPE  - Filter type.  Time varying filter is permitted with time domain
c           filtering only.  See parameter MINPHA for minimum phase filters.
c         = 0, John Shay's frequency domain zero phase bandpass.
c         = 1, John Shay's frequency domain low pass. The corner
c              frequency is the first value of parameter PASS.
c         = 2, John Shay's frequency domain high pass. The corner
c              frequency is the first value of parameter PASS.
c         = 3, John Shay's frequency domain notch.
c         = 10, Low pass 3 pole Butterworth filter.
c         = 20, Warren Wood's frequency domain zero phase bandpass.
c         = 23, Warren Wood's frequency domain notch.
c         = 99, Time domain (convolutional) zero phase time varying
c           Preset = 99       e.g.   ftype 0
c  DBDROP - Decibel drop per octave for the cutoff slope when using
c           frequency domain bandpass filters.  Valid with
c           FTYPE 0, 1, 3, 20 and 23.  When set to 0 with Warren Wood's
c           filters (ftype 20 and 23), a cosine ramp is used for the
c           cutoff slope.
c           Preset = 48.      e.g. dbdrop 6
c  WINDOW - The type of window to apply before computing the fft.
c         = hamm, Hamming
c         = hann, Hanning
c         = bart, Bartlett (triangular)
c         = rect, rectangular (box car - no window)
c         = blac, Blackman
c         = ebla, exact Blackman
c         = blha, Blackman-Harris
c          Preset=hann  e.g. window rect
c
c  WINLEN - The window length, in seconds.  A window length of zero causes
c           the entire time domain gate to be windowed.  A non zero length
c           indicates that winlen data will be modified at both ends of each
c           data gate.
c           Preset = 0.  e.g.  winlen .2
c
c  MINPHA - A switch indicating that the filter should be a minimum
c           phase filter rather than zero phase.  Valid for ftype 0, 1, 2.
c           The switch is set by any no-zero value.
c           Preset = 0      e.g. minpha 1
C  END    - TERMINATES EACH PARAMETER LIST.
C
C
C  WRITTEN AND COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, MAY 1980
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
c  modified 28 sep 89 to change a pass of 0 to .1
c  mod 8 Nov 90 for the OSU frequency filters (add parameters
c       ftype and dbdrop).
c  mod 31 Jan 92 - make minimum phase available.
c  mad 28 Sept 96 - add tappers (parameters window and winlen)
c                 - add Warren Wood's filters
c  mod 9 May 97 - Add parameter INTERP
c  mod 19 May 97 - Make minpha yes/no in addition to 1/0
C
C  THE PARAMETER LIST ON DISC IS:
C  WORD 1)  FNO
C       2   LNO
C       3)  ADDWB
C       4)  LPRINT
C    5-14) SETS
C   15-19) LEVS
C   20-29) PASS
C   30-34) FILLEN
c      35) dbdrop
c      36) ftype
c      37) minpha
c
C
C
C  ARGUMENTS:
C  SCR    - A SCRATCH ARRAY AT LEAST 60 32 BIT WORDS LONG.
C  LSCR   - THE SAME ARRAY BUT THE 32 BIT INTEGER EQUIVALENT.  NEEDED
C           BECAUSE PRIME FORTRAN DOESN'T ALLOW EQUIVALENCING OF ARGUMENTS.
C  TRACE  - THE TRACE HEADER IN SEGY FORMAT
C
      PARAMETER (NPARS = 14)                                            ! THE NUMBER OF USER PARAMETERS
      PARAMETER (MAX=10)                                                ! THE MAXIMUM NUMBER OF SETS FILTER CAN HANDLE
      PARAMETER (MWRDS=40)                                              ! THE NUMBER OF WORDS IN EACH PARAMETER LIST
      DIMENSION VALS(NPARS),LVALS(NPARS)
      EQUIVALENCE (VALS(1),LVALS(1))
      CHARACTER*6 NAMES(NPARS)
      CHARACTER*1 TYPE(NPARS)
      DIMENSION LENGTH(NPARS)
      CHARACTER*80 TOKEN
      CHARACTER*4 CVALS(NPARS), window, windows(8)
      CHARACTER*3 ADDWB
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      COMMON /FILT/ MUNIT,NLISTS
      DIMENSION SCR(111),LSCR(111),TRACE(111)
      INTEGER FNO, ftype
      REAL LEVS
      DIMENSION set(max), pas(max), rlev(max), nfpt(max)
C
C
      EQUIVALENCE (FNO,LVALS(1)),
     2            (LNO,LVALS(2)),
     3            (ADDWB,CVALS(3)),
     4            (LPRINT,LVALS(4)),
     5            (SETS,VALS(5)),
     6            (LEVS,VALS(6)),
     7            (PASS,VALS(7)),
     8            (FILLEN,VALS(8)),
     9            (dbdrop,vals(9)),
     *            (ftype,lvals(10)),
     1            (minpha,lvals(11)),
     3            (winlen,vals(13)),
     4            (interp,lvals(14))
      DATA NAMES/ 'FNO   ', 'LNO   ', 'ADDWB ', 'LPRINT', 'SETS  ',
     &            'LEVS  ', 'PASS  ', 'FILLEN', 'DBDROP', 'FTYPE ',
     &            'MINPHA', 'WINDOW', 'WINLEN', 'INTERP' /
      DATA LENGTH/3,3,5,6,4,4,4,6,6,5,6,6,6,6/
      DATA TYPE/'L','L','A','L','F','F','F','F','F','L','A','A','F','A'/
      DATA windows/'HAMM','HANN','GAUS','BART','RECT','BLAC',
     *  'EBLA','BLHA'/
      DATA LLNO/0/, ntotal/0/
C****
C****      SET THE PRESETS
C****
      FNO=1
      LNO=32768
      IADDWB=0
      LPRINT=0
      DO 10 i = 1, max
         set(i) = 0.
         pas(i) = 0.
         nfpt(i) = 25
         rlev(i) = 1.
   10 CONTINUE
      nset = 0
      npas = 0
      nnfpt = 0
      nrlev = 0
      ftype = 99
      dbdrop = 48.
      minpha = 0
      window = 'HAMM'
      winlen = 0.
      interp = 1
C****
C****     GET A PARAMETER FILE
C****
      CALL GETFIL(1,MUNIT,TOKEN,ISTAT)
C****
C****   THE CURRENT COMMAND LINE IN THE SYSTEM SCRFER MAY HAVE THE PARAMETERS.
C****   GET A PARAMETER LIST FROM THE USER.
C****
      NLISTS=0
      NS=0
      NTOKES=1
  100 CONTINUE
      CALL GETOKE(TOKEN,NCHARS)                                         ! GET A TOKEN FROM THE USER PARAMETER LINE
      CALL UPCASE(TOKEN,NCHARS)                                         ! CONVERT THE TOKEN TO UPPERCASE
      IF( NCHARS .LE. 0 ) THEN
          IF(NOW.EQ.1) PRINT *,' <  ENTER PARAMETERS  >'
          CALL RDLINE                                                   ! GET ANOTHER USER PARAMETER LINE
          NTOKES=0
          GOTO 100
      ENDIF
  150 CONTINUE
      NTOKES=NTOKES+1
      DO 190 I=1,NPARS                                                  ! SEE IF IT IS A PARAMETER NAME
         LEN=LENGTH(I)                                                  ! GET THE LEGAL PARAMETER NAME LENGTH
         IPARAM=I                                                       ! SAVE THE INDEX
         IF(TOKEN(1:NCHARS).EQ.NAMES(I).AND.NCHARS.EQ.LEN) GOTO 200
  190 CONTINUE                                                          ! STILL LOOKING FOR THE NAME
      IF(TOKEN(1:NCHARS).EQ.'END'.AND.NCHARS.EQ.3) GO TO 1000           ! END OF PARAM LIST?
      IF(NS.NE.0) GOTO 230
      PRINT 191, TOKEN(1:NCHARS)
  191 FORMAT(' ***  ERROR  *** FILTER DOES NOT HAVE A PARAMETER ',
     *  'NAMED ',A10)
      IERROR=IERROR+1
      GO TO 100
C****
C****    FOUND THE PARAMETER NAME, NOW FIND THE VALUE
C****
  200 CONTINUE
      ns = 0
      NPARAM = IPARAM
  210 CONTINUE                                                          !  NOW FIND THE VALUE
      CALL GETOKE(TOKEN,NCHARS)
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF( NCHARS .LE. 0 ) THEN                                          ! END OF LINE?
          IF(NOW.EQ.1) PRINT *,' <  ENTER PARAMETERS  >'
          CALL RDLINE                                                   ! GET ANOTHER LINE
          NTOKES = 0
          GOTO 210
      ENDIF
  230 IF( TYPE(NPARAM) .EQ. 'A' ) THEN
          IF(NAMES(NPARAM).EQ.'ADDWB'.AND.TOKEN(1:NCHARS).EQ.'YES')
     *        IADDWB=1
          IF( names(nparam) .EQ. 'WINDOW' ) window = token(1:nchars)
          IF( names(nparam) .EQ. 'INTERP' ) THEN
              IF( token(1:nchars) .EQ. 'YES' ) interp = 1
              IF( token(1:nchars) .EQ. 'NO' ) interp = 0
              IF( token(1:nchars) .EQ. 'ON' ) interp = 1
              IF( token(1:nchars) .EQ. 'OFF' ) interp = 0
              IF( token(1:nchars) .EQ. '1' ) interp = 1
              IF( token(1:nchars) .EQ. '0' ) interp = 0
          ENDIF
          IF( names(nparam) .EQ. 'MINPHA' ) THEN
              IF( token(1:nchars) .EQ. 'YES' ) minpha = 1
              IF( token(1:nchars) .EQ. 'NO' ) minpha = 0
              IF( token(1:nchars) .EQ. 'ON' ) minpha = 1
              IF( token(1:nchars) .EQ. 'OFF' ) minpha = 0
              IF( token(1:nchars) .EQ. '1' ) minpha = 1
              IF( token(1:nchars) .EQ. '0' ) minpha = 0
          ENDIF
          GOTO 100
      ENDIF
      CALL DCODE(TOKEN,NCHARS,AREAL,ISTAT)                              ! TRY AND DECODE IT
      IF( ISTAT .NE. 2 ) THEN                                           ! =2 MEANS IT IS A NUMERIC
          IERROR = IERROR + 1                                           ! DCODE PRINTED AN ERROR
          GOTO 100
      ENDIF
  420 IF( TYPE(NPARAM) .EQ. 'L' ) THEN
          lvals(nparam) = areal
          GOTO 100
      ENDIF
      IF( names(nparam) .EQ. 'SETS' ) THEN
          nset = nset + 1
          set(nset) = areal
          ns = ns + 1
          GOTO 100
      ENDIF
      IF( names(nparam) .EQ. 'PASS' ) THEN
          npas = npas + 1
          pas(npas) = areal
          ns = ns + 1
          GOTO 100
      ENDIF
      IF( names(nparam) .EQ. 'LEVS' ) THEN
          nrlev = nrlev + 1
          rlev(nrlev) = areal
          ns = ns + 1
          GOTO 100
      ENDIF
      IF( names(nparam) .EQ. 'FILLEN' ) THEN
          nnfpt = nnfpt + 1
          nfpt(nnfpt) = areal
          ns = ns + 1
          GOTO 100
      ENDIF
      VALS(NPARAM) = AREAL                                              !  FLOATING POINT VALUES
      GOTO 100
C****
C****   FINISHED A LIST, NOW DO THE ERROR AND VALIDITY CHECKS
C****
 1000 CONTINUE
      iwindow = 0
      DO i = 1, 8
         IF( window .EQ. windows(i) ) iwindow = i
      ENDDO
      IF( iwindow .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  Bad window type ',window
          ierror = ierror + 1
      ENDIF
      IF(LNO.EQ.32768) LNO=FNO                                          ! DEFAULT LNO TO FNO
      IF(FNO.GT.LLNO) GO TO 1020                                        !  IS FNO LARGER THAN THE LAST LNO
      PRINT 1010
 1010 FORMAT(' ***  ERROR  ***  SHOT AND RP NUMBERS MUST INCREASE.')
      IERROR=IERROR+1
 1020 IF(LNO.GE.FNO) GO TO 1030                                         ! DO THEY INCREASE IN THIS LIST
      PRINT 1010
      IERROR=IERROR+1
 1030 LLNO=LNO
      IF( ftype .LT. 0 .OR. ftype .GT. 99 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal FTYPE of ',ftype
          ierror = ierror + 1
      ENDIF
      IF( dbdrop .LT. 0. ) THEN
          PRINT *,' ***  ERROR  ***  DBDROP must be positive.'
          ierror = ierror + 1
      ENDIF
      IF( ftype .EQ. 99 .AND. dbdrop .NE. 48. ) THEN
          PRINT *,' ***  WARNING  ***  DBDROP is ignore when FTYPE = 99'
          iwarn = iwarn + 1
      ENDIF
c**** The OSU way had a special parameter to indicate a minimum phase filter
      IF( set(1) .NE. 0. ) THEN
          PRINT *,' ***  WARNING  ***  SIOSEIS always starts filtering',
     *            ' from the first data point.'
          iwarn = iwarn + 1
      ENDIF
      DO 1110 I = 1, nset                                               ! ALLOW NEGATIVE TIMES FOR HANGING FROM WATER BOTTOM
         IF( i .EQ. 1 .AND. set(i) .EQ. 0. ) GOTO 1110
         IF( set(i) .LE. 0. .AND. iaddwb .EQ. 0 ) THEN
             PRINT *,' ***  ERROR  ***  illegal sets of ',set(i)
             ierror = ierror + 1
         ENDIF
         IF( set(i) .GT. 20. ) THEN
             PRINT *, ' ***  WARNING  *** WINDOW TIME OF ',set(i),
     *           ' LOOKS WRONG. (NOT FATAL)'
             IWARN = IWARN + 1
         ENDIF
 1110 CONTINUE
      DO 1120 i = 1, max, 2
c****  for some reason a pass of 0 gives strange (zero) output
         IF( i .LE. npas .AND. pas(i) .LE. 0. ) pas(i) = .1
         IF( set(i+1) .LT. set(i) + .1 .AND. i .LE. nset ) THEN
              PRINT *, ' ***  WARNING  ***  THERE IS AN UNUSUALLY ',
     *                 ' SHORT FILTER WINDOW.'
              iwarn = iwarn + 1
         ENDIF
         IF(i.NE.1.AND.(set(i).LT.set(i-1)+.1).AND.i.LE.nset) THEN
             PRINT *, ' ***  WARNING  *** FILTER WINDOWS SHOULD BE ',
     *                'AT LEAST .1 SECOND APART.'
             iwarn = iwarn + 1
         ENDIF
         IF( pas(i) .GE. pas(i+1) .AND. pas(i)+pas(i+1) .GT. 0) THEN
             IF( ftype .EQ. 0 .OR. ftype .EQ. 3 .OR. ftype .EQ. 99) THEN
                 PRINT *,' ***  ERROR  ***  illegal passband.'
                 ierror = ierror + 1
             ENDIF
         ENDIF
 1120 CONTINUE
      IF( ftype .NE. 99 .AND. npas .GT. 2 ) THEN
          PRINT *,' ***  ERROR  ***  Time varying filters are',
     &            ' available only via time domains filters.'
          ierror = ierror + 1
      ENDIF
      DO 1130 I = 1, max/2                                              ! DON'T ALLOW THE USER TO GIVE NEGATIVE LEVS
         IF( rlev(i) .LT. 0. ) THEN
             PRINT *, ' ***  ERROR  ***  LEVS<0. NOT ALLOWED.'
             IERROR = IERROR + 1
         ENDIF
         IF( i .LE. nnfpt ) ntotal = ntotal + nfpt(i)
         IF( nfpt(i) .LT. 1 .OR. ntotal .GT. 500 ) THEN
             PRINT *,' ***  ERROR  ***  fillen must be between 1 and',
     *           ' 500.'
             ierror =ierror + 1
         ENDIF
 1130 CONTINUE
      IF( nset .NE. 0 .AND. nset .NE. npas ) THEN
          PRINT *,' ***  ERROR  ***  There must be as many PASS as',
     *           ' SETS.'
          ierror = ierror + 1
      ENDIF
C****
C****      WRITE THE PARAMETER LIST TO DISC
C****
      LSCR(1)=FNO
      LSCR(2)=LNO
      LSCR(3)=IADDWB
      LSCR(4)=LPRINT
      DO 1410 i = 1, max
         scr(4+i) = set(i)
         scr(19+i) = pas(i)
 1410 CONTINUE
      DO 1420 i = 1, max/2
         scr(14+i) = rlev(i)
         scr(29+i) = nfpt(i)
 1420 CONTINUE
      scr(35) = dbdrop
      lscr(36) = ftype
      lscr(37) = minpha
      scr(38) = winlen
      lscr(39) = iwindow
      lscr(40) = interp
      IF(IAND(LPRINT,1).EQ.1)  PRINT 2010,(LSCR(I),I=1,4),
     *   (SCR(J),J=5,35),lscr(36),lscr(37),scr(38),lscr(39),lscr(40)
 2010 FORMAT(' FILTER PARAMS:',/,4(1X,I10),/,3(10(1X,F10.3),/),
     &      1X, F10.3, 2I4,1X,F10.3,1X,I2,I2 )
      CALL WRDISC(MUNIT,SCR,MWRDS)
      NLISTS=NLISTS+1
      NS=0
      nset = 0
      npas = 0
      nnfpt = 0
      nrlev = 0
      ntotal = 0
      LLNO=LNO                                                          ! SAVE THE CURRENT LNO
      LNO=32768                                                         ! DEFAULT THE DEFAULTS
 2020 CALL GETOKE(TOKEN,NCHARS)                                         ! GET THE NEXT TOKEN
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 2030                                        ! WAS IT THE END OF A LINE?
      IF(NOW.EQ.1) PRINT *,' <  ENTER PARAMETERS  >'
      CALL RDLINE                                                       ! GET ANOTHER LINE
      NTOKES=0
      GO TO 2020
 2030 IF(TOKEN(1:NCHARS).NE.'END'.OR.NCHARS.NE.3) GO TO 150
      RETURN
      END
