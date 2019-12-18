      SUBROUTINE SYNED(TRACE,BUF,LBUF,IBUF)
C                                PROCESS SYN
C                                ------- ---
C
C  DOCUMENT DATE: 7 Aug 1990
C
C     PROCESS SYN CREATES A SYNTHETIC SEISMOGRAM IN THE FORM OF A SHOT CON-
C  SISTING OF A GROUP OF TRACES CONTAINING EITHER USER GIVEN TRACE VALUES OR
C  SEISMIC EVENTS.  THE SEISMIC EVENTS ARE SPIKES PLACED IN TIME AND AMPLITUDE
C  ACCORDING TO USER SPECIFIED PARAMETERS AND MAY BE EITHER REFLECTED OR
C  REFRACTED EVENTS.
C      EACH TRACE IS ASSIGNED A RANGE AND THE SPIKES MAY THEN BE ALTERED FROM
C  TRACE TO TRACE ACCORDING TO THE INVERSE NORMAL MOVEOUT.  IN OTHER WORDS, THE
C  USER SPECIFIES A SET OF TIME-VELOCITY-AMPLITUDES FOR ZERO OFFSET AND PROCESS
C  SYN THEN COMPUTES WHERE THE SPIKES ARE ON EACH TRACE ACCORDING TO THE FORMULA
C  TX=SQRT(T0*T0+(X/V)*(X/V)) OR TX=ABS(T0)+X/V WHERE T0, X, AND V ARE SPECIFIED
C  THROUGH USER PARAMETERS.
C     SYNTHETIC SHOTS ARE COMMONLY FILTERED BY PROCESS FILTER BEFORE BEING
C  FURTHER PROCESSED.
C      THE SYNTHETIC PARAMETERS MAY BE SPACIALLY VARIED.
C     EACH PARAMETER LIST MUST BE TERMINATED WITH THE WORD END.  THE ENTIRE SET
C  OF SYN PARAMETERS MUST BE TERMINATED BY THE WORD END.
C
C  THE PARAMETER DICTIONARY
C  --- --------- ----------
C  FNO    - THE FIRST SHOT NUMBER TO CREATE WITH THE CURRENT SYN PARAMETERS.
C           SHOT NUMBERS MUST INCREASE MONOTONICALLY.
C           PRESET=1
C  LNO    - THE LAST SHOT NUMBER TO CREATE WITH THE CURRENT SET OF SYN
C           PARAMETERS.  LNO MUST GREATER THAN OR EQUAL TO FNO IN EACH LIST
C           AND MUST INCREASE LIST TO LIST.
C           DEFAULT=FNO
C  TVA    - TIME-VELOCITY-AMPLITUDE TUPLES.  A SET OF TRIPLES DESCRIBING THE
C           POSITION OF THE SPIKES MAKING UP THE 0 OFFSET TRACE.  A NEGATIVE
C           TIME INDICATES THAT THE EVENT IS A REFRACTED EVENT, WHEREAS A
C           POSITIVE TIME INDICATES A REFLECTED EVENT. A MAXIMUM OF 30 TRIPLETS
C           (90 NUMBERS) MAY BE GIVEN.
C           REQUIRED.
C  VALUES - THE LIST OF TRACE VALUES, EACH VALUE SPARATED FROM THE OTHER BY A
C           BLANK.  THE VALUES MAY BE GIVEN ON AS MANY LINES AS NECESSARY.
C           WHEN THE VALUES ARE GIVEN IN E FORMAT, POSITIVE EXPONENTS MUST
C           HAVE A + RATHER THAN A BLANK.  THE FIRST VALUE GIVEN WILL BE THE
C           FIRST DATA VALUE OF THE TRACE (I.E. THE FIRST VALUE WILL BE AT
C           TIME DELAY).
C           REQUIRED.  E.G.  VALUES .5 1 .5 0 -.5 -.1E+1
C  SI     - SAMPLE INTERVAL IN SECONDS.
C           PRESET=.004
C  NTRCS  - NUMBER OF TRACES PER SHOT.
C           PRESET=24
C  SECS   - SHOT LENGTH IN SECONDS.
C           PRESET=6.
C  X      - THE RANGE OF THE FIRST TRACE.  THE UNITS OF X DO NOT MATTER, SO LONG
C           AS THEY ARE CONSISTENT WITH THE VELOCITIES USED.
C           PRESET=0.
C  XINC   - THE RANGE INCREMENT FROM TRACE TO TRACE.  TRACE 1 WILL HAVE A
C           RANGE OF X, TRACE 2 WILL HAVE A RANGE OF X+XINC, TRACE 3 WILL
C           HAVE A RANGE OF X+2*XINC,...
C           PRESET=100.
C  NTRGAT - THE NUMBER OF TRACES PER GATHER.  WHEN GIVEN NON ZERO, EVERY
C           NTRGAT TRACE WILL BE FLAGGED AS THE END OF A GATHER (THE 51ST
C           INTEGER*4 WORD IN THE TRACE HEADER).  OTHER TRACES WILL HAVE THE
C           FLAG CLEARED TO ZERO.  THE USE OF NTRGAT ALSO SETS THE BINARY
C           TAPE HEADER SORT FLAG TO 2 (SORTED BY RP).
C           PRESET=NONE
C  DELAY  - THE DEEP WATER DELAY.  The value for DELAY is inserted into
c           the SEGY trace header AFTER the seismogram is created.  The
c           times specified in TVA must reflect the delay (e.g. 
c           tva .1 10000 1 delay 1 puts a spike at time 1.1)
C           PRESET=0.
c  NOISE  - The level of white noise to add to the trace is made with
c           VALUES or TVA.  The level given multiplies the unit variance
c           trace.  The seed for each trace is the shot number * 1000
c           plus the trace number.  See Numerical Recipes (Fortran) 
c           function gasdev for the precise algorithm used.
c           PRESET = 0.
C  END    - TERMINATES EACH PARAMETER LIST.
C
C
C  WRITTEN AND COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, JULY 1980
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
C
C  THE PARAMETER LIST ON DISC IS:
C  WORD 1)  FNO
C       2   LNO
C       3)  LPRINT
C       4)  NTRCS
C       5)  SI
C       6)  SECS
C       7)  X
C       8)  XINC
C       9)  NTRGAT
C       10) DELAY
C       11) NSAMPS
c       15) ntvas
c       16) nttvas
C
C
C  ARGUMENTS:
C  TRACE  - A BUFFER THAT CONTAINS THE TRACE HEADER OF THE FIRST TRACE
C           OF THE JOB.  SYNED ONLY SETS WORD 49 OF THE HEADER.
C  BUF    - A SCRATCH ARRAY AT LEAST 70 32 BIT WORDS LONG.
C  LBUF   - THE SAME ARRAY BUT THE 32 BIT INTEGER EQUIVALENT.  NEEDED
C           BECAUSE PRIME FORTRAN DOESN'T ALLOW EQUIVALENCING OF ARGUMENTS.
C
c  mod 25 Oct 95 - Add the delay time to the record length when checking
c                  for synthetic times exceeding record length.
c                - Add a check for synthetic times prior to the delay.
c  mod 1 May 03 - Add parameter EXTHDR to create 1 SEG-Y Rev 1 extended
c                 textual header record (3200 bytes)
c  mod 1 May 2006 - Add NOINC
c  mod 15 June 2007 - Add ttva
C
      PARAMETER (NPARS=16)                                               ! THE NUMBER OF USER PARAMETERS
c**** change npars, then change synex.f (nwrds=npars+max-1)
      PARAMETER (MAX=90)                                                 ! THE MAXIMUM NUMBER OF TVAS SYN CAN HANDLE
      PARAMETER (MWRDS=max+npars)                                        ! THE NUMBER OF WORDS IN EACH PARAMETER LIST
      PARAMETER (MULTI=14)                                               ! THE PARAMETER NUMBER OF THE FIRST MULTI-VALUED PARAMETER
      DIMENSION LENGTH(NPARS),VALS(NPARS),LVALS(NPARS)
      CHARACTER*80 cheadr(40)
      EQUIVALENCE (ivms,cheadr(1))
      CHARACTER*7 NAMES(NPARS)
      CHARACTER*1 TYPE(NPARS)
      CHARACTER*80 TOKEN
      INTEGER FNO
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      COMMON /SYN/ MUNIT,NLISTS
      COMMON /READT/ ILUN,NUMHDR,NUMDAT,IUNHDR,idummy,intrcs,ifmt,nskip,
     *         secss,lrenum,isrcf,idtype
      COMMON /binhdr/ ibinhdr(200)
      INTEGER*2 ibinhdr
      DIMENSION BUF(111),LBUF(111),TRACE(111),IBUF(111)
      INTEGER*2 IBUF
      REAL noise
C
C
      EQUIVALENCE (FNO,LVALS(1)),
     2            (LNO,LVALS(2)),
     3            (LPRINT,LVALS(3)),
     4            (NTRCS,LVALS(4)),
     5            (SI,VALS(5)),
     6            (SECS,VALS(6)),
     7            (X,VALS(7)),
     8            (XINC,VALS(8)),
     9            (NTRGAT,LVALS(9)),
     *            (DELAY,VALS(10)),
     1            (noise,vals(11)),
     2            (nexthdr,lvals(12)),
     3            (noinc,lvals(13)),
     3            (TVA,VALS(14)),
     4            (VALUES,VALS(15)),
     6            (ttva,vals(16))
      DATA NAMES/'FNO   ',
     2           'LNO   ',
     3           'LPRINT',
     4           'NTRCS ',
     5           'SI    ',
     6           'SECS  ',
     7           'X     ',
     8           'XINC  ',
     9           'NTRGAT',
     *           'DELAY ',
     1           'NOISE ',
     2           'NEXTHDR',
     3           'NOINC ',
     3           'TVA   ',
     4           'VALUES',
     5           'TTVA  '/
      DATA LENGTH/3,3,6,5,2,4,1,4,6,5,5,7,5,3,6,4/
      DATA TYPE/'L','L','L','L','F','F','F','F','L',2*'F',2*'L',3*'F'/
C****
C****      SET THE PRESETS
C****
      ntvas = 0
      nttvas = 0
      FNO=1
      LNO=32768
      LPRINT=0
      NTRCS=24
      SI=.004
      SECS=6.
      X=0.
      XINC=100.
      NTRGAT=0
      DELAY=0.
      DO 10 I=1,71
   10 BUF(I)=0.
      LLNO=0
      nlists=0
      ns=0
      NSAMPS=0
      noise = 0.
      nexthdr = -1
      noinc = 1
C****
C*****    GET A PARAMETER FILE
C****
      CALL GETFIL(1,MUNIT,token,ISTAT)
C****
C****   THE CURRENT COMMAND LINE IN THE SYSTEM BUFFER MAY HAVE THE PARAMETERS.
C****   GET A PARAMETER LIST FROM THE USER.
C****
      NTOKES=1
  100 CONTINUE
      CALL GETOKE(TOKEN,NCHARS)                                          ! GET A TOKEN FROM THE USER PARAMETER LINE
      IF(NCHARS.GT.0) GO TO 150
      IF(NOW.EQ.1) PRINT 140
  140 FORMAT(' <  ENTER PARAMETERS  >')
      CALL RDLINE                                                        ! GET ANOTHER USER PARAMETER LINE
      NTOKES=0
      GO TO 100
  150 CONTINUE
      NTOKES=NTOKES+1
      CALL UPCASE(TOKEN,NCHARS)
      DO 190 I=1,NPARS                                                  ! SEE IF IT IS A PARAMETER NAME
      LEN=LENGTH(I)                                                      ! GET THE LEGAL PARAMETER NAME LENGTH
      IPARAM=I                                                          ! SAVE THE INDEX
      IF(TOKEN(1:NCHARS).EQ.NAMES(I)(1:LEN).AND.NCHARS.EQ.LEN) GO TO 200
  190 CONTINUE                                                          ! STILL LOOKING FOR THE NAME
      IF(TOKEN(1:NCHARS).EQ.'END'.AND.NCHARS.EQ.3) GO TO 1000            ! END OF PARAM LIST?
      IF( NS .NE. 0 ) GOTO 230                                          ! it must be a values
      PRINT *,' ***  ERROR  *** SYN DOES NOT HAVE A PARAMETER ',
     &         token(1:nchars)
      IERROR = IERROR + 1
      GOTO 100
C****
C****    FOUND THE PARAMETER NAME, NOW FIND THE VALUE
C****
  200 CONTINUE
      NPARAM=IPARAM
  210 CONTINUE                                                           !  NOW FIND THE VALUE
      CALL GETOKE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 230                                         ! END OF LINE?
      IF(NOW.EQ.1) PRINT 140                                            ! THIS ALLOWS A PARAMETER TO BE ON A DIFFERENT LINE FROM THE NAME
      CALL RDLINE                                                        ! GET ANOTHER LINE
      NTOKES=0
      GO TO 210
  230 CONTINUE
      IF( TYPE(NPARAM) .EQ. 'A') GOTO 100
      CALL DCODE(TOKEN,NCHARS,AREAL,ISTAT)                              ! TRY AND DECODE IT
      IF( ISTAT .NE. 2 ) THEN                                           ! =2 MEANS IT IS A NUMERIC
          IERROR = IERROR + 1                                           ! DCODE PRINTED AN ERROR
          GOTO 100
      ENDIF
      IF( TYPE(NPARAM) .EQ. 'F' ) THEN
          IF( names(nparam) .EQ. 'TVA' .OR.
     &        names(nparam) .EQ. 'VALUES' .OR.
     &        names(nparam) .EQ. 'TTVA' ) THEN
              ns = ns+1 
              IF( names(nparam) .EQ. 'TVA' ) THEN
                  IF( ns .GT. max ) THEN
                      PRINT *,' ***  ERROR  ***  Too many tva tuples.'
                      PRINT *, max/3,' tuples maximum!'
                      ierror = ierror + 1
                  ELSE
                      ntvas = ntvas + 1
                  ENDIF
              ENDIF
              IF( names(nparam) .EQ. 'VALUES' ) THEN
                  nsamps = ns
                  IF( ns .GT. max ) THEN
                      PRINT *,' ***  ERROR  ***  Too many VALUES.', max,
     &                    ' is maximum!'
                      ierror = ierror + 1
                  ENDIF
              ENDIF
              IF( names(nparam) .EQ. 'TTVA' ) THEN
                  IF( ns .GT. max ) THEN
                      PRINT *,' ***  ERROR  ***  Too many ttva tuples.'
                      PRINT *, max/4,' tuples maximum!'
                      ierror = ierror + 1
                  ELSE
                      nttvas = nttvas + 1
                  ENDIF
              ENDIF
              buf(ns+npars) = AREAL
          ELSE
              VALS(NPARAM) = AREAL
          ENDIF
          GOTO 100
      ENDIF
      LVALS(NPARAM) = NINT(AREAL)
      GOTO 100
C****
C****   FINISHED A LIST, NOW DO THE ERROR AND VALIDITY CHECKS
C****
 1000 CONTINUE                                                           ! MAKE SURE ALL SHOT & RP NUMBERS INCREASE
      IF(LNO.EQ.32768) LNO=FNO                                          ! DEFAULT LNO TO FNO
      IF( LLNO .NE. 0 .AND. FNO .LE. LLNO) THEN                          !  IS FNO LARGER THAN THE LAST LNO
          PRINT *,' ***  ERROR  ***  SHOT AND RP NUMBERS MUST INCREASE.'
          IERROR = IERROR + 1
      ENDIF
      IF( LNO .LT. FNO ) THEN                                           ! DO THEY INCREASE IN THIS LIST
          PRINT *,' ***  ERROR  ***  SHOT AND RP NUMBERS MUST INCREASE.'
          IERROR = IERROR + 1
      ENDIF
      LLNO = LNO
c     nsamps is set when VALUES is given
      IF( ntvas+nttvas+nsamps+noise .EQ. 0. .AND. nlists .EQ. 0) THEN
          PRINT *,
     &     ' ***  ERROR  ***  TVA, VALUES OR NOISE MUST BE GIVEN.'
          IERROR = IERROR+1
      ENDIF
      IF( ntvas .GT. 0 ) THEN
          IF( ntvas/3*3 .NE. ntvas ) THEN
              PRINT *, ' ***  ERROR  ***  There must be 3 elements in',
     &                 ' each TVA tuple.'
              ierror = ierror + 1
          ENDIF
          DO I = 1, ntvas, 3                                            ! CHECK THE TIMES
             IF( ABS(BUF(I+npars)) .GT. SECS+delay ) THEN
                 PRINT 1130,BUF(I+npars),SECS
 1130            FORMAT(' ***  WARNING  ***  SYNTHETIC TIME OF ',F10.3,
     *                  ' EXCEEDS RECORD LENGTH OF ',F10.3)
                 IWARN = IWARN + 1
             ENDIF
             IF( ABS(BUF(I+npars)) .LT. delay ) THEN
                 PRINT 1131,BUF(I+npars),delay
 1131            FORMAT(' ***  WARNING  ***  SYNTHETIC TIME OF ',F10.3,
     *                  ' IS BEFORE THE DELAY TIME OF ',F10.3)
                 IWARN = IWARN + 1
             ENDIF
          ENDDO
      ENDIF
      IF( nttvas .GT. 0 ) THEN
          IF( nttvas/4*4 .NE. nttvas ) THEN
              PRINT *, ' ***  ERROR  ***  There must be 4 elements in',
     &                 ' each TTVA tuple.'
              ierror = ierror + 1
          ENDIF
          DO I = 2, nttvas, 4                                           ! CHECK THE TIMES
             IF( ABS(BUF(I+npars)) .GT. SECS+delay ) THEN
                 PRINT 1130,BUF(I+npars),SECS
                 IWARN = IWARN + 1
             ENDIF
             IF( ABS(BUF(I+npars)) .LT. delay ) THEN
                 PRINT 1131,BUF(I+npars),delay
                 IWARN = IWARN + 1
             ENDIF
          ENDDO
      ENDIF
      idtype = 1                                                        ! time data
      NUMDAT=SECS/SI + 1.+.5                                            ! SET THE NUMBER OF SAMPLES PER TRACE - OTHER EDITS MAY WANT IT
C****
C****        WRITE THE PARAMETER LIST TO DISC
C****
      LBUF(1)=FNO
      LBUF(2)=LNO
      LBUF(3)=LPRINT
      LBUF(4)=NTRCS
      BUF(5)=SI
      BUF(6)=SECS
      BUF(7)=X
      BUF(8)=XINC
      LBUF(9)=NTRGAT
      BUF(10)=DELAY
      buf(11) = noise
      LBUF(12)=NSAMPS
      lbuf(13) = nexthdr
      lbuf(14) = noinc
      lbuf(15) = ntvas
      lbuf(16) = nttvas
      NWRDS=MWRDS+NSAMPS
      IF(IAND(LPRINT,1).EQ.1)  PRINT 2010,(LBUF(I),I=1,4),
     *   (BUF(J),J=5,8),LBUF(9),BUF(10),BUF(11),(BUF(I),I=12,NWRDS)
 2010 FORMAT(' SYN PARAMS:',/,4(1X,I10),/,4(1X,F10.3),I10,1X,3F10.3,
     *      /,10(10(1X,F10.3),/))
      CALL WRDISC(MUNIT,BUF,MWRDS)
      NLISTS=NLISTS+1
      NS=0
      ntvas = 0
      nttvas = 0
      nsamps = 0
      DO 2015 i = 1, nwrds
 2015 buf(i) = 0.
      LNO=32768                                                          ! DEFAULT THE DEFAULTS
 2020 CALL GETOKE(TOKEN,NCHARS)                                          ! GET THE NEXT TOKEN
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 2030                                        ! WAS IT THE END OF A LINE?
      IF(NOW.EQ.1) PRINT 140
      CALL RDLINE                                                        ! GET ANOTHER LINE
      NTOKES=0
      GO TO 2020
 2030 call upcase(token,nchars)
      IF(TOKEN(1:NCHARS).NE.'END'.OR.NCHARS.NE.3) GO TO 150
      DO 2040 I=1,60
 2040 TRACE(I)=0.
      TRACE(49)=SI
c**** do the tape headers now
      CALL getfil(1,iunhdr,token,istat)
      DO  i = 1, 40
          cheadr(i) = ' '
          WRITE (cheadr(i),2050) i
 2050     FORMAT('c ',i2)
          CALL ascebc(cheadr(i), 80, cheadr(i) )
      ENDDO
      IF( icompt .NE. 4 ) THEN
          CALL wrdisc( iunhdr, cheadr, 800)
      ELSE
          CALL wrdisc( iunhdr, ivms, 800)
      ENDIF
      DO i = 1,200
         ibinhdr(i) = 0
      ENDDO
      ibinhdr(7) = ntrcs
      ibinhdr(15) = 1
      ibinhdr(31) = idtype
      IF( ntrgat .NE. 0 ) ibinhdr(15)=2
      IF( nexthdr .GE. 0 ) THEN
          ibinhdr(151) = 256
          ibinhdr(153) = nexthdr
      ENDIF
      CALL wrdisc(iunhdr,ibinhdr,100)
      IF( nexthdr .GT. 0 ) THEN
          DO i = 1, 40
             cheadr(i) = ' '
          ENDDO
          DO i = 1, nexthdr
             IF( i .EQ. nexthdr ) cheadr(1) = "((EndText))"
             CALL wrdiscb( iunhdr, cheadr, 3200 )
          ENDDO
      ENDIF
c****
c****  take care of input's common
c****
      intrcs=ntrcs
c****
c****
      RETURN                                                            !  FINISHED ALL OF THE SYN PARAMETERS!!!
      END
