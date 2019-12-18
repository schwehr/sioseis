      SUBROUTINE POUTED(SCR,LSCR)
C                     PROCESS PROUT  (PRINTER OUTPUT)
C
C                     ------- -----
C DOCUMENT DATE: 11 July 1995
C
C     PROCESS PROUT PRINTS THE TRACE AMPLITUDES OF UP TO 5 WINDOWS PER TRACE .
C  EACH TRACE TO BE PRINTED MUST BE EXPLICITLY SPECIFIED BY GIVING FNO, FTR,
C  AND SETS.
C     EACH PARAMETER LIST MUST BE TERMINATED WITH THE WORD END.  THE ENTIRE SET
C  OF PROUT PARAMETERS MUST BE TERMINATED BY THE WORD END.
C
C   EXAMPLE:
C     IN ORDER TO PRINT THE TRACE VALUES BETWEEN TIMES 1.2 AND 1.5 ON TRACE
C  24 AND SHOTS AND 46, THE FOLLOWING MUST BE GIVEN:
C         PROCESS PROUT
C             FNO 45 FTR 24 SETS 1.2 1.5 END
C             FNO 46 SETS 1.2 1.5 END
C         END
C
C  THE PARAMETER DICTIONARY
C  --- --------- ----------
C  FNO    - THE FIRST SHOT (OR RP) TO PRINT.  SHOT (RP) NUMBERS
C           MUST INCREASE MONOTONICALLY.
C           PRESET=1
C  LNO    - THE LAST SHOT (RP) NUMBER TO PRINT.  LNO MUST BE
C           LARGER THAN FNO IN EACH LIST AND MUST INCREASE LIST TO LIST.
C           DEFAULT=FNO
c  NOINC  - The increment between FNO and LNO.
c           Preset = 1 e.g   noinc 100
C  SETS   - START-END TIME PAIRS DEFINING THE DATA TO BE PRINTED.  TIMES
C           ARE IN SECONDS AND MAY BE NEGATIVE WHEN HANGING THE WINDOWS FROM THE
C           WATER BOTTOM.  A MAXIMUM OF 5 WINDOWS MAY BE GIVEN.
C           REQUIRED.
C  INC    - THE INCREMENT BETWEEN DATA VALUES TO PRINT.  THIS IS THE FORTRAN
C           DO LOOP INCREMENT.  E.G. IN ORDER TO PRINT EVERY OTHER DATA VALUE,
C           SET INC TO 2.
C           PRESET = 1.  E.G. INC 2
C  FTR    - THE FIRST TRACE WITHIN THE SHOT (RP) TO PRINT.
C           PRESET=1
C  LTR    - THE LAST TRACE WITHIN EACH SHOT (RP) TO PRINT.  TRACES EXCEEDING
C           LTR WILL NOT BE PRINTED.
C         - PRESET=FTR
C  TRINC  - THE TRACE INCREMENT BETWEEN FTR AND LTR. ALSO CALLED THE TRACE SKIP
C           CYCLE.
C           PRESET=1
C  HEADER - WHEN SET NONZERO, THE TRACE HEADER IS PRINTED.
C           PRESET=0
C  THEADS - WHEN SET NONZERO, THE TAPE HEADERS ARE PRINTED.
C           PRESET=0
C  ADDWB  - WHEN GIVEN A VALUE OF YES, THE WINDOWS GIVEN VIA SETS WILL
C           BE ADDED TO THE WATER BOTTOM TIME OF THE TRACE.  (WATER BOTTOM TIMES
C           MAY BE ENTERED VIA PROCESS WBT).
C           PRESET=NO
c  FORMAT - Indicates how the output should be formated.
c         = MATLAB, Each trace printed will be in a separate file 
c           suitable to use as ASCII input to MATLAB.  The file name
c           will be "sh" or "rp" followed by the shot/rp number followed
c           by the trace number followed by ".m".  Each file starts with
c            " a = [ " and is terminated with "]", the MATLAB syntax for
c           reading vector a.  PROUT parameter SETS must be given when using
c           FORMAT MATLAB.
c         = ( fortran format statement ).  The specified Fortran format
c           will be used when enclosed in parenthesis.
c           PRESET = ' '      e.g.   format (10(1X,F6.0))
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
C       2)  LNO
C       3)  ADDWB
C       4)  LPRINT
C       5)  FTR
C       6)  LTR
C       7)  TRINC
C       8)  HEADER
C       9)  THEADS
c
c
c      13)  ntrlist
C    14-23  SETS   -   (nwrds-max+ns) (nwrds=23, max=10)
c    20 words FORMAT
C    20 words indices
C
C  ARGUMENTS:
C  SCR    - A SCRATCH ARRAY AT LEAST 60 32 BIT WORDS LONG.
C  LSCR   - THE SAME ARRAY BUT THE 32 BIT INTEGER EQUIVALENT.  NEEDED
C           BECAUSE PRIME FORTRAN DOESN'T ALLOW EQUIVALENCING OF ARGUMENTS.
C
c  modified 1 May 92 for the FORMAT parameter
c  mod 26 May 93 to cleanup/fix the indices of the parameter list
c  mod 30 Jul 98 - Add indices
c  mod 21 Sep 05 - Add parameter info
c  mod 25 May 06 - Allow negative trace numbers
c  mod 23 Feb 07 - Add parameter TRLIST
c  mod 14 Aug 09 - Add OPATH
c  mod 16 Aug 09 - Cygwin doesn't have OPEN APPEND
c  mod 27 Aug 09 - Add WBDEPTH@S, WBDEPTH@R, and WBTIME to TRLIST.
c  mod 14 Sep 09 - Add DELAY to TRLIST
c  mod 6 Oct 11 - Add FOLD to TRLIST
C
      PARAMETER (NPARS=17)                                               ! THE NUMBER OF USER PARAMETERS
      PARAMETER (MAX=10)                                                 ! THE MAXIMUM NUMBER OF SETS PROUT CAN HANDLE
      PARAMETER (MAX_LIST = 10 )
      PARAMETER (NWRDS=24)                                               ! THE NUMBER OF WORDS IN EACH PARAMETER LIST
      PARAMETER (MULTI=15)                                               ! THE PARAMETER NUMBER OF THE FIRST MULTI-VALUED PARAMETER
      DIMENSION VALS(NPARS),LVALS(NPARS)
      CHARACTER*7 NAMES(NPARS)
      CHARACTER*1 TYPE(NPARS)
      DIMENSION LENGTH(NPARS)
      CHARACTER*80 TOKEN, form
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW
      INTEGER FNO,ADDWB
      COMMON /POUT/ MUNIT,NLISTS
      DIMENSION SCR(111),LSCR(111),sets(2)
      INTEGER FTR,TRINC,HEADER,INC,THEADS
      INTEGER indices(20), tr_list(MAX_LIST)
      LOGICAL iexist
C
C
      EQUIVALENCE (FNO,LVALS(1)),
     2            (LNO,LVALS(2)),
     3            (noinc,lvals(3)),
     3            (ADDWB,LVALS(4)),
     4            (LPRINT,LVALS(5)),
     5            (FTR,LVALS(6)),
     6            (LTR,LVALS(7)),
     7            (TRINC,LVALS(8)),
     8            (HEADER,LVALS(9)),
     9            (INC,LVALS(10)),
     *            (THEADS,LVALS(11)),
     1            (info,lvals(12)),
     2            (sets,vals(14))
      DATA NAMES/'FNO   ',
     2           'LNO   ',
     3           'NOINC ',
     4           'ADDWB ',
     5           'LPRINT',
     6           'FTR   ',
     7           'LTR   ',
     8           'TRINC ',
     9           'HEADER',
     *           'INC   ',
     1           'THEADS',
     2           'INFO  ',
     3           'FORMAT',
     4           'SETS  ',
     5           'INDICES',
     6           'TRLIST',
     7           'OPATH'/
      DATA TYPE /'L','L','L','A','L','L','L','L','L','L',
     &   'L','L','A','F', 'L','L','A' /
      DATA LENGTH/3,3,5,5,6,3,3,5,6,3,6,4,6,4,7,6,5/
C****
C****      SET THE PRESETS
C****
      FNO=1
      LNO=32768
      noinc = 1
      ADDWB=0
      LPRINT=0
      FTR=1
      LTR=-1
      TRINC=1
      HEADER=0
      THEADS=0
      INC=1
      info = 0
      DO 20 I = nwrds-max+1, nwrds
   20 SCR(I)=0.
      LLNO=0
      nlists=0
      ns=0
      form = ' '
      DO i = 1, 20
         indices(i) = 0
      ENDDO
      nindices = 0
      DO i = 1, MAX_LIST
         tr_list(i) = 0
      ENDDO
      ntrlist = 0
      luno = 6
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
      CALL UPCASE(TOKEN,NCHARS)
      IF(NCHARS.GT.0) GO TO 150
      IF(NOW.EQ.1) PRINT 140
  140 FORMAT(' <  ENTER PARAMETERS  >')
      CALL RDLINE                                                        ! GET ANOTHER USER PARAMETER LINE
      NTOKES=0
      GO TO 100
  150 CONTINUE
      NTOKES=NTOKES+1
      DO 190 I=1,NPARS                                                  ! SEE IF IT IS A PARAMETER NAME
      LEN=LENGTH(I)                                                      ! GET THE LEGAL PARAMETER NAME LENGTH
      IPARAM=I                                                          ! SAVE THE INDEX
      IF(TOKEN(1:NCHARS).EQ.NAMES(I)(1:LEN).AND.NCHARS.EQ.LEN) GO TO 200
  190 CONTINUE                                                          ! STILL LOOKING FOR THE NAME
      IF(TOKEN(1:NCHARS).EQ.'END'.AND.NCHARS.EQ.3) GO TO 1000            ! END OF PARAM LIST?
      IF( NS .NE. 0 .OR. nindices .GT. 0 .OR. ntrlist .GT. 0 ) GO TO 230
      PRINT 191, TOKEN(1:NCHARS)
  191 FORMAT(' ***  ERROR  *** PROUT DOES NOT HAVE A PARAMETER ',
     *  'NAMED ',A10)
      IERROR=IERROR+1
      GO TO 100
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
      CALL UPCASE(TOKEN,NCHARS)
      IF( TYPE(NPARAM) .EQ. 'A' ) THEN
          IF( names(nparam) .EQ. 'OPATH') THEN
              INQUIRE( FILE=token, EXIST=iexist)
              CALL GETFIL( 2, luno, token, istat )
              IF( .NOT. iexist ) THEN
                  OPEN( UNIT=luno, FILE=token, 
     &               FORM='FORMATTED', STATUS='UNKNOWN')
              ELSE
                  OPEN( UNIT=luno, FILE=token, 
c     &               FORM='FORMATTED', STATUS='OLD',POSITION='APPEND')
     &               FORM='FORMATTED', STATUS='OLD')
              ENDIF
          ELSE
              CALL UPCASE(TOKEN,NCHARS)
          ENDIF
          IF( NAMES(NPARAM) .EQ. 'ADDWB' .AND. TOKEN(1:NCHARS).EQ.'YES')
     *        ADDWB=1
          IF( names(nparam) .EQ. 'FORMAT' ) form = token(1:nchars)
          GO TO 100
      ENDIF
      CALL UPCASE(TOKEN,NCHARS)
      IF( names(nparam) .NE. 'INDICES' .AND. 
     &    names(nparam) .NE. 'TRLIST' ) THEN
          CALL DCODE(TOKEN,NCHARS,AREAL,ISTAT)                          ! TRY AND DECODE IT
          IF( ISTAT .NE. 2 ) THEN                                       ! =2 MEANS IT IS A NUMERIC
              IERROR=IERROR+1                                           ! DCODE PRINTED AN ERROR
              GOTO 100
          ENDIF
      ENDIF
      IF(TYPE(NPARAM).EQ.'L') GO TO 500
      IF( names(nparam) .EQ. 'SETS' ) THEN
          NS=NS+1 
          SCR(nwrds-max+ns) = AREAL
          GOTO 100
      ENDIF
  490 VALS(NPARAM)=AREAL                                                 !  FLOATING POINT VALUES
      GO TO 100
  500 CONTINUE                                                          !  32 BIT INTEGER VALUES
      IF( names(nparam) .EQ. 'INDICES' ) THEN
          nindices = nindices + 1
          IF( token(1:1) .EQ. 'I' ) indices(nindices) = 1
          IF( token(1:1) .EQ. 'L' ) indices(nindices) = 2
          IF( token(1:1) .EQ. 'R' ) indices(nindices) = 3
          nindices = nindices + 1
          token(1:nchars-1) = token(2:nchars)
          token(nchars:nchars) = ' '
          nchars = nchars - 1
          CALL dcode( token, nchars, areal, istat )
          indices(nindices) = NINT(areal)
          GOTO 100
      ENDIF
      IF( names(nparam) .EQ. 'TRLIST' ) THEN
          ntrlist = ntrlist + 1
          IF( token(1:6) .EQ. 'SHOTNO' ) tr_list(ntrlist) = 2
          IF( token(1:4) .EQ. 'RPNO' ) tr_list(ntrlist) = 3
          IF( token(1:3) .EQ. 'GMT' .AND. nchars .EQ. 3 ) 
     &        tr_list(ntrlist) = 4
          IF( token(1:5) .EQ. 'RANGE' ) tr_list(ntrlist) = 6
          IF( token(1:6) .EQ. 'SHOTTR' ) tr_list(ntrlist) = 7
          IF( token(1:4) .EQ. 'ESPN' ) tr_list(ntrlist) = 8
          IF( token(1:6) .EQ. 'HEADER' ) tr_list(ntrlist) = 11
          IF( token(1:4) .EQ. 'RPTR' ) tr_list(ntrlist) = 12
          IF( token(1:6) .EQ. 'GMTSEC' ) tr_list(ntrlist) = 15
          IF( token(1:3) .EQ. 'SXD' ) tr_list(ntrlist) = 16
          IF( token(1:3) .EQ. 'SYD' ) tr_list(ntrlist) = 17
          IF( token(1:4) .EQ. 'SXDM' ) tr_list(ntrlist) = 18
          IF( token(1:4) .EQ. 'SYDM' ) tr_list(ntrlist) = 19
          IF( token(1:5) .EQ. 'SXDMS' ) tr_list(ntrlist) = 20
          IF( token(1:5) .EQ. 'SYDMS' ) tr_list(ntrlist) = 21
          IF( token(1:3) .EQ. 'RXD' ) tr_list(ntrlist) = 22
          IF( token(1:3) .EQ. 'RYD' ) tr_list(ntrlist) = 23
          IF( token(1:4) .EQ. 'RXDM' ) tr_list(ntrlist) = 24
          IF( token(1:4) .EQ. 'RYDM' ) tr_list(ntrlist) = 25
          IF( token(1:5) .EQ. 'RXDMS' ) tr_list(ntrlist) = 26
          IF( token(1:5) .EQ. 'RYDMS' ) tr_list(ntrlist) = 27
          IF( token(1:9) .EQ. 'WBDEPTH@S' ) tr_list(ntrlist) = 28
          IF( token(1:9) .EQ. 'WBDEPTH@R' ) tr_list(ntrlist) = 29
          IF( token(1:6) .EQ. 'WBTIME' ) tr_list(ntrlist) = 30
          IF( token(1:5) .EQ. 'DELAY' ) tr_list(ntrlist) = 31
          IF( token(1:4) .EQ. 'FOLD' ) tr_list(ntrlist) = 32
          IF( tr_list(ntrlist) .EQ. 0 ) THEN
             PRINT *,' ***  ERROR  ***  Unknown TRLIST ',token(1:nchars)
              ierror = ierror + 1
          ENDIF
          GOTO 100
      ENDIF
      LVALS(NPARAM)=AREAL
      GOTO 100
C****
C****   FINISHED A LIST, NOW DO THE ERROR AND VALIDITY CHECKS
C****
 1000 CONTINUE                                                           ! MAKE SURE ALL SHOT & RP NUMBERS INCREASE
      IF(LNO.EQ.32768) LNO=FNO                                          ! DEFAULT LNO TO FNO
      IF(FNO.GT.LLNO.OR.FNO.EQ.0.OR.LLNO.EQ.0) GO TO 1020               !  IS FNO LARGER THAN THE LAST LNO
      PRINT 1010
 1010 FORMAT(' ***  ERROR  ***  SHOT AND RP NUMBERS MUST INCREASE.')
      IERROR=IERROR+1
 1020 IF(LNO.GE.FNO) GO TO 1030                                          ! DO THEY INCREASE IN THIS LIST
      PRINT 1010
      IERROR=IERROR+1
 1030 LLNO=LNO
      IF(LTR.EQ.-1) LTR=FTR                                             ! PRESET LTR TO FTR, WHEN LTR NOT GIVEN
c      IF(FTR.GE.0) GO TO 1220
c      PRINT 1210,FTR
 1210 FORMAT(' ***  ERROR  ***  INCORRECT TRACE NUMBER OF ',I10)
c      IERROR=IERROR+1
c 1220 CONTINUE
      IF(LTR.GE.FTR) GO TO 1230
      PRINT 1210,LTR
      IERROR=IERROR+1
 1230 CONTINUE
      IF(TRINC.GT.0) GO TO 1240
      PRINT 1210,TRINC
      IERROR=IERROR+1
 1240 CONTINUE
      IF( form .EQ. 'FORMAT' .OR. form .EQ. 'format' ) THEN
          IF( ns .LT. 2 ) THEN
              PRINT *,' ***  ERROR  *** SETS must be given.'
              ierror = ierror + 1
          ENDIF
      ENDIF
C****
C****      WRITE THE PARAMETER LIST TO DISC
C****
      IF( NS .GT. MAX ) THEN
          ITEMP=MAX
          PRINT *,' ***  ERROR  ***  PROUT CAN HANDLE ONLY ',itemp,
     &          ' SETS.'
          IERROR=IERROR+1
      ENDIF
      LSCR(1)=FNO
      LSCR(2)=LNO
      lscr(3) = noinc
      LSCR(4)=ADDWB
      LSCR(5)=LPRINT
      LSCR(6)=FTR
      LSCR(7)=LTR
      LSCR(8)=TRINC
      LSCR(9)=HEADER
      LSCR(10)=INC
      LSCR(11)=THEADS
      lscr(12) = info
      lscr(13) = ntrlist
      lscr(14) = luno
      IF(IAND(LPRINT,1).EQ.1)  PRINT 2010,(LSCR(I),I=1,14),
     *   (SCR(J),J=nwrds-max+1,nwrds)
 2010 FORMAT(' PROUT PARAMS:',/14(1X,I8),/,
     *   5(10(1X,F10.3),/))
      CALL WRDISC(MUNIT,SCR,NWRDS)
      CALL wrdisc( munit, form, 20 )
      CALL wrdisc( munit, indices, 20 )
      IF( ntrlist .GT. 0 ) THEN
          CALL wrdisc( munit, tr_list, ntrlist )
          IF( IAND(lprint,1) .EQ. 1 ) 
     &       PRINT *,' tr_list:',(tr_list(i),i=1,ntrlist)
      ENDIF
      NLISTS=NLISTS+1
      HEADER=0
      THEADS=0
      ns=0
      nindices = 0
      DO 2015 I = nwrds-max+1, nwrds
 2015 SCR(I)=0.
      LNO=32768                                                          ! DEFAULT THE DEFAULTS
 2020 CALL GETOKE(TOKEN,NCHARS)                                          ! GET THE NEXT TOKEN
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 2030                                        ! WAS IT THE END OF A LINE?
      IF(NOW.EQ.1) PRINT 140
      CALL RDLINE                                                        ! GET ANOTHER LINE
      NTOKES=0
      GO TO 2020
 2030 IF(TOKEN(1:NCHARS).NE.'END'.OR.NCHARS.NE.3) GO TO 150
      RETURN                                                             !  FINISHED ALL OF THE PROUT PARAMETERS!!!
      END
