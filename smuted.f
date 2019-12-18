      SUBROUTINE SMUTED(BUF,LBUF)
C                               PROCESS SMUTE
C                               ------- -----
C
C  DOCUMENT DATE: 9 June 1992
C
c     Process SMUTE performs either a surgical mute or a tail mute to
c  the seismic traces.  A surgical mute zeroes a portion of the trace
c  according to user given start and end times.  A tail mute zeroes the
c  end or tail of the trace starting at a user given time.  SMUTE is
c  similiar to process MUTE except that MUTE always zeroes the front or
c  beginning of the trace.
c     SMUTE was designed for removing occassional bad areas, so spatial
c  interpolation is turned off except when using tail muting (parameter
c  XTP, which is similar to process MUTE parameter XTP).
c     As with process MUTE, a 5 sample linear ramp is applied to the
c  mute zone edges in order to reduce "edge effects".
c     Traces not specified by are muted according to interpolation and
c  extrapolation of the mute times of adjacent traces and shots/rps,
c  unless the INTERP parameter is used.
c     Surgical muting and tail muting may not be perform simultaneously.
c  i.e.  XTP, TTP, XSETS, TSETS are all mutually exclusive.
c  
c     Each parameter list must be terminated with the word END.  Spatial
c  variation is obtained by giving multiple lists or control points.
c  (See doc/syntax).  The spatial variation may be turned by using the 
c  INTERP parameter.
C
C  THE PARAMETER DICTIONARY
C  --- --------- ----------
C  FNO    - THE FIRST SHOT (OR RP) TO APPLY THE MUTES TO.  SHOT (RP) NUMBERS
C           MUST INCREASE MONOTONICALLY.
C           PRESET=1
C  LNO    - THE LAST SHOT (RP) NUMBER TO APPLY THE MUTES TO.  LNO MUST BE
C           LARGER THAN FNO IN EACH LIST AND MUST INCREASE LIST TO LIST.
C           DEFAULT=FNO
C  XTP    - RANGE-TIME-PAIRS.  A LIST OF RANGE AND TAIL MUTE TIME PAIRS.  MUTE TIMES
C           FOR RANGES NOT SPECIFIED ARE OBTAINED THROUGH LINEAR INTERPOLATION
C           IF THE RANGE IS BETWEEN TWO RANGES SPECIFIED.  TRACES WITH
C           A RANGE LESS THAN THE SMALLEST GIVEN RANGE WILL BE MUTED TO THE MUTE
C           TIME OF THE SMALLEST GIVEN RANGE.  LIKEWISE, RANGES LARGER THAN THE
C           LARGEST GIVEN RANGE WILL BE MUTED TO THE MUTE TIME OF THE LARGEST
C           GIVEN RANGE.  XTP MUST BE GIVEN WITH INCREASING RANGES.  THE PROGRAM
C           COMPUTES THE ABSOLUTE VALUE OF BOTH USER RANGES AND DATA RANGES.
C           E.G. XTP 1000 3.0 2000 4.0  - TRACES WITH RANGES LESS THAN 1000 WILL
C           BE MUTED TO 3. SECONDS, TRACES WITH RANGES GREATER THAN 2000 WILL
C           BE MUTED TO 4. SECONDS, AND TRACES WITH RANGE BETWEEN 1000 AND
C           2000 WILL BE MUTED BETWEEN 3. AND 4. SECONDS (PROPORIONATELY).
C           PRESET=NONE
C  TTP    - TRACE NUMBER-TIME-PAIRS.  A LIST OF TRACE NUMBERS (OF A SHOT OR RP)
C           AND TAIL MUTE TIMES (LISTED IN PAIRS).  THE MUTE TIME FOR A TRACE
C           BETWEEN TWO TRACES THAT ARE SPECIFIED IS OBTAINED THROUGH LINEAR
C           INTERPOLATION OF THE MUTE TIMES OF THE SPECIFIED TRACES. TRACES WITH
C           A TRACE NUMBER LESS THAN THE SMALLEST GIVEN WILL BE MUTED TO THE MUTE
C           TIME OF THE SMALLEST TRACE NUMBER.  LIKEWISE, TRACES WITH A TRACE
C           NUMBER LARGER THAN THE LARGEST GIVEN WILL BE MUTED TO THE MUTE TIME
C           OF THE LARGEST GIVEN.  TTP MUST BE GIVEN IN INCREASING TRACE NUMBERS.
C           E.G.  TTP 4 2. 20 5. - TRACES 1 THRU 4 WILL BE MUTED TO 2. SECONDS,
C           TRACES 20 AND UP WILL BE MUTED TO 5. SECONDS, AND TRACES 5 THRU 19
C           WILL BE MUTED BETWEEN 2. AND 5. SECONDS (PROPORTIONATELY).
C           PRESET=NONE
C  XSETS  - RANGE-START TIME-END TIME TRIPLES.  A LIST OF RANGE AND MUTE WINDOW
C           TIMES.  ONLY THOSE TRACES ACTUALLY SPECIFIED ARE MUTED.  SETS MUST
C           BE GIVEN SO THAT THE RANGES INCREASE.  THE SIGN OF THE RANGE IS
C           IGNORED SINCE THE PROGRAM USES THE ABSOLUTE VALUE OF THE TRACE
C           RANGES AS WELL AS THE RANGES SPECIFIED BY THE USER.
C           E.G.  1000 2.5 3.0 2000 3.990 4.000  -  TRACES WITH A RANGE OF 1000
C           WILL BE MUTEED FROM TIME 2.5 TO 3.0 AND TRACES WITH A RANGE OF 2000
C           WILL BE MUTEED FROM TIME 3.99 TO 4.0
C           PRESET=NONE
C  TSETS  - TRACE NUMBER-START TIME-END TIME TRIPLES.  A LIST OF TRACE NUMBER
C           AND MUTE WINDOW TIMES.  ONLY THOSE TRACES ACTUALLY SPECIFIED ARE
C           MUTED.  TSETS MUST BE GIVEN SO THAT THE TRACE NUMBERS INCREASE.
C           E.G.  TSETS 5 0.9 1.0 15 3.0 4.0  -  TRACE NUMBER 5 WILL BE MUTED
C           FROM TIME .9 TO TIME 1.0 AND TRACE 15 WILL BE MUTED FROM 3. TO 4.
C           SECONDS.
C           PRESET=NONE.
C  ADDWB  - WHEN GIVEN A VALUE OF YES, THE MUTE TIMES GIVEN VIA XSETS OR TSETS
C           WILL BE ADDED TO THE WATER BOTTOM TIME OF THE TRACE.  (WATER BOTTOM
C           TIMES MAY BE ENTERED VIA PROCESS WBT).
C           PRESET=NO
c  INTERP - A binary switch (on or off ) indicating that the start and end 
c           times for traces not specified via XSETS/TSETS and XTP/TTP 
c           should be calculated by interpolation or "extension".  Traces
c           between specified traces will be muted using times linearly inter-
c           polated.  Traces not between specified traces will be muted using
c           the "closest" trace (times held constant).  Shots/rps not specified 
c           will be calculated through interpolation and "extension" in a similar 
c           manner.
c           Preset = off for xset/tsets           e.g.   interp on
c           Preset = on for xtp/ttp              e.g    interp off
C  END    - TERMINATES EACH PARAMETER LIST.
C
C  NOTE *****
C  1)  EITHER XSETS, TSETS, XTP, OR TTP MUST BE GIVEN.
C  2)  ALL TIMES ARE IN SECONDS.
C
C
C  WRITTEN AND COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, MARCH 1982
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
C
C
C   THE PARAMETER LIST PASSED TO SMUTEX ON THE DISC LOOKS LIKE:
C    WORD 1)  FNO (32 BIT INTEGER)
C         2)  LNO (32 BIT INTEGER)
C         3)  ADDWB (32 BIT INTEGER)
C         4)  NS (32 BIT INTEGER)  - THE NUMBER OF WORDS IN XSETS OR TSETS
C         5)  LTYPE (32 BIT INTEGER) - 'XSETS ' OR 'TSETS '
C         6)  LPRINT (32 BIT INTEGER)
C         7) - MAXSET+NPARS) - XSETS OR TSETS ARRAY
C
C  ARGUMENTS:
C  BUF    - A SCRATCH ARRAY AT LEAST 60 32 BIT WORDS LONG.
C  LBUF   - THE SAME ARRAY BUT THE 32 BIT INTEGER EQUIVALENT.  NEEDED
C           BECAUSE PRIME FORTRAN DOESN'T ALLOW EQUIVALENCING OF ARGUMENTS.A
c  modified August 1989 for interp by pch
c  mod 5 Feb 92 - Bad error check when more than 1 xset triple was given.
c  mod 10 June 92 - Add ttp and xtp as done by UTIG
c                 - Add interp
c  mod 1 Apr 99 - Make interp preset = 1 when xtp is given
c  mod 6 Oct 02 - Allow fno 0
c  mod 20 Apr 07 - Allow ADDWB 2X for hanging inner mute from the multiple.
C
      PARAMETER ( NPARS = 9 )                                           ! THE NUMBER OF USER PARAMETERS
      PARAMETER (MAXSET=300)                                            ! THE MAXIMUM NUMBER OF TSETSS OR XSETSS THAT SMUTEX CAN HANDLE
      PARAMETER (NWRDS=MAXSET+NPARS)
      PARAMETER (MULTIV=6)                                              ! POINT TO THE FIRST MULTI-VALUED PARAMETER
      CHARACTER*6 NAMES(NPARS)
      CHARACTER*1 TYPES(NPARS)
      DIMENSION LENGTH(NPARS)
      CHARACTER*80 TOKEN
      DIMENSION VALS(NPARS),LVALS(NPARS)
      EQUIVALENCE (VALS(1),LVALS(1))
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      COMMON /SMUTER/ MUNIT,NLISTS
      DIMENSION BUF(111),LBUF(111)
      INTEGER FNO
C
C
      EQUIVALENCE (FNO,LVALS(1)),
     2            (LNO,LVALS(2)),
     3            (ADDWB,LVALS(3)),
     4            (LPRINT,LVALS(4)),
     5            ( interp, lvals(5) ),
     6            (XSETS,VALS(6)),
     7            (TSETS,VALS(6)),
     8            (XTP,VALS(8)),
     9            (TTP,VALS(8))
      DATA NAMES/'FNO   ','LNO   ','ADDWB','LPRINT','INTERP',
     *           'XSETS ','TSETS ','XTP','TTP'/
      DATA LENGTH/3,3,5,6,6,5,5,3,3/
      DATA TYPES/'L','L','A','L','A',4*'F'/
C****
C****      SET THE PRESETS
C****
      FNO=1
      LNO = 9999999
      XSETS=-1.
      LPRINT=0
      IADDWB=0
      LLNO = 0
      NLISTS=0
      NS=0
      nsmuts = 0
      interp = -1
      xtp = -1.
C****
C****    GET A PARAMETER FILE
C****
      CALL GETFIL(1,MUNIT,token,ISTAT)
C****
C****   THE CURRENT COMMAND LINE IN THE SYSTEM BUFFER MAY HAVE THE PARAMETERS.
C****   GET A PARAMETER LIST FROM THE USER.
C****
      NTOKES=1
  100 CONTINUE
      CALL GETOKE(TOKEN,NCHARS)                                         ! GET A TOKEN FROM THE USER PARAMETER LINE
      CALL UPCASE(TOKEN,NCHARS)                                         ! CONVERT THE TOKEN TO UPPERCASE
      IF(NCHARS.GT.0) GO TO 150
      IF(NOW.EQ.1) PRINT 140
  140 FORMAT(' <  ENTER PARAMETERS  >')
      CALL RDLINE                                                       ! GET ANOTHER USER PARAMETER LINE
      NTOKES=0
      GO TO 100
  150 CONTINUE
      NTOKES=NTOKES+1
      DO 190 I=1,NPARS                                                  ! SEE IF IT IS A PARAMETER NAME
      LEN=LENGTH(I)                                                     ! GET THE LEGAL PARAMETER NAME LENGTH
      IPARAM=I                                                          ! SAVE THE INDEX
      IF(TOKEN(1:NCHARS).EQ.NAMES(I)(1:LEN).AND.NCHARS.EQ.LEN) GO TO 200
  190 CONTINUE                                                          ! STILL LOOKING FOR THE NAME
      IF(TOKEN(1:NCHARS).EQ.'END'.AND.NCHARS.EQ.3) GO TO 1000           ! END OF PARAM LIST?
      IF(NS.NE.0) GO TO 230
      PRINT 191, TOKEN(1:NCHARS)
  191 FORMAT(' ***  ERROR  *** SMUTE DOES NOT HAVE A PARAMETER ',
     *  'NAMED ',A10)
      IERROR=IERROR+1
      GO TO 100
C****
C****    FOUND THE PARAMETER NAME, NOW FIND THE VALUE
C****
  200 CONTINUE
      NPARAM=IPARAM
  210 CONTINUE                                                          !  NOW FIND THE VALUE
      CALL GETOKE(TOKEN,NCHARS)
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 230                                         ! END OF LINE?
      IF(NOW.EQ.1) PRINT 140                                            ! THIS ALLOWS A PARAMETER TO BE ON A DIFFERENT LINE FROM THE NAME
      CALL RDLINE                                                       ! GET ANOTHER LINE
      NTOKES=0
      GO TO 210
  230 CONTINUE
      IF( TYPES(NPARAM) .EQ. 'A' ) THEN
          IF( NAMES(NPARAM) .EQ. 'ADDWB' ) THEN
              IF( TOKEN(1:NCHARS) .EQ. 'YES' ) iaddwb = 1
              IF( token(1:nchars) .EQ. '2X' ) iaddwb = 2
              IF( iaddwb .EQ. 0 ) THEN
                  PRINT *,' ***  ERROR  ***  Bad ADDWB value.'
                  ierror = ierror + 1
              ENDIF
          ENDIF
          IF( names(nparam) .EQ. 'INTERP' ) THEN
              IF( token(1:nchars) .EQ. 'YES' ) interp = 1
              IF( token(1:nchars) .EQ. 'NO' ) interp = 0
              IF( token(1:nchars) .EQ. 'ON' ) interp = 1
              IF( token(1:nchars) .EQ. 'OFF' ) interp = 0
              IF( token(1:nchars) .EQ. '1' ) interp = 1
              IF( token(1:nchars) .EQ. '0' ) interp = 0
          ENDIF
          GOTO 100
      ENDIF
      CALL DCODE(TOKEN,NCHARS,AREAL,ISTAT)                              ! TRY AND DECODE IT
      IF( ISTAT .NE. 2 ) THEN                                           ! =2 MEANS IT IS A NUMERIC
          IERROR=IERROR+1                                               ! DCODE PRINTED AN ERROR
          GOTO 100
      ENDIF
      IF( TYPES(NPARAM) .EQ. 'L' ) THEN
          LVALS(NPARAM)=AREAL
          GOTO 100
      ENDIF
      IF( nparam .GE. multiv ) THEN                                     !  IS IT A MULTIVALUED PARAMETER
          ns = ns + 1                                                   ! count the parameters
          BUF(NS+NPARS)=AREAL
          IF( names(nparam) .EQ. 'TTP' .OR. names(nparam).EQ.'XTP') THEN
              IF( MOD(ns,3) .EQ. 2 ) THEN
                  ns = ns + 1
                  buf(ns+npars) = 99999.                                ! make the end time very large.
              ENDIF
          ENDIF
          nsmuts = ns
          LTYPE=1                                                       !  =1 MEANS XSETS
          IF( names(nparam) .EQ. 'TSETS' .OR.
     &        names(nparam) .EQ. 'TTP' ) ltype = 2                      ! = 2 means TSETS/TTP
          XSETS=0.                                                      ! INDICATE THAT TSETS OR XSETS WAS GIVEN
      ELSE
          VALS(NPARAM) = AREAL                                          !  FLOATING POINT VALUES
      ENDIF
      GOTO 100
C****
C****  FINISHED A LIST, NOW DO THE ERROR AND VALIDITY CHECKS
C****
 1000 CONTINUE                                                          ! MAKE SURE ALL SHOT & RP NUMBERS INCREASE
      IF( interp .EQ. -1 ) THEN
c     preset interp to being on when tsets/xsets and off on xtp/ttp
          IF( buf(npars+3) .NE. 99999. ) THEN                           ! ttp/xtp has end time of 99999
              interp = 1
          ELSE
              interp = 0
          ENDIF
      ENDIF
      IF( LNO .EQ. 9999999 ) LNO=FNO                                    ! DEFAULT LNO TO FNO
      IF( (FNO .LE. LLNO .OR. lno .LT. fno) .AND. fno .GT. 0 ) THEN     !  IS FNO LARGER THAN THE LAST LNO
          PRINT *,' ***  ERROR  ***  SHOT AND RP NUMBERS MUST INCREASE.'
          IERROR = IERROR + 1
      ENDIF
      LLNO=LNO
      IF( XSETS .NE. 0. ) THEN
          PRINT *,' ***  ERROR  ***  XSETS OR TSETS MUST BE GIVEN.'
          IERROR = IERROR + 1
      ENDIF
      IF(LTYPE.EQ.2) GO TO 1200                                         !  MAKE SURE THE RANGES OF XSETS INCREASE
      DO 1130 I=1,nsmuts,3
 1130 BUF(NPARS+I)=ABS(BUF(NPARS+I))                                    ! USE THE ABS VALUE OF THE RANGES
      IF(nsmuts.LE.2) GO TO 1300
      DO 1150 I = 3, nsmuts-1, 3      
         IF( BUF(NPARS+I+1) .LE. BUF(NPARS+I-2) ) THEN
             PRINT *,' ***  ERROR  ***  RANGES MUST INCREASE.'
             IERROR=IERROR+1
         ENDIF
 1150 CONTINUE
      GO TO 1300
 1200 CONTINUE                                                          !  CHECK THE TSETS PARAMETER
      DO 1220 I=1,nsmuts,3
      IF(BUF(NPARS+I).GE.0.) GO TO 1220                                 ! ALLOW A TRACE NUMBER OF ZERO
      PRINT 1210,BUF(NPARS+I)
 1210 FORMAT(' ***  ERROR  ***  ILLEGAL TTP/TSETS.',F10.4)
      IERROR=IERROR+1
 1220 CONTINUE
      IF(nsmuts.LE.3) GO TO 1300
      ITEMP=nsmuts-1
      DO I=3,ITEMP,3
         IF( BUF(NPARS+I+1) .LE. BUF(NPARS+I-2) ) THEN
             PRINT *,
     &       ' ***  ERROR  ***  Trace numbers or ranges must increase.',
     &       buf(npars+i+1)
             IERROR=IERROR+1
         ENDIF
      ENDDO
 1300 CONTINUE                                                          !  MAKE CHECKS COMMON TO BOTH XSETS AND TSETS
      IF(MOD(nsmuts,3).EQ.0) GO TO 1320
      PRINT 1310
 1310 FORMAT(' ***  ERROR  ***  TSETS AND XSETS MUST BE IN TRIPLES.')
      IERROR=IERROR+1
 1320 CONTINUE                                                          !  MAKE SURE THE TIMES ARE OK
      DO 1340 I=1,nsmuts,3
      IF(BUF(NPARS+I+1).GE.-10..AND.BUF(NPARS+I+1).LT.50. .OR.
     &   buf(npars+i+1) .EQ. 99999. ) GO TO 1340
      PRINT 1330,BUF(NPARS+I+3)
 1330 FORMAT(' ***  ERROR  ***  ILLEGAL SMUTE TIME OF ',F10.4)
      IERROR=IERROR+1
 1340 CONTINUE
C****
C****      WRITE THE PARAMETER LIST TO DISC
C****
      IF(nsmuts.LE.MAXSET) GO TO 1360
      ITEMP=MAXSET/3
      PRINT 1350,ITEMP
 1350 FORMAT(' ***  ERROR  ***  SMUTE CAN HANDLE ONLY ',I3,' TRIPLES')
      IERROR=IERROR+1
 1360 CONTINUE
      LBUF(1)=FNO
      LBUF(2)=LNO
      LBUF(3)=IADDWB
      LBUF(4)=nsmuts
      LBUF(5)=LTYPE
      LBUF(6)=LPRINT
      lbuf(7) = interp
      ITEMP=NPARS+1
      ITEMP1 = itemp + nsmuts - 1
      IF( IAND(LPRINT,1) .EQ. 1 ) PRINT *,' SMUTE ',(LBUF(I),I=1,7),
     *   (BUF(J),J=ITEMP,ITEMP1)
      CALL WRDISC(MUNIT,BUF,NWRDS)
      NLISTS=NLISTS+1
      LLNO=LNO
      LNO = 9999999                                                     ! DEFAULT THE DEFAULTS
      LTYPE=0
      NS=0
      nsmuts = 0
 2020 CALL GETOKE(TOKEN,NCHARS)                                         ! GET THE NEXT TOKEN
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 2030                                        ! WAS IT THE END OF A LINE?
      IF(NOW.EQ.1) PRINT 140
      CALL RDLINE                                                       ! GET ANOTHER LINE
      NTOKES=0
      GO TO 2020
 2030 IF(TOKEN(1:NCHARS).NE.'END'.OR.NCHARS.NE.3) GO TO 150
      RETURN                                                            !  FINISHED ALL OF THE PARAMETERS!!!
      END
