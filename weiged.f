      SUBROUTINE WEIGED(BUF,LBUF)
c
C             PROCESS WEIGHT (TRACE WEIGHTING, KILLING, REVERSING)
C             ------- ------
C
C  DOCUMENT DATE: 7 March 1991
C
C    PROCESS WEIGHT WEIGHTS INDIVIDUAL TRACES BY MULTIPLYING THE ENTIRE TRACE
C  BY A CONSTANT (SCALAR MULTIPLY).
C    A WEIGHT OF 1. RESULTS IN NO AMPLITUDE CHANGE ON THE TRACE.
C    A WEIGHT OF 0. RESULTS IN THE TRACE BEING KILLED.
C    A WEIGHT OF -1. RESULTS IN THE TRACE BEING REVERSED IN POLARITY.
C     ALL TRACES HAVE A DEFAULT WEIGHT OF 1.  THIS MEANS THAT ONLY THOSE TRACES
C  SPECIFIED BY THE USER WILL BE WEIGHTED.   WEIGHTS ARE NOT SPACIALLY VARIED,
C  THUS ONLY THOSE SHOTS ACTUALLY SPECIFIED WILL BE WEIGHTED.
C     EACH PARAMETER LIST MUST BE TERMINATED WITH THE WORD END.  THE ENTIRE SET
C  OF WEIGHT PARAMETERS MUST BE TERMINATED BY THE WORD END.
C
C  THE PARAMETER DICTIONARY
C  --- --------- ----------
C  FNO    - THE FIRST SHOT (OR RP) TO APPLY THE WEIGHTS TO.  SHOT (RP) NUMBERS
C           MUST INCREASE MONOTONICALLY.
C           PRESET=1
C  LNO    - THE LAST SHOT (RP) NUMBER TO APPLY THE WEIGHTS TO.  LNO MUST BE
C           LARGER THAN FNO IN EACH LIST AND MUST INCREASE LIST TO LIST.
C           PRESET = 999999   E.G. LNO 1
C  XWP    - RANGE-WEIGHT-PAIRS.  A LIST OF RANGE AND WEIGHT PAIRS.
C           XWP MUST BE GIVEN WITH INCREASING RANGES.  THE PROGRAM
C           COMPUTES THE ABSOLUTE VALUE OF BOTH USER RANGES AND DATA RANGES.
C           E.G. XWP 1000 3.0 2000 0.  - TRACES WITH RANGES EXACTLY EQUAL TO
C           1000. WILL BE MULTIPLIED BY 3., AND TRACES WITH A RANGE OF EXACTLY
C           2000. WILL BE MULTIPLIED 0. (OR KILLED).  ALL OTHER TRACES WILL NOT
C           BE MULTIPLIED.
C           DEFAULT=ALL 1.
C  TWP    - TRACE NUMBER-WEIGHT-PAIRS. A LIST OF TRACE NUMBERS (OF A SHOT OR RP)
C           AND WEIGHTS (LISTED IN PAIRS).  ONLY THOSE TRACES SPECIFIED WILL BE
C           MULTIPLIED.  TRACE NUMBERS MUST INCREASE WITHIN EACH LIST.
C           E.G. TWP 4 -1. 20 0. INDICATES THAT TRACE 4 WILL BE INVERTED IN
C           POLARITY AND TRACE 20 WILL BE KILLED.
C           DEFAULT=ALL 1.
c  WEIGHT - The multiplier for all traces of the specified shot/rp (FNO).
c           The DEFAULT value is 1., which means that the weight is
c           applied to shot FNO to LNO, then reset to 1. (the default value).
c           DEFAULT 1.             e.g.    fno 1234 weight 0 end
c  W      - An abbreviation for WEIGHT.  Equivalent to WEIGHT.
C  END    - TERMINATES EACH PARAMETER LIST.
C
C  NOTE *****
C  1)  EITHER XWP OR TWP MUST BE GIVEN.
C  2)  IN ORDER TO APPLY THE SAME SET OF WEIGHTS TO ALL SHOTS (RPS), LNO
C        MUST BE SET TO A VERY LARGE NUMBER. E.G. LNO 32767
c  3)  A maximum of 100 TWP or XWP pairs may be given.
C
C  EXAMPLE:
C      PROCESS WEIGHT
C         LNO 32767 XWP 1228 -1 1408 0 END
C      END
C  WILL REVERSE THE POLARITY OF ALL TRACES HAVING A RANGE OF 1228, AND WILL
C  KILL ALL TRACES WITH A RANGE OF 1408, ON SHOTS (RPS) 1 THROUGH 32767.
C
C
C  COPYRIGHTED (C) BY:
C   PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, NOVEMBER 1980
C  ALL RIGHTS RESERVED.
C
C   THE PARAMETER LIST PASSED TO WEIGEX ON THE DISC LOOKS LIKE:
C    WORD 1)  FNO (32 BIT INTEGER)
C         2)  LNO (32 BIT INTEGER)
C         3)  ADDWB (32 BIT INTEGER)
C         4)  NS (32 BIT INTEGER)  - THE NUMBER OF WORDS IN XWP OR TWP
C         5)  LTYPE (32 BIT INTEGER) - 'XWP ' OR 'TWP '
C         6)  LPRINT (32 BIT INTEGER)
c         7)  record weight (REAL)
C         9) - MAXXWP+NPARS) - XWP OR TWP ARRAY
C
C  ARGUMENTS:
C  BUF    - A SCRATCH ARRAY AT LEAST 60 32 BIT WORDS LONG.
C  LBUF   - THE SAME ARRAY BUT THE 32 BIT INTEGER EQUIVALENT.  NEEDED
C           BECAUSE PRIME FORTRAN DOESN'T ALLOW EQUIVALENCING OF ARGUMENTS.
C
c  mod 21 Feb 96 - The parameter "W" didn't work.
c  mod 5 Nov 98 - Clarify a couple of TWP error messages
c  mod 23 Sep 99 - Add IHDR, LHDR, HDR
c  mod 29 Sep 02 - set nweigs = 0 when a parameter name is read because
c                  twp 1 twp 2 twp 3 twp 4   passed edit.  ag geez.
c  mod 11 Feb 05 - Allow ihdr/hdr/lhdr to be given with no other weights.
c  mod 29 Jun 06 - Add type and type sdev
C
      PARAMETER (NPARS=13)                                              ! /* THE NUMBER OF USER PARAMETERS
      PARAMETER (MULTIV=12)                                             ! /* POINT TO THE FIRST MULTI-VALUED PARAMETER
      PARAMETER (MAXXWP=200)                                            ! /* THE MAXIMUM NUMBER OF TWPS OR XWPS THAT WEIGEX CAN HANDLE
      PARAMETER (NWRDS=MAXXWP+NPARS)                                    ! /* THE NUMBER OF WORDS IN EVERY PARAMETER LIST ON DISC
      EQUIVALENCE (VALS(1),LVALS(1))
      CHARACTER*7 NAMES(NPARS)
      CHARACTER*1 TYPES(NPARS)
      DIMENSION LENGTH(NPARS)
      CHARACTER*80 TOKEN
      DIMENSION VALS(NPARS),LVALS(NPARS)
      CHARACTER*4 CVALS(NPARS)
      CHARACTER*3 ADDWB
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      COMMON /WEIGR/ MUNIT,NLISTS,npar,nwrd
      DIMENSION BUF(111),LBUF(111)
      INTEGER FNO, hdr, type
      EQUIVALENCE (w,weight)
C
C
      EQUIVALENCE (FNO,LVALS(1)),
     2            (LNO,LVALS(2)),
     3            (ADDWB,CVALS(3)),
     4            (LPRINT,LVALS(4)),
     5            (weight,vals(5)),
     6            (w,vals(5)),
     7            (ihdr,lvals(7)),
     8            (lhdr, lvals(8)),
     9            (hdr, lvals(9)),
     &            (inverse, lvals(10)),
     1            (type, lvals(11)),
     6            (XWP,VALS(11)),
     7            (TWP,VALS(12))
      DATA NAMES/'FNO   ', 'LNO   ', 'ADDWB ', 'LPRINT', 'WEIGHT',
     *           'W     ', 'IHDR  ', 'LHDR  ', 'HDR   ', 'INVERSE',
     &           'TYPE  ',
     &           'XWP   ', 'TWP   '/
      DATA LENGTH/3,3,5,6,6,1,4,4,3,7,4,3,3/
      DATA TYPES/'L','L','A','L',2*'F',3*'L',2*'A',2*'F'/
C****
C****      SET THE PRESETS
C****
      npar = npars
      nwrd = nwrds
      FNO=1
      LNO=999999
      IADDWB=0
      LLNO = -1
      LTYPE=0
      NLISTS=0
      NS=0
      nweigs = 0
      LPRINT=0
      weight = 1.
      ihdr = 0
      lhdr = 0
      hdr = 0
      inverse = 0
      type = 0
C****
C0****    GET A PARAMETER FILE
C****
      CALL GETFIL(1,MUNIT,token,ISTAT)
C****
C****   THE CURRENT COMMAND LINE IN THE SYSTEM BUFFER MAY HAVE THE PARAMETERS.
C****   GET A PARAMETER LIST FROM THE USER.
C****
      NTOKES=1
  100 CONTINUE
      CALL GETOKE(TOKEN,NCHARS)                                         ! /* GET A TOKEN FROM THE USER PARAMETER LINE
      CALL UPCASE(TOKEN,NCHARS)                                         ! /* CONVERT THE TOKEN TO UPPERCASE
      IF(NCHARS.GT.0) GO TO 150
      IF(NOW.EQ.1) PRINT 140
  140 FORMAT(' <  ENTER PARAMETERS  >')
      CALL RDLINE                                                       ! /* GET ANOTHER USER PARAMETER LINE
      NTOKES=0
      GO TO 100
  150 CONTINUE
      NTOKES=NTOKES+1
      DO 190 I=1,NPARS                                                  ! /* SEE IF IT IS A PARAMETER NAME
      LEN=LENGTH(I)                                                     ! /* GET THE LEGAL PARAMETER NAME LENGTH
      IPARAM=I                                                          ! /* SAVE THE INDEX
      IF(TOKEN(1:NCHARS).EQ.NAMES(I)(1:LEN).AND.NCHARS.EQ.LEN) GO TO 200
  190 CONTINUE                                                          ! /* STILL LOOKING FOR THE NAME
      IF(TOKEN(1:NCHARS).EQ.'END'.AND.NCHARS.EQ.3) GO TO 1000           ! /* END OF PARAM LIST?
      IF(NS.NE.0) GO TO 230
      PRINT 191, TOKEN(1:NCHARS)
  191 FORMAT(' ***  ERROR  *** WEIGHT DOES NOT HAVE A PARAMETER ',
     *  'NAMED ',A10)
      IERROR=IERROR+1
      GO TO 100
C****
C****    FOUND THE PARAMETER NAME, NOW FIND THE VALUE
C****
  200 CONTINUE
      ns = 0
      NPARAM=IPARAM
  210 CONTINUE                                                          ! /*  NOW FIND THE VALUE
      CALL GETOKE(TOKEN,NCHARS)
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 230                                         ! /* END OF LINE?
      IF(NOW.EQ.1) PRINT 140                                            ! /* THIS ALLOWS A PARAMETER TO BE ON A DIFFERENT LINE FROM THE NAME
      CALL RDLINE                                                       ! /* GET ANOTHER LINE
      NTOKES=0
      GO TO 210
  230 CONTINUE
      IF( TYPES(NPARAM) .EQ. 'A' ) THEN
          IF(NAMES(NPARAM).EQ.'ADDWB'.AND.TOKEN(1:NCHARS).EQ.'YES')
     *       IADDWB=1
          IF(NAMES(NPARAM).EQ.'INVERSE'.AND.TOKEN(1:NCHARS).EQ.'YES')
     *       INVERSE=1
          IF( names(nparam) .EQ. 'TYPE' .AND.token(1:nchars).EQ.'SDEV')
     &        type = 1
          IF(names(nparam).EQ.'TYPE'.AND.token(1:nchars).NE.'SDEV')THEN
              PRINT *,' ***  ERROR  ***  ILLEGAL TYPE.'
              ierror = ierror
          ENDIF
          GO TO 100
      ENDIF
      CALL DCODE(TOKEN,NCHARS,AREAL,ISTAT)                              ! /* TRY AND DECODE IT
      IF(ISTAT.EQ.2) GO TO 420                                          ! /* =2 MEANS IT IS A NUMERIC
      IERROR=IERROR+1                                                   ! /* DCODE PRINTED AN ERROR
      GO TO 100
  420 IF(TYPES(NPARAM).EQ.'L') GO TO 500
      IF( names(nparam) .EQ. 'W' ) nparam = nparam - 1
      IF(NPARAM.LT.MULTIV) GO TO 490                                    ! /*  IS IT A MULTIVALUED PARAMETER
      NS=NS+1                                                           ! /*  THE TOKEN WAS A MULTI-VALUED PARAMETER
      nweigs = ns
      BUF(NS+NPARS)=AREAL
      IF( names(NPARAM) .EQ. 'XWP') LTYPE=1
      IF( names(NPARAM) .EQ. 'TWP') LTYPE=2
      GO TO 100
  490 VALS(NPARAM)=AREAL                                                ! /*  FLOATING POINT VALUES
      GO TO 100
  500 CONTINUE                                                          ! /*  32 BIT INTEGER VALUES
      LVALS(NPARAM)=AREAL
      GO TO 100
C****
C****   FINISHED A LIST, NOW DO THE ERROR AND VALIDITY CHECKS
C****
 1000 CONTINUE                                                          ! /* MAKE SURE ALL SHOT & RP NUMBERS INCREASE
      IF(FNO.GT.LLNO) GO TO 1020                                        ! /*  IS FNO LARGER THAN THE LAST LNO
      PRINT 1010
 1010 FORMAT(' ***  ERROR  ***  SHOT AND RP NUMBERS MUST INCREASE.')
      IERROR=IERROR+1
 1020 IF(LNO.GE.FNO) GO TO 1030                                         ! /* DO THEY INCREASE IN THIS LIST
      PRINT 1010
      IERROR=IERROR+1
 1030 LLNO=LNO
      IF( LTYPE .EQ. 0 .AND. weight .EQ. 1. .AND.
     &    ihdr+lhdr+hdr .EQ. 0 .AND. type .EQ. 0 ) THEN
          PRINT *,' ***  WARNING  ***  No weights given.'
          iwarn = iwarn + 1
      ENDIF
      IF(LTYPE.EQ.2) GO TO 1200
      DO 1130 I=1,nweigs,2
 1130 BUF(NPARS+I)=ABS(BUF(NPARS+I))                                    ! /* USE THE ABS VALUE OF THE RANGES
      IF(nweigs.LE.2) GO TO 1300
      DO 1150 I=3,nweigs,2
      IF(BUF(NPARS+I).GT.BUF(NPARS+I-2)) GO TO 1150
      PRINT 1140
 1140 FORMAT(' ***  ERROR  ***  THE RANGES OF XWP MUST INCREASE.')
      IERROR=IERROR+1
 1150 CONTINUE
      GO TO 1300
 1200 CONTINUE                                                          ! /*  CHECK THE TWP PARAMETER
      DO i=1,nweigs,2
         IF( buf(npars+i) .LT. 0 ) THEN
             PRINT *,' ***  ERROR  ***  Illegal TWP trace number ',
     &        buf(npars+i)
             ierror = ierror + 1
         ENDIF
      ENDDO
      IF(nweigs.LE.2) GO TO 1300
      DO I=3,nweigs,2
         IF( buf(npars+i) .LE. buf(npars+i-2) ) THEN
            PRINT *,' ***  ERROR  ***  TWP trace numbers must increase',
     &         buf(npars+i-2), buf(npars+i)
             ierror = ierror + 1
         ENDIF
      ENDDO
 1230 CONTINUE
 1300 CONTINUE                                                          ! /*  MAKE CHECKS COMMON TO BOTH XWP AND TWP
      IF(MOD(nweigs,2).EQ.0) GO TO 1320
      IF( ltype .EQ. 1 ) THEN
          token = 'XWP'
      ELSE
          token = 'TWP'
      ENDIF
      PRINT 1310,token
 1310 FORMAT(' ***  ERROR  ***  ',A4,' MUST BE IN PAIRS.')
      IERROR=IERROR+1
 1320 CONTINUE                                                          ! /*  MAKE SURE THE TIMES ARE OK
C****
C****      WRITE THE PARAMETER LIST TO DISC
C****
      IF(nweigs.LE.MAXXWP) GO TO 1360
      ITEMP=MAXXWP / 2
      PRINT 1350,ITEMP
 1350 FORMAT(' ***  ERROR  ***  WEIGEX CAN HANDLE ONLY ',I3,' WEIGHTS')
      IERROR=IERROR+1
 1360 CONTINUE
      LBUF(1)=FNO
      LBUF(2)=LNO
      LBUF(3)=IADDWB
      LBUF(4)=nweigs
      LBUF(5)=LTYPE
      LBUF(6)=LPRINT
      buf(7) = weight
      lbuf(8) = ihdr
      lbuf(9) = lhdr
      lbuf(10) = hdr
      lbuf(11) = inverse
      lbuf(12) = type
      ITEMP=NPARS+1
      ITEMP1=NPARS+nweigs
      IF( IAND(LPRINT,1) .EQ. 1 ) THEN
          PRINT *, (LBUF(I),I=1,6),buf(7)
          PRINT *, (lbuf(i),i=8,11)
          PRINT *, (BUF(J),J=ITEMP,ITEMP1)
      ENDIF
      CALL WRDISC(MUNIT,BUF,NWRDS)
      NLISTS=NLISTS+1
      NS=0
      nweigs = 0
      LLNO=LNO
      LNO=32768                                                         ! /* DEFAULT THE DEFAULTS
      weight = 1.
 2020 CALL GETOKE(TOKEN,NCHARS)                                         ! /* GET THE NEXT TOKEN
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 2030                                        ! /* WAS IT THE END OF A LINE?
      IF(NOW.EQ.1) PRINT 140
      CALL RDLINE                                                       ! /* GET ANOTHER LINE
      NTOKES=0
      GO TO 2020
 2030 IF(TOKEN(1:NCHARS).NE.'END'.OR.NCHARS.NE.3) GO TO 150
      RETURN                                                            ! /*  FINISHED ALL OF THE PARAMETERS!!!
      END
