      SUBROUTINE MIXED(BUF,LBUF)
C                              PROCESS MIX
C                              ------- ---
C
C
C    PROCESS MIX PERFORMS A RUNNING, WEIGHTED, DIP MIX.  MIX IS DEFINED TO BE
C  THE SUM OR ADDITION OF ADJACENT (IN A NUMERICAL SENSE).
c     There are three types of mix:
c 1) A running roll-along mix - The mix crosses record boundaries.  If the
c    data are shots, the last trace of a shot is mixed with the first trace
c    of the next shot.  e.g. A three trace equally weighted zero dip
c    unweighted running mix will output the following traces:
c          TRACE 1 = INPUT TRACES 1
C          TRACE 2 = INPUT TRACES 1+2
C          TRACE 3 = INPUT TRACES 1+2+3
C          TRACE 4 = INPUT TRACES 2+3+4
C          TRACE 5 = INPUT TRACES 3+4+5
C          ETC.
c 2) A running record mix - The mix stops and the end of a shot and starts
c    over on the next shot.  A three trace equally weighted zero dip running
c    record mix of 24 trace shots will output the following traces:
c          TRACE 1 = INPUT TRACES 1
C          TRACE 2 = INPUT TRACES 1+2
C          TRACE 3 = INPUT TRACES 1+2+3
C          TRACE 4 = INPUT TRACES 2+3+4
c                .     .
c                .     .
c          TRACE 24 = INPUT TRACES 22+23+24
c 3) A record mix - This mix starts over after every mix set and does not
c    output the tappered traces.  e.g. A three trace record mix of 24 trace
c    shots will output the following;
c          TRACE 1 = INPUT TRACES 1+2+3
c          TRACE 2 = INPUT TRACES 4+5+6
c          TRACE 3 = INPUT TRACES 7+8+9
c            .        .
c            .        .
c          TRACE 8 = INPUT TRACES 22+23+24
c
C     A WEIGHTED OR TAPERED MIX ALLOWS EACH TRACE TO BE INDEPENDANTLY SCALED
C  PRIOR TO THE MIX.  REFERING TO THE EARLIER EXAMPLE, A 1 2 1 WEIGHTED MIX
C  WILL HAVE OUTPUT TRACE 3 CONTAINING (TRACE1)*1+(TRACE2)*2+(TRACE3)*1, OUTPUT
C  TRACE 4=(TRACE2)*1.+(TRACE3)*2.+(TRACE4)*1.
C     A DIP MIX IS A MIX WITH EACH TRACE SHIFTED IN TIME PRIOR TO THE MIX.  THE
C  TIME SHIFT IS RELATIVE TO THE FIRST TRACE WITHIN THE MIX SO THAT THE FIRST
C  INPUT TRACE IS NOT SHIFTED, THE SECOND IS SHIFTED BY DIP SECONDS, THE THIRD
C  BY DIP*2 SECONDS.
C     EACH PARAMETER LIST MUST BE TERMINATED WITH THE WORD END.  THE ENTIRE SET
C  OF MIX PARAMETERS MUST BE TERMINATED BY THE WORD END.
C
C  THE PARAMETER DICTIONARY
C  --- --------- ----------
C  FNO    - THE FIRST SHOT (OR RP) TO APPLY THE MIX TO.  SHOT (RP) NUMBERS
C           MUST INCREASE MONOTONICALLY.
C           PRESET=1
C  LNO    - THE LAST SHOT (RP) NUMBER TO APPLY THE MIX TO.  LNO MUST BE
C           LARGER THAN FNO IN EACH LIST AND MUST INCREASE LIST TO LIST.
C           DEFAULT = 999999
c  TYPE   - The type of mix to be performed.
c         =1, Running mix.
c         =2, Running record mix.
c         =3, Record mix.
c           Preset = 1                e.g. type 3
C  WEIGHT - A LIST OF WEIGHTS TO USE IN THE MIX.  THE TOTAL NUMBER OF WEIGHTS
C           GIVEN INDICATES THE NUMBER OF TRACES IN THE MIX. E.G. A 3 TRACE
C           EQUALLY WEIGHTED MIX WILL BE DONE BY GIVING WEIGHT 1 1 1.
C           AT LEAST 2 WEIGHTS MUST BE GIVEN AND NO MORE THAN 10 MAY BE GIVEN.
C           REQUIRED.
C  DIP    - THE AMOUNT OF SHIFT, IN SECONDS, TO APPLY TO SUCCESSIVE INPUT TRACES
C           WITHIN EACH MIXED TRACE.
C           PRESET=0.
C  MAXLEN - THE MAXIMUM LENGTH, IN SECONDS, OF THE LARGEST INPUT TRACE
C           (NEEDED FOR ALLOCATING AP MEMORY).
C           PRESET=THE LENGTH OF THE FIRST INPUT TRACE.
C  END    - TERMINATES EACH PARAMETER LIST.
C
C  NOTE *****
C    1) AT LEAST 2, BUT NOT MORE THAN 10, WEIGHTS MUST BE GIVEN.
C    2) NEITHER WEIGHT NOR DIP MAY BE CHANGED WITHIN A JOB.
C
C
C  WRITTEN AND COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, 11 MARCH 1981
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
c  mods:
c   22 March 1991 by pch to add the mix type parameter
C
C   THE PARAMETER LIST PASSED TO MIXEX ON THE DISC LOOKS LIKE:
C    WORD 1)  FNO (INTEGER*4)
C         2)  LNO (INTEGER*4)
C         3)  DIP (FLOATING POINT)
C         4)  NWEIGS (INTEGER*4) - THE NUMBER OF TRACES IN THE MIX
C         5)  LPRINT (INTEGER*4)
C         6)  MAXLEN (REAL)
c         7)  type (INTEGER)
c         8)  hdr
c         9)  lhdr
c        10)  ihdr
C        11) - MAXMIX+NPARS) - THE MIX WEIGHTS.
C
C  ARGUMENTS:
C  BUF    - A SCRATCH ARRAY AT LEAST 60 32 BIT WORDS LONG.
C  LBUF   - THE SAME ARRAY BUT THE INTEGER*4 EQUIVALENT.  NEEDED
C           BECAUSE PRIME FORTRAN DOESN'T ALLOW EQUIVALENCING OF ARGUMENTS.
C
C
C
      PARAMETER (NPARS=10)                                               /* THE NUMBER OF USER PARAMETERS
      PARAMETER (MULTIV=10)                                              /* THE FIRST MULTI-VALUED PARAMETER
      PARAMETER (MAXMIX=10)                                             /* THE MAXIMUM NUMBER OF TRACES THAT MAY BE MIXED
      CHARACTER*6 NAMES(NPARS)
      CHARACTER*1 TYPES(NPARS)
      DIMENSION LENGTH(NPARS)
      CHARACTER*80 TOKEN
      DIMENSION VALS(NPARS),LVALS(NPARS)
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      EQUIVALENCE (VALS(1),LVALS(1))
      COMMON /MIXR/ MUNIT,NLISTS, nwrds
      DIMENSION BUF(111),LBUF(111)
      INTEGER FNO, type, hdr, lhdr, ihdr
      REAL MAXLEN
C
C
      EQUIVALENCE (FNO,LVALS(1)),
     2            (LNO,LVALS(2)),
     3            (DIP,VALS(3)),
     4            (LPRINT,LVALS(4)),
     5            (MAXLEN,VALS(5)),
     6            (type,lvals(6)),
     7            (hdr,lvals(7)),
     8            (lhdr,lvals(8)),
     9            (ihdr,lvals(9)),
     *            (WEIGHT,VALS(10))
      DATA NAMES/ 'FNO   ', 'LNO   ', 'DIP   ', 'LPRINT', 'MAXLEN',
     *            'TYPE  ', 'HDR   ', 'LHDR  ', 'IHDR  ', 'WEIGHT'/
      DATA LENGTH/3,3,3,6,6,4,3,4,4,6/
      DATA TYPES/'L','L','F','L','F','L',3*'L','F'/
C****
C****      SET THE PRESETS
C****
      FNO=1
      LNO = 999999
      MAXLEN=0.
      DIP=0.
      WEIGHT=0.
      LPRINT=0
      LLNO = 0
      NLISTS=0
      type = 1
      hdr = 0
      ihdr = 0
      lhdr = 0
      NS=0
C****
      CALL GETFIL(1,MUNIT,token,ISTAT)
C****   THE CURRENT COMMAND LINE IN THE SYSTEM BUFFER MAY HAVE THE PARAMETERS.
C****   GET A PARAMETER LIST FROM THE USER.
C****
      NTOKES=1
  100 CONTINUE
      CALL GETOKE(TOKEN,NCHARS)                                          /* GET A TOKEN FROM THE USER PARAMETER LINE
      CALL UPCASE(TOKEN,NCHARS)                                         /* CONVERT THE TOKEN TO UPPERCASE
      IF( NCHARS .LE. 0 ) THEN
          IF(NOW.EQ.1) PRINT *,' <  ENTER PARAMETERS  >'
          CALL RDLINE                                                        /* GET ANOTHER USER PARAMETER LINE
          NTOKES=0
          GOTO 100
      ENDIF
  150 NTOKES=NTOKES+1
      DO 190 I = 1, NPARS                                                  /* SEE IF IT IS A PARAMETER NAME
         LEN = LENGTH(I)                                                      /* GET THE LEGAL PARAMETER NAME LENGTH
         IPARAM = I                                                          /* SAVE THE INDEX
         IF( TOKEN(1:NCHARS) .EQ. NAMES(I)(1:LEN) .AND.
     &      NCHARS.EQ.LEN) GO TO 200
  190 CONTINUE                                                          /* STILL LOOKING FOR THE NAME
      IF(TOKEN(1:NCHARS).EQ.'END'.AND.NCHARS.EQ.3) GO TO 1000            /* END OF PARAM LIST?
      IF(NS.NE.0) GO TO 230
      PRINT *,' ***  ERROR  *** MIX DOES NOT HAVE A PARAMETER NAMED ',
     &      TOKEN(1:NCHARS)
      IERROR=IERROR+1
      GO TO 100
C****
C****    FOUND THE PARAMETER NAME, NOW FIND THE VALUE
C****
  200 CONTINUE
      NPARAM=IPARAM
  210 CONTINUE                                                           /*  NOW FIND THE VALUE
      CALL GETOKE(TOKEN,NCHARS)
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF( NCHARS .LE. 0 ) THEN                                          /* END OF LINE?
          IF(NOW.EQ.1) PRINT *,' <  ENTER PARAMETERS  >'
          CALL RDLINE                                                        /* GET ANOTHER LINE
          NTOKES=0
          GOTO 210
      ENDIF
  230 CONTINUE
      IF(TYPES(NPARAM).NE.'A') GO TO 240
      GO TO 100
  240 CONTINUE
      CALL DCODE(TOKEN,NCHARS,AREAL,ISTAT)                              /* TRY AND DECODE IT
      IF(ISTAT.EQ.2) GO TO 420                                          /* =2 MEANS IT IS A NUMERIC
      IERROR=IERROR+1                                                    /* DCODE PRINTED AN ERROR
      GO TO 100
  420 IF(TYPES(NPARAM).EQ.'L') GO TO 500
      IF(NPARAM.LT.MULTIV) GO TO 490                                     /*  IS IT A MULTIVALUED PARAMETER
      NS=NS+1                                                           /*  THE TOKEN WAS A MULTI-VALUED PARAMETER
      BUF(NS+NPARS)=AREAL
      GO TO 100
  490 VALS(NPARAM)=AREAL                                                 /*  FLOATING POINT VALUES
      GO TO 100
  500 CONTINUE                                                          /*  32 BIT INTEGER VALUES
      LVALS(NPARAM)=AREAL
      GO TO 100
C****
C****   FINISHED A LIST, NOW DO THE ERROR AND VALIDITY CHECKS
C****
 1000 CONTINUE                                                           /* MAKE SURE ALL SHOT & RP NUMBERS INCREASE
      IF(LNO.EQ.32768) LNO=FNO                                          /* DEFAULT LNO TO FNO
      IF( FNO .LE. LLNO .OR. lno .LT. fno ) THEN
          PRINT *,' ***  ERROR  ***  SHOT AND RP NUMBERS MUST INCREASE.'
          IERROR=IERROR+1
      ENDIF
      LLNO=LNO
      IF( NS .LE. 1 ) THEN
          PRINT *,
     *    ' ***  ERROR  ***  AT LEAST 2 TRACE WEIGHTS MUST BE GIVEN.'
          IERROR=IERROR+1
      ENDIF
      IF( ABS(DIP) .GT. 5. ) THEN
          PRINT *,' ***  ERROR  ***  A DIP OF ',dip,' SECONDS EXCEEDS ',
     *     '5.'
          IERROR=IERROR+1
      ENDIF
      IF( type .LT. 1 .OR. type .GT. 4 ) THEN
          PRINT *,' ***  ERROR  ***  TYPE must be 1-4.'
          ierror = ierror + 1
      ENDIF
C****
C****      WRITE THE PARAMETER LIST TO DISC
C****
      IF( NS .GT. MAXMIX .AND. type .NE. 4 ) THEN
          ITEMP=MAXMIX
          PRINT *,
     *    ' ***  ERROR  ***  MIX CAN HANDLE ONLY ',itemp,' WEIGHTS.'
          IERROR=IERROR+1
      ENDIF
      IF( hdr+lhdr+ihdr .NE. 0 ) type = 4
      LBUF(1)=FNO
      LBUF(2)=LNO
      BUF(3)=DIP
      LBUF(4)=NS
      LBUF(5)=LPRINT
      BUF(6)=MAXLEN
      lbuf(7) = type
      lbuf(8) = hdr
      lbuf(9) = lhdr
      lbuf(10) = ihdr
      ITEMP=NPARS+1
      nwrds = NPARS + NS
      IF( IAND(LPRINT,1) .EQ. 1 )  THEN
          PRINT *,(LBUF(I),I=1,2),
     *  BUF(3),LBUF(4),LBUF(5),BUF(6),(lbuf(j),j=7,10)
          PRINT *,(BUF(J),J=ITEMP,nwrds)
      ENDIF
      CALL WRDISC(MUNIT,BUF,NWRDS)
      NLISTS=NLISTS+1
      NS=0
      LLNO=LNO
      LNO=32768                                                          /* DEFAULT THE DEFAULTS
 2020 CALL GETOKE(TOKEN,NCHARS)                                          /* GET THE NEXT TOKEN
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 2030                                        /* WAS IT THE END OF A LINE?
      IF(NOW.EQ.1) PRINT *,' <  ENTER PARAMETERS  >'
      CALL RDLINE                                                        /* GET ANOTHER LINE
      NTOKES=0
      GO TO 2020
 2030 IF(TOKEN(1:NCHARS).NE.'END'.OR.NCHARS.NE.3) GO TO 150
      RETURN                                                             /*  FINISHED ALL OF THE PARAMETERS!!!
      END
