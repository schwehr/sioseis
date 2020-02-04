      SUBROUTINE UFILED(BUF,LBUF)
C                                PROCESS UFILTR
C                                ------- ------
C
C  DOCUMENT DATE: 11 NOVEMBER 1982                                          44
C
C     PROCESS UFILTR APPLIES USER GIVEN FILTER POINTS TO EVERY SEISMIC TRACE.
C  THE FILTERING IS DONE AS CONVOLUTION IN THE TIME DOMAIN. THE USER MAY SPECIFY
C  THE TIME ORIGIN OF THE FILTER BY SPECIFYING THE AMOUNT OF SHIFT TO APPLY AFTER
C  CONVOLUTION.  ZEROES ARE INSERTED BEFORE AND AFTER THE DATA TRACE SO THAT
C  FULL CONVOLUTION IS PERFORMED WITHOUT PROGRAM BUFFER EDGE EFFECTS.
C     ONE USE OF PROCESS UFILTR IS IN APPLYING A SINGLE COMPENSATION FILTER TO
C  ALL DATA.  ANOTHER USE IS IN APPLYING A HILBERT TRANSFORM OR FIRST
C  DERIVATIVE FILTER (1,-1).
C
C     EACH PARAMETER LIST MUST BE TERMINATED WITH THE WORD END.  THE ENTIRE SET
C  OF UFILTR PARAMETERS MUST BE TERMINATED BY THE WORD END.
C
C  THE PARAMETER DICTIONARY
C  --- --------- ----------
C  FNO    - THE FIRST SHOT (OR RP) TO APPLY THE FILTER TO.  SHOT (RP) NUMBERS
C           MUST INCREASE MONOTONICALLY.
C  LNO    - THE LAST SHOT (RP) NUMBER TO APPLY THE FILTER TO.  LNO MUST BE
C           LARGER THAN FNO IN EACH LIST AND MUST INCREASE LIST TO LIST.
C  NSHIFT - THE NUMBER OF SAMPLES TO SHIFT THE DATA AFTER CONVOLUTION.  A
C           NEGATIVE SHIFT MOVES THE DATA TO THE LEFT AND NSHIFT DATA VALUES
C           ARE DROPPED FROM THE BEGINNING.  A POSITIVE SHIFT MOVES THE DATA
C           TO THE RIGHT AND NSHIFT ZEROES ARE INSERTED AT THE BEGINNING OF
C           DATA.  A ZERO PHASE FILTER SHOULD HAVE A SHIFT OF -NFPTS/2.
C           PRESET=0.  E.G. NSHIFT -15
C  FILPTS - A LIST OF FILTER POINTS, EACH VALUE SEPARATED FROM THE OTHER BY A
C           BLANK.  THE POINTS MAY BE GIVEN ON AS MANY LINES AS NECESSARY.  WHEN
C           THE VALUES ARE GIVEN IN AN E FORMAT, POSITIVE EXPONENTS MUST HAVE
C           A + RATHER THAN A BLANK.
C           REQUIRED. E.G. FILPTS .5 .5
C  END    - TERMINATES EACH PARAMETER LIST.
C
C
C  WRITTEN AND COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, NOVEMBER 1982
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
C
c  MODS:
c  May 1996 - Change fno/lno presets to 0 and 999999999 since spatial
c             variation is not done, yet all shots should be filtered
c             if fno/lno are not given.
c
C       2)  LNO
C       3)  NFPTS
C       4)  NSHIFT
C       5)  LPRINT
C       6)  NFPTS FILTER POINTS    ****  THIS IS A VALIABLE FROM LIST TO LIST!!!
C
C
C  ARGUMENTS:
C  BUF    - A SCRATCH ARRAY AT LEAST 500 32 BIT WORDS LONG.
C  LBUF   - THE SAME ARRAY BUT THE 32 BIT INTEGER EQUIVALENT.  NEEDED
C           BECAUSE PRIME FORTRAN DOESN'T ALLOW EQUIVALENCING OF ARGUMENTS.
C
      PARAMETER (NPARS=6)                                               ! /* THE NUMBER OF USER PARAMETERS
      PARAMETER (MULTIV=6)                                              ! /* THE PARAMETER NUMBER OF THE FIRST MULTI-VALUED PARAMETER
      COMMON /UFILTR/ MUNIT,NLISTS
      DIMENSION BUF(111),LBUF(111)
      CHARACTER*6 NAMES(NPARS)
      CHARACTER*1 TYPES(NPARS)
      DIMENSION LENGTH(NPARS)
      CHARACTER*80 TOKEN
      DIMENSION VALS(NPARS),LVALS(NPARS)
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      COMMON /sioln1/ cbuf
      COMMON /sioln2/ idum1, idum2, iprint
      CHARACTER*100 cbuf
      INTEGER FNO
C
C
      EQUIVALENCE (FNO,LVALS(1)),
     2            (LNO,LVALS(2)),
     3            (LPRINT,LVALS(3)),
     4            (NFPTS,LVALS(4)),
     5            (NSHIFT,LVALS(5)),
     6            (FILPTS,VALS(6))
      DATA NAMES/'FNO   ','LNO   ','LPRINT','NFPTS ','NSHIFT',
     *           'FILPTS'/
      DATA TYPES/'L','L','L','L','L','F'/
      DATA LENGTH/3,3,6,5,6,6/
C****
C****      SET THE PRESETS
C****
      FNO=0
      LNO=999999999
      NFPTS=0
      NSHIFT=0
      LPRINT=0
      IADDWB=0
      LLNO = -1
      NLISTS=0
      NS=0
      isavep = iprint
C****
C****   GET A PARAMETER FILE
C****
      CALL GETFIL(1,MUNIT,TOKEN,ISTAT)
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
  191 FORMAT(' ***  ERROR  *** UFILTR DOES NOT HAVE A PARAMETER ',
     *  'NAMED ',A10)
      IERROR=IERROR+1
      GO TO 100
C****
C****    FOUND THE PARAMETER NAME, NOW FIND THE VALUE
C****
  200 NS=0
      iprint = isavep
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
      IF(TYPES(NPARAM).NE.'A') GO TO 240
      IF(NAMES(NPARAM).EQ.'ADDWB'.AND.TOKEN(1:NCHARS).EQ.'YES')
     *    IADDWB=1
      GO TO 100
  240 CONTINUE
      CALL DCODE(TOKEN,NCHARS,AREAL,ISTAT)                              ! /* TRY AND DECODE IT
      IF(ISTAT.EQ.2) GO TO 420                                          ! /* =2 MEANS IT IS A NUMERIC
      IERROR=IERROR+1                                                   ! /* DCODE PRINTED AN ERROR
      GO TO 100
  420 IF(TYPES(NPARAM).EQ.'L') GO TO 500
      IF(NPARAM.LT.MULTIV) GO TO 490                                    ! /*  IS IT A MULTIVALUED PARAMETER
      iprint = 0
      NS=NS+1                                                           ! /*  THE TOKEN WAS A MULTI-VALUED PARAMETER
      nfpts = ns
      TVA=1.
      ITEMP=MULTIV
      BUF(NS+ITEMP-1)=AREAL
      GO TO 100
  490 VALS(NPARAM)=AREAL                                                ! /*  FLOATING POINT VALUES
      GO TO 100
  500 CONTINUE                                                          ! /*  32 BIT INTEGER VALUES
      LVALS(NPARAM)=AREAL
      GO TO 100
C****
C****   FINISHED A LIST, NOW DO THE ERROR CHECKING
C****
 1000 CONTINUE
      iprint = isavep
      IF(FNO.GT.LLNO) GO TO 1020
      PRINT 1010
 1010 FORMAT(' ***  ERROR  ***  SHOT AND RP NUMBERS MUST INCREASE.')
      IERROR=IERROR+1
 1020 IF(LNO.GE.FNO) GO TO 1030                                         ! /* DO THEY INCREASE IN THIS LIST
      PRINT 1010
      IERROR=IERROR+1
 1030 LLNO=LNO
C****
C****      WRITE THE PARAMETER LIST TO DISC
C****
      LBUF(1)=FNO
      LBUF(2)=LNO
      LBUF(3)=NFPTS
      LBUF(4)=NSHIFT
      LBUF(5)=LPRINT
      NWRDS=NFPTS+5
      IF(IAND(LPRINT,1).EQ.1)  PRINT 2010,(LBUF(I),I=1,5),
     *   (BUF(J),J=6,NWRDS)
 2010 FORMAT(' UFILTR PARAMS:',/,5(1X,I10),/,5(10(1X,F10.3),/))
      CALL WRDISC(MUNIT,BUF,NWRDS)
      NLISTS=NLISTS+1
      LLNO=LNO
      NS=0
      NFPTS=0
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
