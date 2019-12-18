      SUBROUTINE SDECED(BUF,LBUF)
C                        PROCESS DECON  (LINEAR PREDICTION DECONVOLUTION)
C                        ------- -----
C
C DOCUMENT DATE: 27 March 1989
C
C     DECON DESIGNS AND APPLIES A LEAST SQUARES PREDICTION ERROR FILTER.  LINEAR
C  PREDICTION DECONVOLUTION REDUCES PERIODIC EVENTS SUCH AS BUBBLE PULSES, RING,
C  OR EVEN LONG PERIOD MULTIPLES.
C    PROCEDURALLY, AN AUTOCORRELATION OF THE DESIGN WINDOW IS TAKEN AND AN
C  INVERSE FILTER IS DESIGNED SO THAT THE AUTOCORRELATION OF THE SAME WINDOW
C  AFTER DECON RESULTS IN A SPIKE FOLLOWED BY ZEROES.
C     TIME VARYING DECON IS PERFORMED BY APPLYING DIFFERENT FILTERS TO DIFFERENT
C  PARTS OF THE TRACE.  THE DIFFERENT PARTS OF THE TRACE ARE CALLED WINDOWS.
C  THE PORTION OF THE TRACE BETWEEN WINDOWS ARE MERGED BY RAMPING (LINEAR).
C  THE MERGE ZONE THUS CONTAINS DATA THAT HAS BEEN FILTERED BY DIFFERENT FILTERS
C  AND THEN ADDED TOGETHER AFTER BEING RAMPED.
C     E.G.
C               F1            F2            F3
C          ..........     ..........     ..........
C                    .   .          .   .
C                     . .            . .
C                      .              .
C                     . .            . .
C                    .   .          .   .
C     UP TO 5 WINDOWS MAY BE GIVEN, AND MAY BE SPATIALLY VARIED BY SHOT OR BY
C  HANGING THE WINDOWS ON THE WATER BOTTOM.
C     ALL PARAMETERS THAT REMAIN CONSTANT FOR A SET OF SHOTS (RPS) MAY BE
C  DESCRIBED IN A PARAMETER SET FNO TO LNO.  WINDOWS BETWEEN TWO PARAMETER
C  SEDTS ARE CALCULATED BY LINEARLY INTERPOLATING BETWEEN LNO OF ONE SET AND FNO
C  OF THE NEXT SET.
C     EACH PARAMETER LIST MUST BE TERMINATED WITH THE WORD END.  THE ENTIRE SET
C  OF SDECON PARAMETERS MUST BE TERMINATED BY THE WORD END.
C
C  THE PARAMETER DICTIONARY
C  --- --------- ----------
C  FNO    - THE FIRST SHOT (OR RP) TO APPLY THE DECON TO.  SHOT (RP) NUMBERS
C           MUST INCREASE MONOTONICALLY.
C           PRESET=1
C  LNO    - THE LAST SHOT (RP) NUMBER TO APPLY THE DECON TO.  LNO MUST BE
C           LARGER THAN FNO IN EACH LIST AND MUST INCREASE LIST TO LIST.
C           DEFAULT=FNO
C  SEDTS  - START-END TIME PAIRS DEFINING THE DESIGN WINDOWS.  TIMES ARE IN
C           SECONDS AND MAY BE NEGATIVE WHEN HANGING THE WINDOWS FROM THE WATER
C           BOTTOM.  A MAXIMUM OF 5 WINDOWS MAY BE GIVEN.
C           REQUIRED.
C  VEL    - THE VELOCITY TO USE TO 'MOVE-IN' EACH DESIGN WINDOW TIME.  MOVE-IN
C           IS USEFUL FOR DESCRIBING WINDOW TIMES THAT NEED TO VARY ACCORDING
C           TO THE SHOT-RECEIVER DISTANCE, AS IN FOLLOWING A REFLECTOR ON A
C           RECORD BEFORE NMO.  EACH DESIGN WINDOW TIME WILL BE DETERMINED
C           FROM THE EQUATION: T=SQRT(T0*T0+X*X/(VEL*VEL)), WHERE T0 IS THE
C           NORMAL INCIDENCE TWO WAY TRAVEL TIME, AND X IS THE SHOT TO
C           RECEIVER DISTANCE OF THE TRACE DESCRIBED VIA PROCESS GEOM.
C           PRESET=0.
C  FILLEN - THE LENGTH OF EACH FILTER IN SECONDS. UP TO 5 FILTER LENGTHS MAY BE
C           GIVEN.  THE FILTER LENGTH MUST BE SUFFICIENT TO INCLUDE THE PERIOD
C           BEING REMOVED.
C           PRESET=.160 .160. .160 .160 .160
C  PREWHI - THE PERCENTAGE PREWHITENING TO ADD BEFORE FILTER DESIGN.
C           A HIGH LEVEL OF PREWHITENING REDUCES THE EFFECTIVENESS OF THE
C           FILTER.  SOME LEVEL OF PREWHITENING IS NEEDED IN ORDER FOR THE
C           FILTER TO BE STABLE.  PREWHITENING IS LIKE PERFORMING A BANDPASS
C           FILTER BEFORE DECON.
C           PRESET=25.
C  ADDWB  - WHEN GIVEN A VALUE OF YES, THE WINDOWS GIVEN VIA SEDTS AND SEATS
C           WILL BE ADDED TO THE WATER BOTTOM TIME OF THE TRACE.  (WATER BOTTOM
C           TIMES MAY BE ENTERED VIA PROCESS WBT).
C           PRESET=NO
C  PDIST  - THE PREDICTION DISTANCE, IN SECONDS.  THE PREDICTION DISTANCE IS
C           THE TIME DELAY OF THE EVENT TO BE REMOVED.
C           PRESET=3*SAMPLE INTERVAL. E.G. PDIST .15 (FOR WATER BOTTOM MULTIPLE)
C  PADDWB - WHEN GIVEN A VALUE OF YES, THE WATER BOTTOM TIME WILL BE ADDED
C           TO THE VALUE OF PDIST ON EACH TRACE.
C           PRESET=NO.  E.G. PADDWB YES
c  DOUBLE - When given a value of yes, the correlations and convolution are
c           performed in DOUBLE PRECISION.  THE USE OF THIS PARAMETER WILL
c           INCREASE THE CPU TIME CONSIDERABLY, but might make the decon work
c           better especially if long windows are used.
c           Preset = no.    e.g.  yes
C  END    - TERMINATES EACH PARAMETER LIST.
C
C  WRITTEN AND COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, JUNE 1980
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
C
C
C  THE PARAMETER LIST ON DISC IS:
C  WORD 1)  FNO
C       2   LNO
C       3)  ADDWB
C       4)  LPRINT
C       5)  PREWHITENER (AS A DECIMAL FRACTION)
C       6)  PREDICTTION DISTANCE
C       7)  VELOCITY
C       8)  PDIST
C       9)  IPADDW
c      10)  double
c      11)  gap
c      12)  nzcross
C   13-22)  SEDTS
C   23-27)  FILLENS
C   28-37)  SEATS
C
C
C  ARGUMENTS:
C  BUF    - A SCRATCH ARRAY AT LEAST 60 32 BIT WORDS LONG.
C  LBUF   - THE SAME ARRAY BUT THE 32 BIT INTEGER EQUIVALENT.  NEEDED
C           BECAUSE PRIME FORTRAN DOESN'T ALLOW EQUIVALENCING OF ARGUMENTS.
C
c  mod  jan 97 (mwh)  Allow overlapping design windows but enforce a minimum
c                     0.1 sec between application windows.
c  mod 4 Feb 11 - add parameter GAP
c  mod 18 Feb 11 - add parameter NZCROSS
c
      PARAMETER (NPARS=15)                                               ! THE NUMBER OF USER PARAMETERS
      PARAMETER (MAX=10)                                                 ! THE MAXIMUM NUMBER OF SEDTS FILTER CAN HANDLE
      PARAMETER (NWRDS=37)                                               ! THE NUMBER OF WORDS IN EACH PARAMETER LIST
      PARAMETER ( idsgptr = 13 )                                        ! the index of dsg within the parameter list
      PARAMETER ( infptptr = idsgptr+MAX )                              ! the index of nfpts within the parameter list
      PARAMETER ( iappptr = infptptr+5 )                                ! the index of app within the parameter list

      CHARACTER*7 NAMES(NPARS)
      CHARACTER*1 TYPE(NPARS)
      DIMENSION LENGTH(NPARS)
      CHARACTER*80 TOKEN
      DIMENSION VALS(NPARS),LVALS(NPARS)
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      PARAMETER (MULTI=13)                                               ! THE PARAMETER NUMBER OF THE FIRST MULTI-VALUED PARAMETER
      COMMON /SDEC/ MUNIT,NLISTS
      DIMENSION BUF(111),LBUF(111)
      INTEGER FNO
C
C
      EQUIVALENCE (FNO,LVALS(1)),
     2            (LNO,LVALS(2)),
     3            (ADDWB,LVALS(3)),
     4            (LPRINT,LVALS(4)),
     5            (PREWHI,VALS(5)),
     6            (PREDIC,VALS(6)),
     7            (VEL,VALS(7)),
     8            (PDIST,VALS(8)),
     9            (PADDWB,LVALS(9)),
     *            (double,lvals(10)),
     1            (gap,vals(11)),
     2            (nzcross,lvals(12)),
     2            (SEDTS,VALS(13)),
     3            (FILLEN,VALS(14)),
     4            (SEATS,VALS(15))
      DATA NAMES/'FNO   ','LNO   ','ADDWB ','LPRINT','PREWHI',
     *           'PREDIC','VEL   ','PDIST ','PADDWB','DOUBLE',
     *           'GAP  ','NZCROSS',
     *   'SEDTS ','FILLEN','SEATS '/
      DATA LENGTH/3,3,5,6,6,6,3,5,6,6,3,7,
     &            5,6,5/
      DATA TYPE/'L','L','A','L',4*'F',2*'A','F','L',
     &          3*'F'/
C****
C****      SET THE PRESETS
C****
      FNO=1
      LNO=32768
      LPRINT=0
      PREWHI=25.
      PREDIC=3.
      PDIST=-1.
      VEL=0.
      double = 0.
      gap = 0.
      nzcross = 0
      DO 10 I=1,50
   10 BUF(I)=0.
      BUF(11)=-1.                                                       ! MAKE SURE AT LEAST ONE SEDTS IS GIVEN
      DO I= infptptr, infptptr+4
         BUF(I)=.160
      ENDDO
C****
C*****    GET A PARAMETER FILE
C****
      CALL GETFIL(1,MUNIT,token,ISTAT)
C****
C****   THE CURRENT COMMAND LINE IN THE SYSTEM BUFFER MAY HAVE THE PARAMETERS.
C****   GET A PARAMETER LIST FROM THE USER.
C****
      IADDWB=0
      IPADDW=0
      LLNO=0
      NS=0
      NLISTS=0
      NTOKES=1
  100 CONTINUE
      CALL GETOKE(TOKEN,NCHARS)                                          ! GET A TOKEN FROM THE USER PARAMETER LINE
      CALL UPCASE(TOKEN,NCHARS)                                         ! CONVERT THE TOKEN TO UPPERCASE
      IF(NCHARS.GT.0) GOTO 150
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
      IF(NS.NE.0) GO TO 230
      PRINT 191, TOKEN(1:NCHARS)
  191 FORMAT(' ***  ERROR  *** DECON DOES NOT HAVE A PARAMETER ',
     *  'NAMED ',A10)
      IERROR=IERROR+1
      GO TO 100
C****
C****    FOUND THE PARAMETER NAME, NOW FIND THE VALUE
C****
  200 CONTINUE
      NS=0
      NPARAM=IPARAM
  210 CONTINUE                                                           !  NOW FIND THE VALUE
      CALL GETOKE(TOKEN,NCHARS)
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 230                                         ! END OF LINE?
      IF(NOW.EQ.1) PRINT 140                                            ! THIS ALLOWS A PARAMETER TO BE ON A DIFFERENT LINE FROM THE NAME
      CALL RDLINE                                                        ! GET ANOTHER LINE
      NTOKES=0
      GO TO 210
  230 CONTINUE
      IF( TYPE(NPARAM) .EQ. 'A') THEN
          CALL upcase(token,nchars)
          IF( names(nparam) .EQ. 'DOUBLE' .AND.
     *        token(1:nchars) .EQ. 'YES' ) double = 1.

          IF(NAMES(NPARAM).EQ.'ADDWB'.AND.TOKEN(1:NCHARS).EQ.'YES')
     *        IADDWB=1
          IF(NAMES(NPARAM).EQ.'PADDWB'.AND.TOKEN(1:NCHARS).EQ.'YES')
     *       IPADDW=1
          GOTO 100
      ENDIF
  240 CONTINUE
      CALL DCODE(TOKEN,NCHARS,AREAL,ISTAT)                              ! TRY AND DECODE IT
      IF(ISTAT.EQ.2) GO TO 420                                          ! =2 MEANS IT IS A NUMERIC
      IERROR=IERROR+1                                                   ! DCODE PRINTED AN ERROR
      GO TO 100
  420 IF(TYPE(NPARAM).EQ.'L') GO TO 500
      IF(NPARAM.LT.MULTI) GO TO 490                                     !  IS IT A MULTIVALUED PARAMETER
      NS=NS+1                                                           !  THE TOKEN WAS A MULTI-VALUED PARAMETER
      ITEMP = IDSGPTR - 1                                                        ! IS IT SEDTS?
      IF(NAMES(NPARAM).EQ.'FILLEN') ITEMP = infptptr - 1
      IF(NAMES(NPARAM).EQ.'SEATS ') ITEMP = iappptr - 1
      BUF(ITEMP+ns)=AREAL
      GO TO 100
  490 VALS(NPARAM)=AREAL                                                 !  FLOATING POINT VALUES
      GO TO 100
  500 CONTINUE                                                          !  32 BIT INTEGER VALUES
      LVALS(NPARAM)=AREAL
      GO TO 100
C****
C****   FINISHED A LIST, NOW DO THE ERROR AND VALIDITY CHECKS
C****
 1000 CONTINUE                                                           ! MAKE SURE ALL SHOT & RP NUMBERS INCREASE
      IF( FLOAT(NINT(predic)) .NE. predic ) THEN
          PRINT *,' ***  WARNING  ***  PREDIC is the number of samples.'
          iwarn = iwarn + 1
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
      IF( BUF(11) .EQ. -100. ) THEN
          PRINT *,
     *    ' ***  ERROR  ***  AT LEAST ONE DESIGN WINDOW MUST BE GIVEN.'
          IERROR=IERROR+1
          GOTO 1200
      ENDIF
      DO I = idsgptr, idsgptr + MAX - 1                                 ! ALLOW NEGATIVE TIMES FOR HANGING FROM WATER BOTTOM
         IF( ABS(BUF(I)) .GT. 20.) THEN
             PRINT *,' ***  WARNING  *** WINDOW TIME OF ',buf(i),
     *         ' MAY BE WRONG. (NOT FATAL)'
             IWARN = IWARN + 1
         ENDIF
      ENDDO
      DO i = idsgptr+2, idsgptr + MAX - 1, 2                                 ! ALLOW NEGATIVE TIMES FOR HANGING FROM WATER BOTTOM
         IF( buf(i) .NE. 0. .AND. BUF(I) .LT. BUF(I-1) + .1 ) THEN
                      PRINT *,
     *    ' ***  WARNING  *** WINDOWS SHOULD BE AT LEAST .1 SEC APART.'
                      IWARN =IWARN+1
         ENDIF
      ENDDO
      DO i = iappptr, iappptr+9, 2
         IF( buf(i) .NE. 0. ) THEN
             IF( buf(i+1) .LT. buf(i) ) THEN
                 PRINT *,' ***  ERROR  ***  Incorrect SEDTS of ',
     &               buf(i), buf(i+1)
                 ierror = ierror + 1
             ENDIF
             IF( i .GT. 27 .AND. buf(i) .LT. buf(i-1) + .1 ) THEN
                 PRINT *,' ***  ERROR  ***  DECON application windows ',
     &             'must be at .1 sec. apart.'
                 ierror = ierror + 1
             ENDIF
         ENDIF
      ENDDO
 1200 CONTINUE                                                          ! NOW CHECK THE FILTER LENGTHS
      DO  I= infptptr, infptptr+4
          IF( BUF(I) .LE. 0. ) THEN
              PRINT *, ' ***  ERROR  ***  ILLEGAL FILTER LENGTH OF ',
     *            buf(i)
              IERROR = IERROR+1
          ENDIF
          IF( BUF(I) .GT. .500 ) THEN
              PRINT *,' ***  WARNING  ***  A FILTER LENGTH OF ',
     *                buf(i), 'IS UNUSUALLY LARGE.'
              IWARN = IWARN+1
          ENDIF
      ENDDO
C****
C****      WRITE THE PARAMETER LIST TO DISC
C****
      IF(NS.LE.MAX) GO TO 1360
      ITEMP=MAX
      PRINT 1350,ITEMP
 1350 FORMAT(' ***  ERROR  ***  DECON CAN HANDLE ONLY ',I3,' SEDTS.')
      IERROR=IERROR+1
 1360 CONTINUE
      LBUF(1)=FNO
      LBUF(2)=LNO
      LBUF(3)=IADDWB
      LBUF(4)=LPRINT
      BUF(5)=PREWHI/100.                                                ! CONVERT FROM PERCENTAGE TO A DECIMAL FRACTION
      BUF(6)=PREDIC
      BUF(7)=VEL
      BUF(8)=PDIST
      LBUF(9)=IPADDW
      buf(10) = double
      buf(11) = gap
      lbuf(12) = nzcross
      IF( IAND(LPRINT,1) .EQ. 1 )  THEN
          PRINT *,(LBUF(I),I=1,4), (BUF(J),J=5,8),LBUF(9),
     &          buf(10),buf(11),lbuf(12)
          PRINT *,(BUF(K),K=13,nwrds)
      ENDIF
      CALL WRDISC(MUNIT,BUF,NWRDS)
      NLISTS=NLISTS+1
      LLNO=LNO
      NS=0
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
      RETURN                                                             !  FINISHED ALL OF THE PARAMETERS!!!
      END
