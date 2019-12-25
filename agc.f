      SUBROUTINE AGC(BUFIN,BUFOUT,SCRBUF,SPEC)
C     AGC CALCULATES AND APPLIES AGC (AUTOMATIC GAIN CONTROL) TO AN ARRAY OF REAL
C VALUES (A SEISMIC TRACE).  AGC IS A TYPE OF SCALING SUCH THAT THE AVERAGE
C ABSOLUTE VALUE OF THE AMPLITUDES WITHIN A WINDOW WILL BE A CERTAIN LEVEL
C AFTER SCALING.
C     THIS AGC PROGRAM DIFFERS FROM OTHER AGCS IN THE WAY IT HANDLES ZERO (OR
C NEAR ZERO) INPUT AMPLITUDES.  IF THE WINDOW CONTAINS TOO MANY ZEROES, THE
C CALCULATED MULTIPLIER FOR THAT WINDOW IS IGNORED AND THE LAST ONE IS USED.
C E.G. IN THE CASE OF MUTES PRIOR TO P-BREAKS, THE FIRST MULTIPLIER WILL BE
C CALCULATED FROM SAY 75 PERCENT LIVE VALUES, THUS PREVENTING THE P-BREAKS FROM
C BEING OVERDRIVEN.
C     ANY TRACE WITH ALL AMPLITIDES BELOW A CERTAIN LEVEL (DEAD) WILL BE SET TO
C ZERO.
C
C   AGC VARIABLES:
C   ARGUMENTS:
C   BUFIN  - THE INPUT ARRAY (REAL)
C   BUFOUT -  THE OUTPUT ARRAY (REAL) - THE INPUT AND OUTPUT ARRAYS MAY BE THE
C             SAME.
C   SCRBUF  - A SCRATCH ARRAY (REAL) - MUST BE INDXET LONG
C    SPEC   - SPECIAL CASE SWITCH
C           <0, PRINT THE AVERAGE ABSOLUTE VALUE FOR EACH WINDOW
C           =0, DO NOTHING SPECIAL
C COMMON VARIABLE:
C   INDXST  - THE INDX OF THE FIRST SAMPLE IN BUFIN TO PROCESS.  BUFOUT WILL
C             HAVE UNTOUCHED DATA VALUES PRIOR TO INDXST.  THIS PARAMETER CAN BE
C             NON-ZERO IN ORDER TO AVOID SCALING THE WATER LAYER (SAY CPU TIME).
C             THIS SHOULD BE 1 IN MOST CASES.
C   INDXET - THE INDEX OF BUFIN OF THE LAST SAMPLE TO PROCESS.  THE NUMBER OF
C            SAMPLES IN THE ARRAY (WHEN INDXET IS 1).
C   OLEVEL - THE OUTPUT AMPLITUDE LEVEL.  THE AVERAGE ABSOLUTE VALUE.
C   CLIP   - THE CLIP LEVEL.  ANY AMPLITUDE (ABS VALUE) EXCEEDING THE CLIP
C            VALUE WILL BE SET TO CLIP.
C   NPTS   - THE NUMBER OF SAMPLES IN EACH WINDOW( THE WINDOW LENGTH IN SAMPLES)
C            REMEMBER THAT THE WINDOW IS SLIDE (ADVANCED) BY ONE SAMPLE
C   NDEAD  - THE NUMBER OF DEAD VALUES TO ALLOW IN A WINDOW IN ORDER TO GET A
C            NEW WINDOW MULTIPLIER.
C   DEAD   - A LEVEL BELOW WHICH AN AMPLITUDE IS CONSIDERED DEAD.
C   MIDPT  - THE MID POINT OF THE WINDOW TO RECEIVE THE MULTIPLIER FOR THE WINDOW
C            A FORWARD LOOKING WINDOW WOULD HAVE MIDPT=1
c   agcpct - The percent AGC, expressed as percent/100.  agcpct
c            multiplies the multiplier.  An agcpct < 1 has the
c            effect of lessening the AGC (makes it "softer").
C
C
C    PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, JANUARY 1979
c  mod 13 April 1991 by pch - add agcpct
c  mod June 96 by s.s - change the way agcpct works
c
      COMMON /AGCCOM/ INDXST,INDXET,OLEVEL,CLIP,NPTS,NDEAD,DEAD,MIDPT,
     &     agcpct
      DIMENSION BUFIN(1),BUFOUT(1),SCRBUF(1)
      LOGICAL FIRST
      double precision agctot, agcmean
      FIRST=.TRUE.
      NZEROS=0
      SUM=0.
      NDONE=0
C****
C****       PUT THE ABSOLUTE VALUES INTO SCRBUF
C****
      DO 100 I = 1, INDXET
      SCRBUF(I)=ABS(BUFIN(I))
  100 CONTINUE
c  S.S  percent agc change.  First, find the average absolute value of nonzero
c    samples for the entire trace
      if( agcpct .ne. 1.) then
        agctot = 0
        nnonzero = 0
        do 110 i=1,indxet
          if(scrbuf(i) .ne. 0.) then
            agctot = agctot + abs(scrbuf(i))
            nnonzero = nnonzero + 1
          endif
110     continue
        agcmean = agctot / nnonzero
c        write (*,*) 'agcmean ',agcmean,' agcpct',agcpct
      endif
C****
C****            FIND THE FIRST WINDOW WITH A GOOD MULTIPLIER
C****
      INDX=INDXST-1
      DO 190 I = 1, NPTS                                                !  DO THE FIRST WINDOW
         J = INDX + I
         IF( SCRBUF(J) .GT. DEAD ) THEN
             SUM = SUM + SCRBUF(J)
         ELSE
             NZEROS = NZEROS + 1
         ENDIF
  190 CONTINUE
  200 CONTINUE
      NDONE = NDONE + 1
      IF(NZEROS.LT.NDEAD) GO TO 300
      J=INDXST+NDONE-1
      K = J + NPTS
      IF(K.GT.INDXET) GO TO 250
      IF( SCRBUF(J).LE.DEAD) THEN
          NZEROS = NZEROS - 1
          IF( nzeros .LT. 0 ) nzeros = 0
      ELSE
          SUM = SUM - SCRBUF(J)
      ENDIF
      IF( SCRBUF(K) .LE. DEAD ) THEN
          NZEROS = NZEROS + 1
          IF( nzeros .GT. npts ) nzeros = npts
      ELSE
          SUM = SUM + SCRBUF(K)
      ENDIF
      GO TO 200
C****
C****           THE TRACE IS DEAD HERE
C****
  250 CONTINUE
      DO 260 I = 1, INDXET                                              ! MOVE THE INPUT WITHOUT CHANGING THE AMPLITUDES
  260 BUFOUT(I)=BUFIN(I)
      PRINT 270
  270 FORMAT(' ***  WARNING  ***  NOT ENOUGH NON-ZERO VALUES IN ORDER',
     *   ' TO AGC, NO AGC APPLIED.')
      RETURN
C****
C****
  300 CONTINUE
      TEMP=FLOAT(NPTS-NZEROS)                                           ! CHECK FOR NUMBER SIZE
      IF( TEMP .LE. 0. ) THEN
          AVE = CLIP
      ELSE
          AVE = SUM / TEMP
      ENDIF
      IF(AVE.EQ.0.) AVE=.00000001
      IF( SPEC .LT. 0 ) PRINT *,' ave=',ave,' ndone=',ndone
C****
C****         MULTIPLY THE CENTRE POINT OF THE WINDOW AND ADVANCE THE WINDOW
C****         ONE SAMPLE AND REPEAT
C****           USE THE FIRST MULTIPLIER FOR ALL POINTS LESS THAN THE FIRST MID
C****      POINT
C****
c S.S.  changing way pctagc works
c      xmult = olevel / ave * agcpct
c      xmult = olevel / ave / (2.-agcpct)
      IF( agcpct .EQ. 1. ) THEN
          xmult = olevel / ave
      ELSE
          xmult = olevel / (ave + ((1.-agcpct) * agcmean))
      ENDIF
      IF( FIRST ) THEN
          FIRST = .FALSE.
          N = INDXST + NDONE + MIDPT - 3
          DO 390 I = 1, N
             BUFOUT(I) = BUFIN(I) * XMULT
             IF( CLIP .NE. 0. ) THEN
                 IF( ABS(BUFOUT(I)) .GT. CLIP )
     *               BUFOUT(I) = SIGN(CLIP,BUFIN(I))
             ENDIF
  390     CONTINUE
      ENDIF
  400 CONTINUE
      I = INDXST + NDONE + MIDPT - 2
      BUFOUT(I) = BUFIN(I) * XMULT
      IF( CLIP .NE. 0.) THEN
          IF( ABS(BUFOUT(I)) .GT. CLIP )
     *        BUFOUT(I) = SIGN(CLIP,BUFIN(I))
      ENDIF
      IF( INDXST+NPTS+NDONE-1 .GT. INDXET ) GOTO 1000
      J=INDXST+NDONE-1
      K=J+NPTS
      IF( SCRBUF(J) .LE. DEAD ) THEN
          NZEROS = NZEROS - 1
          IF( nzeros .LT. 0 ) nzeros = 0
      ELSE
          SUM = SUM - SCRBUF(J)
      ENDIF
      IF( SCRBUF(K) .LE. DEAD ) THEN
          NZEROS = NZEROS + 1
          IF( nzeros .GT. npts ) nzeros = npts
      ELSE
          SUM = SUM + SCRBUF(K)
      ENDIF
      NDONE = NDONE + 1
      IF( NZEROS .LT. NDEAD ) GOTO 300
      GOTO 400
C****
C****            FINISHED FINDING XMULTIPLIERS, NOW FINISH THE REMAINDER OF THE
C****       LAST WINDOW
C****
 1000 CONTINUE
      J=INDXST+NDONE+MIDPT-1
      DO 1010 I=J,INDXET
      BUFOUT(I)=BUFIN(I)*XMULT
      IF(CLIP.EQ.0.) GO TO 1010
      IF(ABS(BUFOUT(I)).GT.CLIP) BUFOUT(I)=SIGN(CLIP,BUFIN(I))
 1010 CONTINUE
      RETURN
      END
