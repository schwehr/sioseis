      SUBROUTINE T2DINT(BUFIN,NSAMPS,ST,SIT,BUFOUT,MSAMPS,SD,SID,SCRBUF,
     *  vtp, nvtp )
C     T2DINT CONVERTS A TIME TRACE (BUFIN) INTO A DEPTH TRACE (BUFOUT) USING
C  INTERVAL VELOCITIES.  The output trace is uniformly sampled by sid.
c
C  ARGUMENTS:
C   BUFIN  - THE ARRAY OF TIME DOMAIN DATA TO BE TRANSFORMED.
C   NSAMPS - THE INTEGER NUMBER OF SAMPLES IN ARRAY BUFIN.  THE LENGTH OF THE
C            THE INPUT DATA.
C   ST     - THE START TIME OF BUFIN.  THE TIME IN SECONDS ASSOCIATED WITH THE
C            FIRST INPUT SAMPLE, BUFIN(1).
C   SIT    - SAMPLE INTERVAL IN TIME.  THE TIME UNITS, IN SECONDS, BETWEEN
C            SUCCESSIVE SAMPLES IN BUFIN.
C   BUFOUT - AN ARRAY TO RECEIVED THE TRANSFORMED TRACE.  THE DEPTH DOMAIN TRACE
C            THE OUTPUT ARRAY.  MUST BE DIFFERENT FROM BUFIN.
C   MSAMPS - THE NUMBER OF OUTPUT SAMPLES.  T2D WILL ONLY STORE MSAMPS SAMPLES
C            IN THE OUTPUT ARRAY.
C   SD     - START DEPTH.  THE DEPTH OF THE FIRST OUTPUT SAMPLE (BUFOUT(1)).
C   SID    - SAMPLE INTERVAL IN THE DEPTH DOMAIN.  THE  DISTANCE BETWEEN
C            SUCCESSIVE OUTPUT SAMPLES.
C   SCRBUF - A SCRATCH ARRAY OF LENGTH NSAMPS.
C   vtp    - The array of interval velocity-time pairs to use in conversion.
c            The first interval contains all times up to the first vtp time.
c   nvtp   - The number of words in the vtp array.
C
c   (C) ???????
C      PAUL HENKART, October 1990
C
      DIMENSION BUFIN(1),BUFOUT(1),SCRBUF(1),vtp(1)
C
c    METHOD:
C    1)  CALCULATE A DEPTH FOR EVERY TIME SAMPLE.
c    2)  Work through the output depths one-by-one and look for the
c        first input depth greater or equal to it.
C
c****
c****  fill scrbuf with the depth of every output sample
c****
      j = 1
      DO 100 I = 1, NSAMPS
         T = ST+SIT*FLOAT(I-1)
   10    IF( t .LE. vtp(j+1) ) THEN
             IF( j .EQ. 1 ) THEN
                 scrbuf(i) = vtp(1) * t
                 depth = scrbuf(i)
                 time = t
             ELSE
                 scrbuf(i) = depth + (t - time) * vtp(j)
             ENDIF
         ELSE
             depth = scrbuf(i-1)
             time = st + sit * FLOAT(i-2)
             j = j + 2
             GOTO 10
         ENDIF
c          print *,' i=',i,' depth=',scrbuf(i),' j=',j,' vtp=',vtp(j),
c     *       ' time=',time,' t=',t
  100 CONTINUE
C
C    FIND THE CLOSEST DEPTH IN THE CONVERTED DEPTH ARRAY FOR EVERY
C    OUTPUT DEPTH SAMPLE.  THE OUTPUT DEPTH ARRAY IS A UNIFORMLY SAMPLED
C    ARRAY BUT THE CONVERTED (TIME) ARRAY IS NOT.  USE WHATEVER CONVERTED
C    ARRAY POINT THAT IS CLOSEST, THUS ALLOWING THE SAME POINT TO APPEAR
C    IN THE OUTPUT ARRAY OR SKIPPING SOME.
C
      INDXT=1                                                           ! THE INDEX TO THE CONVERTED TIME ARRAY
      INDXD=0                                                           ! THE INDEX TO THE OUTPUT DEPTH ARRAY
C
  110 INDXD=INDXD+1
      IF(INDXD.GT.MSAMPS) RETURN
      DEPTH=SD+(INDXD-1)*SID                                            ! CALCULATE THE NEXT DEPTH VALUE
  120 IF( DEPTH .LT. SCRBUF(INDXT) ) THEN
          IF( DEPTH .LT. SCRBUF(INDXT-1) ) THEN
c****         the depth wanted is before the one we're at, so back up
              INDXT=INDXT-1
              IF(INDXT.GT.0) GO TO 120
              BUFOUT(INDXD)=0.                                                  ! THE OUTPUT IS BEFORE THE INPUT!
              INDXT=1
          ELSE
c****         the depth is between this one and the prior one, use the prior one
              BUFOUT(INDXD)=BUFIN(INDXT-1)
          ENDIF
c         print *,' 130 dep=',depth,' indxd=',indxd,' indxt=',indxt,
c     &        ' scr=',scrbuf(indxt)
          GO TO 110
       ENDIF
c****
c**** if the depth is after this one, go to the next one
      IF( DEPTH .EQ. SCRBUF(INDXT) ) THEN
c***      the depth is the same as this one, use it!
          BUFOUT(INDXD)=BUFIN(INDXT)
c          print *,' 200 dep=',depth,' indxd=',indxd,' indxt=',indxt,
c     &        ' scr=',scrbuf(indxt),' buf=',bufout(indxd)
          GO TO 110
      ENDIF
c****
c****  set up to try the next time sample
  300 INDXT=INDXT+1
      IF(INDXT.LE.NSAMPS) GO TO 120
      DO 310 I=INDXD,MSAMPS
  310 BUFOUT(I)=0.
      RETURN
      END
