      SUBROUTINE SSEIS(ITYPE,SI,X,TVA,NTVA,OBUF,NOBUF,DELAY)
C    SSEIS GENERATES (IN MEMORY) A SYNTHETIC SEISMIC TRACE ACCORDING TO USER
C SUPPLIED RANGE, AND TIME-VELOCITY-AMPLITUDE INFORMATION.
C
C    PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, FEBRUARY 1979
C
C ARGUMENTS:
C        ITYPE  - THIS ARGUMENT IS NO LONGER USED.  (AS OF 2 MARCH 1982)
c               = 1, then tva (tva is triplets)
c               = 2, then ttva (tva is triplets)
C        SI     - SAMPLE INTERVAL IN SECONDS.
C        X      - RANGE TO THE TRACE - SHOT TO RECEIVER DISTANCE
C        TVA    - TIME-VELOCITY-AMPLITUDE TUPLES.  A SPIKE WILL BE PLACED ON THE
C                 TRACE ACCORDING TO ONE OF THE FORMULAE SPECIFIED ABOVE.  THE
C                 VELOCITIES MUST BE GIVEN IN UNITS OF SECONDS.  THE
C                 TIME SPECIFIED BY THE USER IS TO OR THE ZERO OFFSET TIME. E.G.
C                 ARRAY TVA HAS:  1.0,6000.,1.,2.0,10000.,1.5
C                 WILL CAUSE 2 EVENTS TO BE GENERATED.  ONE AT T0=1 SEC WITH
C                 VELOCITY=6000 AND AN AMPLITUDE OF 1.  ANOTHER EVENT WILL BE
C                 GENERATED AT 2.0 WITH VELOCITY=10000 AND AMPLITUDE =1.5
C                 THE EVENT IS A REFLECTED EVENT IF THE TIME SPECIFIED IS POSITIVE.
C                 A NEGATIVE TIME MEANS THAT THE EVENT IS A REFRACTED EVENT.
C                 (REFLECTED EVENT - TX=SQRT(T0**2+(X/V)**2).
C                 (REFRACTED EVENT - TX=ABS(T0)+X/V).
C        NTVA   - THE  NUMBER OF TUPLES IN THE TVA ARRAY.  TVA IS NTVA*3 LONG.
C        OBUF   - OUTPUT ARRAY TO RECEIVE THE SYNTHETIC TRACE.
C        NOBUF  - THE NUMBER OF SAMPLES IN OBUF.  IN TIME, THE SYNTHETIC IS
C                 SI*NOBUF WORDS LONG.
C
C   COPYRIGHTED BY:
C   PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, JULY 1980
C  ALL RIGHTS RESERVED BY THE AUTHOR.
C
c  mod 23 Oct 97 - Don't write past nobuf.
c  mod 15 June 07 - Allow the refraction by using 4 elements in tva,
c                   where the first element is 1 for reflection and 2
c                   is for refraction.
      DIMENSION TVA(1),OBUF(nobuf)
      SAVE th
      DATA th/10000./

      DO 100 I=1,NOBUF                                                  ! CLEAR THE OUTPUT BUFFER
  100 OBUF(I)=0.
C****
C****
      DO 290 I=1,NTVA
         IF( itype .EQ. 1 ) THEN
             J = (I-1)*3+1
             jtype = 1
         ELSEIF( itype .EQ. 2 ) THEN
             J = (I-1)*4+2
             jtype = tva((i-1)*4+1)
         ENDIF
         T0=ABS(TVA(J))
         IF( TVA(J) .LT. 0.) JTYPE = 2                                  ! REFRACTED ARRIVAL
c         IF( jtype .EQ. 2 ) jtype = 3
         V = TVA(J+1)
         A = TVA(J+2)
         IF( jtype .EQ. 1 ) THEN
             TX = SQRT(T0*T0+(X/V)*(X/V))
         ELSE
             TX = T0+ABS(X)/V
         ENDIF
         IBLK = (TX-DELAY)/SI+.5+1
         IF( iblk .LE. nobuf) OBUF(IBLK) = A
  290 CONTINUE
      IF( jtype .EQ. 3 ) THEN
          DO 300 i = 1, nobuf
             obuf(i) = th + FLOAT(i)
  300     CONTINUE
          th = th + 10000.
      ENDIF
      RETURN
      END
