      SUBROUTINE NMONAP( XSQUAR, DELA, SI, NSAMPS, IV2AD, IANS, itype,
     &    ans )
C     NMONAP computes time shifts like nmo.  It does NOT applied the shifts.
c  Actually, this computes the an array of indices pointing to the input
c  array.
C  FAST NMO IS TX=X**2/2*V**2*T0 + T0
C  SLOW BUT ACCURATE NMO IS TX=SQRT(T0**2+X**2/V**2)
C  *** NOTE ***  THIS ROUTINE DOES NOT USE THE AP.
C  (NMO IS REALLY TX-T0, SO NMONAP IS NOT REALLY NMO)
C
C  ARGUMENTS:
C    XSQUAR - THE SQUARE OF THE RANGE (SHOT RECEIVER DISTANCE)
C    DELA   - THE FIRST T0 VALUE NEEDED.  THE DEEP WATER DELAY.
C    SI     - THE SAMPLE INTERVAL IN SECONDS.
C    NSAMPS - THE NUMBER OF TX VALUES TO COMPUTE. IANS MUST BE NSAMPS LONG.
C    IV2AD  - THE AP SIMULATOR ADDRESS OF THE SQUARE OF THE VELOCITIES.  EVERY
C             TIME SAMPLE MUST HAVE A VELOCITY. I.E. THERE MUST BE NSAMPS
C             VELOCITIES.  THE VELOCITY AT IV2AD MUST BE ASSOCIATED WITH TIME
C             DELAY, AT IV2AD+1 THE V FOR DELAY+SI, ETC.
C    IANS   - AN ARRAY TO RECEIVE THE INDEXES POINTING TO THE DATA ARRAY TX.
c    itype = 1 means NMO (tx=sqrt(t0**2 + x**2/v**2))
c          = 2 means DENMO or movein (t0 = sqrt(tx**2-x**2/v**2))
c          = 3 means SMO (Slant Move Out (t0 = tx - x/v))
c    ans - The array of TXs, the type real time TX time.
C
C  COPYRIGHTED BY PAUL HENKART, SEISMIC REFLECTION PROCESSORS, SAN DIEGO, 12 JAN 1983
C
c   mod 2 Mar 98 - change move in to use t0 velocities rather than tx.
c   mod 25 Jul 01 - movein with t0 bombed when the t0 data was before
c                   the delay.  i.e. delays were not handled quite right.
c   mod 17 Jul 02 - Add returning of the TX array
C
      COMMON /APMEM/A(32767)
      DIMENSION IANS(1), ans(1)
C
      DELAY = DELA
      CONST = SI/2.-DELAY
      IV = IV2AD-1
      X2 = XSQUAR                                                       ! It is faster not to have ARGUMENTS IN LOOPS (INDIRECT ADDRESSING)
      T0=DELAY-SI
      IF( itype .EQ. 1 ) THEN
C****
C****  SLOW NMO HERE
C****
          DO 100 I=1,NSAMPS
             T=T0+SI*FLOAT(I)                                           ! ADDING SI LEADS TO ROUND OFF ERRORS
             TEMP=SQRT(T*T+X2/A(IV+I))
             ans(i) = temp
             IANS(I)=IFIX((TEMP+CONST)/SI)+1                            ! ADD SI/2 TO ROUND OFF SAMPLE INTERVAL
  100     CONTINUE
      ELSEIF( itype .EQ. 2 ) THEN
c****
c****   Move In
c****   The trick here is that each output sample is a tx sample, but
c****   the velocities were given for t0, so find out what the moveout
c****   is, then subtract the move out from time of the velocity in
c****   order to find the velocity for the tx time.
c****
          DO 200 i = 1, nsamps
             t = t0 + si * FLOAT(i)
             temp =  t*t - x2 / a(iv+i)
c	 print *,' i=',i,' iv=',iv,' a()=',a(iv+i),' si=',si,' t=',t,
c     &   ' temp=',temp
             IF( temp .LE. 0. ) THEN
                 temp = t
             ELSE
                 temp = SQRT(temp)
             ENDIF
c             itemp = (t-temp-t0) / si
             itemp = NINT((t-temp) / si)
             IF( itemp .GE. i ) itemp = i-1
             jtemp = i-itemp
c             IF( jtemp .LT. 0 .OR. jtemp .GT. nsamps .OR.
c     &           a(iv+jtemp) .LE. 0 ) THEN
c	 print *,' i=',i,' it=',itemp,' t=',t,' temp=',temp,a(iv+i-itemp)
c                 STOP
c             ENDIF
             temp = t*t - x2 / a(iv+i-itemp)
             IF( temp .GT. 0 ) THEN
                 temp = SQRT(temp)
             ELSE
                 temp = 0.
             ENDIF
             ians(i) = IFIX((temp+const)/si) + 1
c**** The T0 data may be before the start of data, so use the first sample
             IF( ians(i) .LT. 1 ) ians(i) = 1
c	 	print *,' i=',i,' ians=',ians(i),' temp=',temp
c		print *,' temp=',temp,' const=',const,' a=',a(iv+i-itemp),
c     &     ' t=',t,' x2=',x2,' itemp=',itemp
  200     CONTINUE
      ELSEIF( itype .EQ. 3 ) THEN
c****
c****     Slant Move Out
          x = SQRT( x2 )
          DO 300 i = 1, nsamps
             t = t0 + si * FLOAT(i)
             temp = t + SQRT(x2/a(iv+i))
             ians(i) = IFIX((temp+const)/si) + 1
  300     CONTINUE
      ENDIF
      RETURN
      END
