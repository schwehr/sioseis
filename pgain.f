      SUBROUTINE pgain( t, b, dt, nsamps, stime, etime, timmax, alpha )
c    Computes and applies a unique gain correction designed by M.W. Lee
c  of the U.S.G.S. Branch of Petroleum Geology, Denver (303)-236-5753
c     The gain = (time*1000.)**alpha
c   modified by pch to use stime in the calculation of the gain.
c   (it previously assumed both the gain function and the first sample
c   started at time zero, but then only applied the gain from stime
c   to etime)
c
      DIMENSION t(1), b(1)
      LOGICAL virgin
      SAVE virgin
      DATA virgin/.TRUE./
c  t : trace buffer nsamps long
c  b : scratch buffer
c  dt : sample interval in msec
c  nsamps : number of samples
c  stime : start time of trace in msec
c  etime : ending time to apply gain function
c  timmax : maximum time expected in msec
c  alpha : exponent to the time
c
      IF( virgin ) THEN
          virgin = .FALSE.
          jsamp = IFIX(timmax / dt) + 1
          isamp = IFIX(etime / dt) + 1
          DO 100 k = 1, isamp
             time = stime + (k-1) * dt
             b(k) = time ** alpha
  100     CONTINUE
          IF( isamp .LT. jsamp ) THEN
              DO 110 k = isamp + 1, jsamp
  110         b(k) = b(isamp)
          ENDIF
      ENDIF
c
c Apply gain function to the trace
c
      DO 200 k = 1, nsamps
  200 t(k) = t(k) * b( k)
      RETURN
      END

