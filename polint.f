      SUBROUTINE polint( xa, ya, n, x, y, dy )
c     Polynomial interpolation.  "Numerical Recipes", page 82 (sect 3.1)
c  Given array XA and YA, each of length N, and given a value X, this
c  routine returns a value Y, and an error estimate DY.  If P(x) is the
c  polynomial of degree N-1 such that P(XA) = YA, i = 1,...N, then
c  the returned value Y = P(X)
c
      PARAMETER (nmax = 10)                                             ! the largest anticipated value of N
      DIMENSION xa(n), ya(n), c(nmax), d(nmax)
      ns = 1
      dif = ABS(x-xa(1))
      DO 11 i = 1, n
         dift = ABS(x-xa(i))
         IF( dift .LT. dif ) THEN
             ns = i
             dif = dift
         ENDIF
         c(i) = ya(i)
         d(i) = ya(i)
   11 CONTINUE
      y = ya(ns)
      ns = ns - 1
      DO 13 m = 1, n-1
         DO 12 i = 1, n-m
            ho = xa(i) - x
            hp = xa(i+m) - x
            w = c(i+1) - d(i)
            den = ho - hp
            IF( den .EQ. 0. ) RETURN                                    ! if two input xa's are identical
            den = w / den
            d(i) = hp * den
            c(i) = ho * den
   12    CONTINUE
         IF( 2*ns .LT. n-m ) THEN
             dy = c(ns+1)
         ELSE
             dy = d(ns)
             ns = ns - 1
         ENDIF
         y = y + dy
   13 CONTINUE
      RETURN
      END
