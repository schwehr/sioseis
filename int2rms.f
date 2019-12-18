      SUBROUTINE int2rms( vtpint, vtp, n )
c   int2rms converts interval velocity two-way-travel time pairs to
c rms velocity two-way-trace time pairs using Dix's formula.
c
c   Vrms(n) = SQRT( (IV(n)**2(T(n)-T(n-1) + Vrms(n-1)**2*T(n-1))  / T(n) )
c
c  ARGUMENTS:
c  vtpint - The input interval velocity (two way) time pairs.
c  vtp    - The array to receive the output average velocity
c           (two way) time pairs.
c  n      - the number of elements in the vtpint and vtp arrays.
c
      DIMENSION vtpint(n), vtp(n)
c
      vtp(1) = vtpint(1)
      vtp(2) = vtpint(2)
      IF( n .EQ. 2 ) RETURN
      DO j = 3, n, 2
         temp = vtp(j-2) * vtp(j-2) * vtp(j-1)
     &        + vtpint(j) * vtpint(j) * (vtpint(j+1)-vtpint(j-1))
         vtp(j) = SQRT( temp / vtpint(j+1) )
         vtp(j+1) = vtpint(j+1)
      ENDDO
      RETURN
      END

