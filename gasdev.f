      FUNCTION gasdev( idum )
c     Returns a normally distributed deviate with zero mean and unit
c variance, using routine RAN1(idum) as the source of uniform deviates.
c     From "Numerical Recipes" by Press, Flannery, Teulolsky, Vetterling
c
      DATA iset/0/
      SAVE iset, gset
c
      IF( iset .EQ. 0 ) THEN                                            ! we don't have an extra deviate handy, so pick two
    1     v1 = 2. * ran1(idum) - 1.                                     ! uniform numbers in the square extending from
          v2 = 2. * ran1(idum) - 1.                                     ! -1 to +1 in each direction.
          r = v1*v1 + v2*v2                                             ! see if the are on the unit circle
          IF( r .GE. 1 ) GOTO 1                                         ! and if they are not, try again
          fac = SQRT( -2.*LOG(r)/r )                                    ! now make the Box-Muller transformation
          gset = v1 * fac                                               ! to get two normal deviates.  Return one
          gasdev = v2 * fac                                             ! and save the other for the next time.
          iset = 1                                                      ! set the flag
      ELSE                                                              ! we have an extra deviate handy,
          gasdev = gset                                                 ! so return it
          iset = 0                                                      ! and unset the flag
      ENDIF
      RETURN
      END
