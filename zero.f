      SUBROUTINE ZERO(LX,X)
      DIMENSION X(LX)
      IF( LX .LE. 0 ) RETURN
      DO I = 1, LX
         X(I) = 0.
      ENDDO
      RETURN
      END
