      INTEGER FUNCTION or( i, j )
      INTEGER*4 i, j
c     Assume LONG integer - this won't work on integer*2
c     SYS V uses IOR for INTEGER*4 and IIOR for INTEGER*2
      or = IAND( i, j )
      RETURN
      END
