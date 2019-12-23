      SUBROUTINE mp32ieee( inbuf, n, iobuf)
c     mp32ieee converts MP32 floating point to IEEE floating point.
c
c  ARGUMENTS:
c  inbuf  - The input array of MP32 floats to be converted.
c  n      - The number of reals to convert.
c  iobuf - The output array of IEEE reals.  MUST NOT BE THE SAME ARRAY
c                                           AS INBUF    ****************
c
c  Copyright (C) The Regents of the University of California, January 1992
c  Paul Henkart, Scripps Institution of Oceanography, La Jolla, Ca. 92093
c  All rights reserved.
c
      INTEGER*2 inbuf(n)
      INTEGER n, iobuf(n)
      INTEGER mant, iexp, isign, i
      INTEGER*2 rshift
c
      DO 1000 i = 1, n
         IF( inbuf(i) .EQ. 0 ) THEN
             iobuf(i) = 0
             GOTO 1000
         ENDIF
c   count bits from right to left, starting with 0
c         mant = AND( inbuf(i), 16#03FF )  { the mantissa is bits 0-9
         mant = IAND( inbuf(i), 1023 )
         mant = LLSHIFT( mant, 13 )
         iexp = RSHIFT( inbuf(i), 10 )
         iexp = IAND(iexp,31)
         iexp = iexp + 127
         iexp = LLSHIFT(iexp,23)
         iobuf(i) = IOR( iexp, mant )
         isign = IAND(inbuf(i),32768)
         isign = LLSHIFT(isign,16)
         iobuf(i) = IOR(iobuf(i),isign)
c
 1000 CONTINUE
      RETURN
      END
