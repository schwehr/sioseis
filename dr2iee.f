      SUBROUTINE dr2iee( inbuf, n, iobuf)
c     dr2iee converts DEC floating point (REAL*4) to IEEE floating point
c  (REAL*4).
c
c  ARGUMENTS:
c  inbuf  - The input array of DEC reals to be converted.
c  n      - The number of reals to convert.
c  iobuf - The output array of IEEE reals.  May be the same array as inbuf.
c
c  Copyright (C) The Regents of the University of California, August 1988
c  Paul Henkart, Scripps Institution of Oceanography, La Jolla, Ca. 92093
c  All rights reserved.
c
      DIMENSION inbuf(n), iobuf(n)
      INTEGER rshift
c
      DO 1000 i = 1, n
c   count bits from right to left, starting with 0
c         mant = IAND( inbuf(i), 16#007FFFFF )  { the mantissa is bits 0-22
         IF( inbuf(i) .EQ. 0 ) GOTO 1000
         mant = IAND( inbuf(i), 8388607 )
         isign = RSHIFT( inbuf(i),31 )
         isign = IAND(isign,1)
         isign = LSHIFT(isign,31)
         iexp = RSHIFT( inbuf(i),23 )
         iexp = IAND(iexp,255)
         iexp = iexp - 2
         iexp = LSHIFT(iexp,23)
         iobuf(i) = IOR( iexp, mant )
         iobuf(i) = IOR( iobuf(i),isign )
c
 1000 CONTINUE
      RETURN
      END
