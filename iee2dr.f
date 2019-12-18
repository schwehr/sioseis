      SUBROUTINE iee2dr( inbuf, n, iobuf)
c     iee2dr converts IEEE floating point (REAL*4) to DEC floating point
c  (REAL*4).
c
c  ARGUMENTS:
c  inbuf  - The input array of IEEE reals to be converted. 
c  n      - The number of reals to convert. (INTEGER)
c  iobuf - The output array of DEC reals.  May be the same array as inbuf.
c
c   Do a 16 bit byte swap on the upper and lower 16 bit words after
c  calling iee2dr on non DEC VAX machines.
c
c   Both IEEE and DEC use the hidden bit and both have an 8 bit exponent
c  and 23 bit mantissa.  Dec uses an exponent bias of 128 while IEEE 
c  uses 127.  DEC also normalizes so that the decimal point is before
c  the leading bit while IEEE puts the decimal after the leading bit.
c
c  Copyright (C) Seismic Reflection Processors, Solana Beach, CA 92075
c  All rights reserved.
c
      INTEGER RSHIFT
      DIMENSION inbuf(n), iobuf(n)
      INTEGER inbuf, n, iobuf
      INTEGER isign, mant, iexp, i, itemp
c
      DO 1000 i = 1, n
         IF( inbuf(i) .EQ. 0 ) THEN
             iobuf(i) = 0
             GOTO 1000
         ENDIF
c   count bits from right to left, starting with 0
c         mant = IAND( inbuf(i), 16#007FFFFF )  { the mantissa is bits 0-22
         mant = IAND( inbuf(i), 8388607 )
         iexp = RSHIFT( inbuf(i),23 )
         iexp = IAND(iexp,255) + 2
         iexp = LSHIFT(iexp,23)
         itemp = RSHIFT( inbuf(i), 31 )
         isign = LSHIFT( itemp, 31 )
         iobuf(i) = IOR( iexp, mant )
         iobuf(i) = IOR( iobuf(i), isign )
c
 1000 CONTINUE
      RETURN
      END
