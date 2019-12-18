      SUBROUTINE pc2dr( inbuf, n, iobuf)
c     pc2dr converts IEEE floating point (REAL*4), in PC byte order,
c   to DEC floating point (REAL*4).
c    *************  This works on VMS VAX ONLY *************
c
c  ARGUMENTS:
c  inbuf  - The input array of IEEE reals to be converted. 
c  n      - The number of reals to convert. (INTEGER)
c  iobuf - The output array of DEC reals.  May be the same array as inbuf.
c
c
c   Both IEEE and DEC use the hidden bit and both have an 8 bit exponent
c  and 23 bit mantissa.  Dec uses an exponent bias of 128 while IEEE 
c  uses 127.  DEC also normalizes so that the decimal point is before
c  the leading bit while IEEE puts the decimal after the leading bit.
c
c  Copyright (C) Seismic Reflection Processors, Solana Beach, CA 92075
c  All rights reserved.
c
      DIMENSION inbuf(n), iobuf(n)
      INTEGER inbuf, n, iobuf
      INTEGER isign, mant, iexp, i, itemp
      INTEGER*2 j,jj(2)
      EQUIVALENCE (itemp,jj)
c
      DO 1000 i = 1, n
         IF( inbuf(i) .EQ. 0 ) THEN
             iobuf(i) = 0
             GOTO 1000
         ENDIF
c   count bits from right to left, starting with 0
c         mant = AND( inbuf(i), 16#007FFFFF )  { the mantissa is bits 0-22
         mant = IAND( inbuf(i), 8388607 )
10	format(1x,z8)
         iexp = ISHFT( inbuf(i),-23 )    ! move to the right 23 bits
         iexp = IAND(iexp,255) + 2
         iexp = ISHFT(iexp,+23)          ! move back left 23 
         itemp = ISHFT( inbuf(i), -31 )
         isign = ISHFT( itemp, +31 )
         iobuf(i) = IOR( iexp, mant )
         iobuf(i) = IOR( iobuf(i), isign )
c        Do a 16 bit word swap on the upper and lower 16 bit words after
         itemp = iobuf(i)
         j = jj(1)
         jj(1) = jj(2)
         jj(2) = j
         iobuf(i) = itemp
c
 1000 CONTINUE
      RETURN
      END
