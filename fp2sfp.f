      SUBROUTINE fp2sfp( fpbuf, n, jbuf )
c     fp2sfp converts internal floating point (REAL) to "short floating 
c  point".  Short floating point is a 16 bit word, the sign bit,
c  followed by 11 bits of 2's complement mantissa, followed by 4 bits
c  of exponent expressed as 2**(15-exp).
c     This format was first brought to my attention by UTIG.
c     The companion to this routine is sfp2fp.f
c
c  ARGUMENTS:
c  fpbuf - the input array of REAL
c  n     - The number of data samples to convert.
c  jbuf  - The output REAL array.  jbuf may be the same array as
c          fpbuf.   INTEGER*2
c
c  COPYRIGHT (C) Paul Henkart, Seismic Reflection Processors, 24 June 1990
c  ALL RIGHTS RESERVED.
c
      INTEGER n
      INTEGER RSHIFT
      INTEGER*2 jbuf(n)
      REAL fpbuf(n)
c
      DO 100 i = 1, n
         itemp = fpbuf(i)
         jsign = 0
         IF( itemp .LT. 0 ) THEN
             jsign = 1                                                  ! the sign bit
             itemp = -itemp
         ENDIF
         DO 50 j = 1, 16
            iexp = j
            IF( itemp .LT. 2048 ) THEN
                mant = itemp
                IF( jsign .NE. 0 ) mant = -mant
                mant = LSHIFT(mant,4)
                jbuf(i) = IOR(mant,16-iexp)
                GOTO 100
            ENDIF
            itemp = RSHIFT(itemp,1)
   50    CONTINUE
         PRINT *,' ***  WARNING  ***  Value',temp,' exceeds the range ',
     &           'for 16 bit gain words.'
  100 CONTINUE
      RETURN
      END
