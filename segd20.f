      SUBROUTINE segd20( ibuf, obuf, nsamps )
c     SEGD20 converts the SEG-D 20 bit exponent words to floating point.
c     The SEG-D 20 bit exponent (also called 20 bit binary) groups the
c  data into groups of 5.  There are 4 data words in each group of 5.
c  The exponents for each word are 4 bits big (no sign bit) and are a
c  positive power of 2.  These 4 exponents precede the 4 data words and
c  are in the first 16 bits.  The mantissas are signed 16 bit occupying
c  the next 4 words, representing values 2**-16.
c     This here subroutine is cute because each data word requires only
c  1 multiply to convert it (rather an exponentiation).  This is very
c  similar to subroutine IBM2FP.
c
c  ARGUMENTS:
c  ibuf   - The input array (of SEG-D 20 bit exponent data).  The array
c           must be alligned correctly - the first word must be the
c           4 exponents.
c  obuf   - The output array of REAL data.  Must not be the same as ibuf
c  nsamps - The number of data values to convert.
c     
c  Copyright (C) Paul Henkart, Seismic Reflection Processors, San Diego, Ca.
c mod 14 Aug 07 - g95 IAND requires arguments to be same type and kind.
c  ALL RIGHTS RESERVED.
c
      INTEGER*2 ibuf(1), rshift, i15
      REAL obuf(nsamps)
      DIMENSION exp(16), iexp(4)
      LOGICAL first
      SAVE exp, first
      DATA first/.TRUE./, i15/15/
c
      IF( first ) THEN
          exp(1) = 1. / 32768.
          DO 100 i = 2, 16
             exp(i) = exp(i-1) * 2.
  100     CONTINUE
      ENDIF
c
      i = 1
      j = 1
      DO 400 k = 1, nsamps, 4
         iexp(1) = IAND( rshift(ibuf(i),12), i15)
         iexp(2) = IAND( rshift(ibuf(i),8), i15)
         iexp(3) = IAND( rshift(ibuf(i),4), i15)
         iexp(4) = IAND( ibuf(i),i15 )
         i = i + 1
         DO 200 l = 1, 4
            temp = ibuf(i)                                               ! float the signed integer
            obuf(j) = temp * exp(iexp(l) + 1)
            i = i + 1
            j = j + 1
  200    CONTINUE
  400 CONTINUE
c
      RETURN
      END
