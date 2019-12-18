      SUBROUTINE ie2ibm( inbuf, n, iobuf)
c*****      THIS NO LONGER WORKS because    newman = LRSHIFT( mant, nshift )
c****  produces unpredictable results because LRSHIFT vacated bits are unpredictablek

c     ie2ibm converts IEEE floating point (REAL*4) to IBM floating
c point (REAL*4).
c
c  ARGUMENTS:
c  inbuf  - The input array of DEC reals to be converted.
c  n      - The number of reals to convert.
c  iobuf  - The output array of IBM reals.  May be the same array as inbuf.
c
c  COPYRIGHT (C) The Regents of the University of California, April 1987
c  Paul Henkart, Scripps Institution of Oceanography, La Jolla, Ca. 92093
c  All rights reserved.
c
c  mod 15 June 2006 - Change RSHIFT to LRSHIFT
c
      DIMENSION inbuf(n), iobuf(n)
      EQUIVALENCE (temp,itemp)
c
      DO 1000 i = 1, n
      IF( inbuf(i) .EQ. 0 ) THEN
          iobuf(i) = 0
          GOTO 1000
      ENDIF
c
      jtemp = inbuf(i)   
      mant = IAND( jtemp, 8388607) 
c	print *,' in=',jtemp,' mant=',mant
c the mantissa is bits 0-22, 8388607 is hex(007FFFFF)
      mant =IOR( mant, 8388608)  
c put the hidden bit on; 8388608 = hex(00800000)
      iexp = LRSHIFT( jtemp, 23 )
c	print *,' mant=',mant,iexp
c move the exponent to the right
      iexp = IAND( iexp, 255) - 127
c	print *,' iexp=',iexp
c get rid of the sign - bias = 127
      IF( jtemp .GT. 0 ) THEN
          jsign = 0
      ELSE
          jsign = -1
      ENDIF
      IF( iexp .GE. 0 ) THEN   
c is it greater than 1?
c          newexp = LRSHIFT( iexp, 2 ) + 65 
c****   replaced above with the following 3 line
          itemp = LRSHIFT( iexp, 2 )   ! iexp is max of FF or 255
          itemp = IAND( itemp, 63 )    ! 63 = hex(3F)
          newexp = itemp + 65          ! 65 = hex(41)
c divide by 4 (IBM is hex based)
          nshift = 3 - MOD(iexp,4)
      ELSE
          iexp = - iexp
c          newexp = 64 - LRSHIFT( iexp, 2 )
c****   replaced above with the following 3 line
          itemp = LRSHIFT( iexp, 2 )
          itemp = IAND( itemp, 63 )    ! 63 = hex(3F)
          newexp = 64 - itemp
          itemp = iexp + 3
          nshift = MOD( itemp, 4 )
          IF( nshift .EQ. 3 ) newexp = newexp + 1
      ENDIF
      newman = LRSHIFT( mant, nshift ) 
 *************          HOW DO WE CLEAR THE VACATED BITS?                     ***************
c IBM has the mantissa on the right
      IF( jsign .LT. 0 ) newexp =IOR( newexp, 128 )   ! 128 = hex(80)
c take care of the sign bit
      newexp = LLSHIFT( newexp, 24 )  
c IBM exp is on the left
      iobuf(i) =IOR( newman, newexp )
c	print *, newman,newexp,iobuf(i)
c	stop
c
 1000 CONTINUE
      RETURN
      END
