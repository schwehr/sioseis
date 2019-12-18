      SUBROUTINE zcmute( buf, is, ie, nsamps )
c    Mute (zero) from the first zero crossing before is, to
c to the first zero crossing after ie.  Will not zero before
c is = 1 or after ie = nsamps.
c    The rationale is that "normal" (old) mute blindly zeroed
c regardless of adjacent values and would cause "spikes" or
c edge effects.
c  
c   Paul Henkart, 3 September 2005, aboard Healy (so who owns this?)
c
      DIMENSION buf(nsamps)
c
      IF( is .LT. 1 .OR. ie .GT. nsamps ) RETURN
      indx1 = is
      indx2 = ie
      IF( is .GT. 1 ) THEN
          sign1 = SIGN(1.,buf(is))
          DO i = is-1, 1, -1
             temp = SIGN(1.,buf(i))
             IF( temp .NE. sign1 ) GOTO 100
             indx1 = i
          ENDDO
  100     CONTINUE
      ENDIF
      IF( ie .LT. nsamps ) THEN
          sign1 = SIGN(1.,buf(ie))
          DO i = ie+1, nsamps
             temp = SIGN(1.,buf(i))
             IF( temp .NE. sign1 ) GOTO 200
             indx2 = i
          ENDDO
  200     CONTINUE
      ENDIF
      DO i = indx1, indx2
         buf(i) = 0.
      ENDDO
      RETURN
      END
