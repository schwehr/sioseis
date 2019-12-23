      PROGRAM cal2jul
      CHARACTER*80 token
      PRINT *, ' Print calendar day to julian day conversion.'
   10 CONTINUE
      PRINT *, ' Enter year month day (eg 1980 10 13)'
      CALL rdline
      CALL getoke( token, nchars)
      IF( nchars .LT. 1 ) THEN
          PRINT *,' ERROR '
          GOTO 10
      ENDIF
      CALL dcode( token, nchars, areal, istat)
      iyear = NINT(areal)
      CALL getoke( token, nchars)
      IF( nchars .LT. 1 ) THEN
          PRINT *,' ERROR '
          GOTO 10
      ENDIF
      CALL dcode( token, nchars, areal, istat)
      month = NINT(areal)
      CALL getoke( token, nchars)
      IF( nchars .LT. 1 ) THEN
          PRINT *,' ERROR '
          GOTO 10
      ENDIF
      CALL dcode( token, nchars, areal, istat)
      iday = NINT(areal)
      CALL caljul( month, iday, iyear, jday )
      PRINT *, ' julian day=',jday
      END
