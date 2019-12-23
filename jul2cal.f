      PROGRAM jul2cal
      CHARACTER*80 token
      PRINT *, ' Print julian day to calendar day conversion.'
   10 CONTINUE
      PRINT *, ' Enter the year and julian day (eg 1980 331).'
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
      jday = NINT(areal)
      CALL julcal( month, iday, iyear, jday )
      PRINT *, ' calendar month =',month,' day=',iday
      END
