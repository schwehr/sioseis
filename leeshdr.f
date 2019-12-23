      SUBROUTINE leeshdr( ishot_hr, ishot_min, shot_sec,
     &    ifix_hr, ifix_min, fix_sec,
     &    dlat, dlong, wdepth, rmaggie)
c****  Decode Lee Ellett's Geometrics External header.
c   05-02-19,06:34:26,063424.35,-050 15 58.0260,-163 34 15.1560,1.0,115.2,009.8,4969,055757.411
c   06-09-06,03:03:11,030310,026 10.9520,-110 24.1460,2.1,207.0,5.9N, 2275.2

c      segddex put the line in "cbufin" for getokec(arg1,arg2).
c      getokec returns the token (delimited by commas) in arg 1.
c              arg 2 is the number of characters in the token.
c
c   mod Nov 13, 2006 - The format has changed.
c
      REAL*8 dlat, dlong, dtemp

      CHARACTER*82 out

      CALL getokec( out, n )
      CALL getokec( out, n )
      IF( out(3:3) .NE. ':' .OR. out(6:6) .NE. ':' ) THEN
          PRINT *,' Bad time in Lee header.'
          ishot_hr = 0
          ishot_min = 0
          shot_sec = 0
          ifix_hr = 0
          ifix_min = 0
          fix_sec = 0
          dlat = 0.
          dlong = 0.
          wdepth = 0.
          rmaggie = 0.
          RETURN
      ENDIF
      READ ( out, '(I2,1x,I2,1x,F2.0)' ) ishot_hr, ishot_min, shot_sec
      CALL getokec( out, n )
c      READ ( out, '(I2,I2,F7.2)' ) ifix_hr, ifix_min, fix_sec
      CALL dcode( out(1:1), 2, areal, istat )
      ifix_hr = NINT(areal)
      CALL dcode( out(3:3), 2, areal, istat )
      ifix_min = NINT(areal)
      CALL dcode( out(5:5), n-4, fix_sec, istat )
      CALL getokec( out, n )
c**** you can't add minutes to a neg lat, so save the sign.
      sign = +1.
      CALL dcode( out, n, areal, istat )
      IF( out(1:1) .EQ. '-' ) THEN
          sign = -1.
          areal = -areal
      ENDIF
      dlat = areal
      CALL getokec( out, n )
      CALL dcode( out, n, areal, istat )
      dtemp = areal
c****  It's now in floating point minutes like the GGA string - no seconds
c      CALL getokec( out, n )
c      CALL dcode( out, n, sec, istat )
c**** 60min =1deg, 60sec = 1min, 60*60sec = 1deg
c      dlat = dlat + dtemp / 60.D0 + sec / 3600.D0
      dlat = dlat + dtemp / 60.D0
      IF( sign .LT. 0. ) dlat = -dlat
      CALL getokec( out, n )
      sign = +1.
      CALL dcode( out, n, areal, istat )
      IF( out(1:1) .EQ. '-' ) THEN
          sign = -1.
          areal = -areal
      ENDIF
      dlong = areal
      CALL getokec( out, n )
      CALL dcode( out, n, areal, istat )
      dtemp = areal
c      CALL getokec( out, n )
c      CALL dcode( out, n, sec, istat )
c      dlong = dlong + dtemp / 60.D0 + sec / 3600.D0
      dlong = dlong + dtemp / 60.D0
      IF( sign .LT. 0. ) dlong = -dlong
      IF( out(1:1) .EQ. '-' ) dlong = -dlong
      CALL getokec( out, n )
      CALL dcode( out, n, hdop, istat )
      CALL getokec( out, n )
      CALL dcode( out, n, cog, istat )
      CALL getokec( out, n )
c      CALL dcode( out, n, sog, istat )
      CALL getokec( out, n )
      IF( n .GT. 6 ) THEN
          n = 6
          CALL dcode( out, n, wdepth, istat )
      ELSE
          CALL dcode( out, n, wdepth, istat )
          CALL getokec( out, n )
c**** it's terminated with cr/lf rather than null!
          CALL dcode( out, n-2, rmaggie, istat )
      ENDIF
      RETURN

      END
