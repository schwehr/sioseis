      SUBROUTINE gpggaa( ihour, min, isec, mils, dlat, dlong, istat )
c****  mils will be 9999 if it's not in the nmea string.
c****  GPGGA decodes a NMEA $GPGGA string that was read via subroutine
c****  rline.  The string must be in rline common buffer because
c****  that's where subroutine getoke expects it.
c
c mod 4 June 2004 - set everything to zero if the first field is empty.
c mod 8 October 2006 - set everything to zero if the lat or long are empty.
c mod 19 June 2007 - Read minutes as REAL*8
c mod 23 Feb 2016 - UW Micro Eel  is bad:
c   $GPGGA,230741.60,3258.8455102,N,11758.7932197,W,1,0?,1.2,5.0,M,-33.6,M,,*58
c         so just return after doing dlong and forget the other things nobody uses
c mod 29 Mar 2016 - Geo Eel might screw up any field, so check every field for proper length
c         and keep track of a delta_lat and delta_long and apply them to the last lat/long
c         if the current lat/long is screwed up.
c                 - add mils
c  $GPGGA,084117.20,3308.8398645,N,11841.9110338,W,1,09,0.9,7.2,M,-34.0,M,,*51
c  $GPGGA,084122.20,3308.8382883,N,11841.9030828,W,1,09,0.9,6.9,M,-34.0,M,,*5B
c  $GPGGA,08412w.20,3308.8367035,N,11841.8953377,W,1,09,0.9,7.2,M,-34.0,M,,*56   BAD timestamp
c  $GPGGA,084132.20,3308.8352674,N,11851.8870243,W,1,09,0.9,7.0,M,-34.0,M,,*53   BAD longitude minute
c  $GPGGA,084137.20,3308.8335151,N,11841.8790745,W,1,09,0.9,6.6,M,-34.0,M,,*52
c
c mod 29 Mar 2016 - change to gpggaa to delete unused arguments and add istat
c         istat = 0 if ok
c               = 1 if not ok - an error occurred
c mod 13 Apr 2016 - detecting bad $GPGGA is uglier than I thought.  Don't try to correct anything
c        until all fields are examined.  i.e. lat may pass being the right length and being a
c        number, but may be bad - the long may not be detected bad.
c
      REAL*8 dlat, dlong, dtemp, dmin
      REAL*8 delta_lat/99999./, delta_long/99999./, last_lat/0/,
     &       last_long/0/
      INTEGER dgps_sec
      CHARACTER*3 dgps_id
      INTEGER ishort/1/
      CHARACTER*82 out
      SAVE

      istat = 0
      CALL getokec( out, n )
      IF( out(1:6) .NE. '$GPGGA' ) THEN
          PRINT *,'  ***  ERROR  ***  Nav string is not $GPGGA',
     &        out(1:n)
          STOP
      ENDIF
c
c     get the timestamp
      CALL getokec( out, n )
      IF( n .LT. 6 ) GOTO 9000
c      READ ( out, '(I2,I2,I2)' ) ihour, min, isec
c      Unfortunately, g77 crashes if the above is bad
      CALL dcode( out(1:1), 2, areal, jstat )
      IF( jstat .EQ. 2 ) THEN
          ihour = NINT(areal)
      ELSE
          ihour = 999
          GOTO 10000
      ENDIF
      CALL dcode( out(3:3), 2, areal, jstat )
      IF( jstat .EQ. 2 ) THEN
          min = NINT(areal)
      ELSE
          min = 999
          GOTO 10000
      ENDIF
      CALL dcode( out(5:5), 2, areal, jstat )
      IF( jstat .EQ. 2 ) THEN
          isec = NINT(areal)
      ELSE
          isec = 999
          GOTO 10000
      ENDIF
      mils = 9999
c      IF( n .EQ. 9 ) READ ( out, '(7X,I2)' ) mils
      IF( n .EQ. 9 ) THEN
          CALL dcode( out(8:8), 2, areal, jstat )
          IF( jstat .EQ. 2 ) THEN
              mils = NINT(areal)
          ELSE
              mils = 9999
              GOTO 10000
          ENDIF
      ENDIF
c
c     Get the lat
      CALL getokec( out, n )
      IF( n .LT. 9 ) THEN        ! some "old" units didn't have much precision
          IF( delta_lat .EQ. 99999. ) GOTO 9000
c         if lat is bad, long will be too
          GOTO 10000
      ENDIF
      CALL dcode(out, 2, deg, jstat )
      IF( jstat .NE. 2 ) GOTO 10000    !   2 means successful
      ideg = deg
      CALL ddcode(out(3:n), n-2, dmin, jstat )
      IF( jstat .NE. 2 ) GOTO 10000    !   2 means successful
      dlat = DFLOAT(ideg)
      dlat = dlat + dmin / 60.D0
      CALL getokec( out, n )
      IF( n .NE. 1 ) GOTO 10000
      IF( out(1:1) .EQ. 'S' ) dlat = -dlat
c
c     Get the long
      CALL getokec( out, n )
      IF( n .LT. 9 ) THEN        ! some "old" units didn't have much precision
          IF( delta_long .EQ. 99999. ) GOTO 9000
          GOTO 10000
      ENDIF
      CALL dcode(out, 3, deg, jstat )
      IF( jstat .NE. 2 ) GOTO 10000    !   2 means successful
      ideg = deg
      CALL ddcode(out(4:n), n-3, dmin, jstat )
      IF( jstat .NE. 2 ) GOTO 10000    !   2 means successful
      dlong = DFLOAT(ideg)
      dlong = dlong + dmin / 60.D0
      CALL getokec( out, n )
      IF( n .NE. 1 ) GOTO 10000
      IF( out(1:1) .EQ. 'W' ) dlong = -dlong
c
c
c     clean up for the next fix
      IF( last_lat + last_long .NE. 0. ) THEN     ! can only do this on/after second fix
          delta_lat = dlat - last_lat
          delta_long = dlong - last_long
      ENDIF

 9000 CONTINUE
      last_lat = dlat
      last_long = dlong
      RETURN

10000 CONTINUE
      dlat = last_lat + delta_lat
      dlong = last_long + delta_long
      istat = 1
      GOTO 9000

      END
