      SUBROUTINE gpgga( ihour, min, isec, dlat, dlong, iquality,
     &        nsats, hdop, dgps_sec, dgps_id )
c****  GPGGA decodes a NMEA $GPGGA string that was read via subroutine
c****  rline.  The string must be in rline common buffer because
c****  that's where subroutine getoke expects it.
c
c mod 4 June 2004 - set everything to zero if the first field is empty.
c mod 8 October 2006 - set everything to zero if the lat or long are empty.
c mod 19 June 2007 - Read minutes as REAL*8
c
      REAL*8 dlat, dlong, dtemp, dmin
      INTEGER dgps_sec
      CHARACTER*3 dgps_id

      CHARACTER*82 out

      CALL getokec( out, n )
      IF( out(1:6) .NE. '$GPGGA' ) THEN
          PRINT *,'  ***  ERROR  ***  Nav string is not $GPGGA',
     &        out(1:n)
          STOP
      ENDIF
      CALL getokec( out, n )
      IF( n .LT. 6 ) GOTO 9000
      READ ( out, '(I2,I2,I2)' ) ihour, min, isec
      CALL getokec( out, n )
      IF( n .LT. 4 ) GOTO 9000
      CALL dcode(out, 2, deg, istat )
      ideg = deg
      CALL ddcode(out(3:n), n-2, dmin, istat )
      dlat = DFLOAT(ideg)
      dlat = dlat + dmin / 60.D0
      CALL getokec( out, n )
      IF( out(1:1) .EQ. 'S' ) dlat = -dlat
      CALL getokec( out, n )
      IF( n .LT. 4 ) GOTO 9000
      CALL dcode(out, 3, deg, istat )
      ideg = deg
      CALL ddcode(out(4:n), n-3, dmin, istat )
      dlong = DFLOAT(ideg)
      dlong = dlong + dmin / 60.D0
      CALL getokec( out, n )
      IF( out(1:1) .EQ. 'W' ) dlong = -dlong
      CALL getokec( out, n )
      READ( out, '(I1)' ) iquality
      CALL getokec( out, n )
c $GPGGA,125955,3730.50910,N,12230.31960,W,1,6,3.5,591.0,M,0.0,M,3.0,0005*7F
c      READ( out, '(I2)' ) nsats
      CALL dcode(out, n, areal, istat )
      nsats = areal
      CALL getokec( out, n )
c      READ( out, '(F3.1)' ) hdop
      CALL dcode(out, n, hdop, istat )
c     altitude, meters
      CALL getokec( out, n )
      CALL getokec( out, n )
c     geoid height, meters
      CALL getokec( out, n )
      CALL getokec( out, n )

      CALL getokec( out, n )
      dgps_sec = 99999.
c      IF( n .GT. 0 ) READ( out, '( )' ) dgps_sec
      CALL getokec( out, n )
      dgps_id = ' '
      IF( n .GT. 0 ) dgps_id = out(1:n)

      RETURN

 9000 CONTINUE
      ihour = 0
      min = 0
      isec = 0
      dlat = 0.
      dlong = 0.
      iquality = 0
      nsats = 0
      hdop = 0
      dgps_sec = 0
      dgps_id = ' '
      RETURN


      END
