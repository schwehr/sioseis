      SUBROUTINE navgeom( navfil, ihead, lhead, dfls )
c  Return dfls and insert long/lat in SEG-Y header words 19, 20.
c     Compute and return DLFS (distance from last shot) from the SEG-Y
c  header and a navigation file whose format is:
c      yr day hr min sec deglat minlat deglong minlong
c  yr can be 2 or 4 digits (e,g, 1997 or 97)
c  day is day of year
c  hr is the 24 hour clock
c  min
c  sec is the floating point second of the fix.
c  deglat is the latitude in degrees.  Minus is South.
c  minlat is the decimal minutes of latitude
c  deglong is the longitude in degrees.  Minus is West.
c  longmin is the decimal minutes of longitide.
c
c  The entries are free field and may be blank or tab delimited.
c
c   Think SEG-Y and think GPS.
c  Assume a flat earth.
c  Assume a constant distance per degree latitude/longitude after
c      the first call.
c
c  mod Oct 00 - Add the illegal SEG-Y milliseconds
c             - make a bunch more variables REAL*8
c             - check for the shot being at the fix, esp with mils!
c  mod 17 Oct 02 - D0 negative lat/long on first one correctly!
c  mod 20 May 04 - Write the long/lat into the SEG-Y header
c                - Bad dfls when shot time = fix time
c  mod 24 May 04 - The scalar in word 36 had the wrong sign
c  mod 3 Aug 04 - Didn't handle longitude over 180/-180
c  mod 25 Aug 05 - Changed scalar from 10 to 100.
c  mod 29 Apr 10 - Check for mil changing (might be shooting fast!)
c  mod 5 May 10 - Change warning of shot-times to < 1 second.
c

      INTEGER*2 ihead(120)
      DIMENSION lhead(60)
      CHARACTER*10 token
      COMMON /sioln2/ ICHAR, NCHARS1, iprint, lunpo
      INTEGER ichar, nchars1, iprint, lunpo
      REAL*8 dlat, dlong, dlatlast, dlonglast, dtemp, dxm, dym, distm,
     &       timefix, timelast, timeshot, sec, speed, secs, scalar
      REAL*8 seclog, deglat, rminlat, deglong, rminlong, dpdlat, dpdlong
      LOGICAL first
      SAVE 
      DATA lastmin/-1/, timelast, timefix/2*400.D0/, scalar/100./
      DATA first/.TRUE./

c**** if same shot time
      IF( ihead(83) .EQ. lastsec .AND. ihead(82) .EQ. lastmin .AND.
     &    ihead(81) .EQ. ihr .AND. ihead(80) .EQ. lastday .AND.
     *    ihead(84) .EQ. lastmil ) THEN
          dtemp = speed * secs
          dfls = dtemp
          lhead(19) = NINT(dlong * 60.D0 * 60.D0 * scalar)
          lhead(20) = NINT(dlat * 60.D0 * 60.D0 * scalar)
          ihead(36) = -NINT(scalar)
          ihead(45) = 2
          RETURN
      ENDIF
      iday = ihead(80)
      ihr = ihead(81)
      min = ihead(82)
      isec = ihead(83)
      mil = 0
      IF( ihead(84) .GT. 4 ) mil = ihead(84)
      ishotno = lhead(3)
      IF( lastmin .GE. 0 .AND. ishotno .NE. lastshotno + 1 ) THEN
          PRINT *,' Non consecutive shot numbers ',lastshotno,ishotno
      ENDIF
      sec = DBLE(isec) + DBLE(mil)/1000.D0
      timeshot = DBLE(iday) + DBLE(ihr)/24.D0 +
     &           DBLE(min)/1440.D0 + sec/86400.D0
c	 print *,' segy ',ishotno,iday,ihr,min,isec,mil,timeshot
c      print *,lastshotno,lastday,lasthr,lastmin,lastsec,timelast,timefix
c**** First time through is really just setting up.
      IF( first ) THEN
          dfls = 0
          lastyr = ihead(79)
          lastday = ihead(80)
          lasthr = ihead(81)
          lastmin = ihead(82)
          lastsec = ihead(83)
c****     SEG-Y standard says word 84 is the time base (local vs GMT),
c****     SIOSEIS likes to put the millisecond in there
          lastmil = 0
          IF( ihead(84) .gt. 4 ) lastmil = ihead(84)
          lastshotno = lhead(3)
   10     CONTINUE
c****     get the fix of the first shot
c	 print *,' timeshot=',timeshot,' timefix=',timefix
          IF( timeshot .LT. timefix ) THEN
              dlatlast = dlat
              dlonglast = dlong
              timelast = timefix
              CALL rline( navfil )
              IF( nchars1 .LT. 1 ) THEN
                  PRINT *,' ***  ERROR  ***  SEGY time ', lastday,
     &              lasthr, lastmin, lastsec,' not in NAVFIL.'
                  RETURN
              ENDIF
              CALL getoke( token, nchars )
              CALL dcode( token, nchars, areal, istat )
              logyr = NINT(areal)
              CALL getoke( token, nchars )
              CALL dcode( token, nchars, areal, istat )
              logday = NINT(areal)
              CALL getoke( token, nchars )
              CALL dcode( token, nchars, areal, istat )
              loghr = NINT(areal)
              CALL getoke( token, nchars )
              CALL dcode( token, nchars, areal, istat )
              logmin = NINT(areal)
              CALL getoke( token, nchars )
              CALL ddcode( token, nchars, seclog, istat )
              CALL getoke( token, nchars )
              CALL ddcode( token, nchars, deglat, istat )
              CALL getoke( token, nchars )
              CALL ddcode( token, nchars, rminlat, istat )
              CALL getoke( token, nchars )
              CALL ddcode( token, nchars, deglong, istat )
              CALL getoke( token, nchars )
              CALL ddcode( token, nchars, rminlong, istat )
              timefix = DBLE(logday) + DBLE(loghr)/24.D0 +
     &                  DBLE(logmin)/1440.D0 + seclog/86400.D0
c	 print *,' read log1 ',logday, loghr, logmin, deglat,rminlat,
c     &                    deglong, rminlong, timefix
              GOTO 10
          ENDIF
          dlat = DABS(deglat) + rminlat/60.D0
          IF( deglat .LT. 0 ) dlat = -dlat
          dlong = DABS(deglong) + rminlong/60.D0
          IF( deglong .LT. 0 ) dlong = -dlong
C         GET DISTANCE (KM) PER DEGREE
          CALL dlendeg( dlat, dpdlat, dpdlong )
c	print *,' dlat=',dlat,' dpdlat=',dpdlat,' dpdlong=',dpdlong
          IF( timeshot .LE. timefix ) THEN
              first = .FALSE.
              RETURN
          ENDIF
      ENDIF
  100 CONTINUE
c	 print *,' at 100, shot ',timeshot,' fix ',timefix, first
      IF( timeshot .GT. timefix ) THEN
          dlatlast = dlat
          dlonglast = dlong
          timelast = timefix
          CALL rline( navfil )
          IF( nchars1 .LT. 1 ) THEN
              PRINT *,' ***  ERROR  ***  SEGY time ', lastday,
     &              lasthr, lastmin, lastsec,' not in NAVFIL.'
              lastshotno = ishotno
              RETURN
          ENDIF
          CALL getoke( token, nchars )
          CALL dcode( token, nchars, areal, istat )
          logyr = NINT(areal)
          CALL getoke( token, nchars )
          CALL dcode( token, nchars, areal, istat )
          logday = NINT(areal)
          CALL getoke( token, nchars )
          CALL dcode( token, nchars, areal, istat )
          loghr = NINT(areal)
          CALL getoke( token, nchars )
          CALL dcode( token, nchars, areal, istat )
          logmin = NINT(areal)
          CALL getoke( token, nchars )
          CALL ddcode( token, nchars, seclog, istat )
          CALL getoke( token, nchars )
          CALL ddcode( token, nchars, deglat, istat )
          CALL getoke( token, nchars )
          CALL ddcode( token, nchars, rminlat, istat )
          CALL getoke( token, nchars )
          CALL ddcode( token, nchars, deglong, istat )
          CALL getoke( token, nchars )
          CALL ddcode( token, nchars, rminlong, istat )
          dlat = DABS(deglat) + rminlat/60.D0
          IF( deglat .LT. 0 ) dlat = -dlat
          dlong = DABS(deglong) + rminlong/60.D0
          IF( deglong .LT. 0 ) dlong = -dlong
          timefix = DBLE(logday) + DBLE(loghr)/24.D0 +
     &              DBLE(logmin)/1440.D0 + seclog/86400.D0
c          print *,' read log2 ',logday, loghr, logmin, seclog,
c     &           deglat,rminlat, deglong, rminlong, timefix
C         CALCULATE THE LATITUDE RANGE (Y) IN METERS
          dym=(dlat-dlatlast)*dpdlat
C         CALCULATE THE LONGITUDE RANGE (X) IN METERS
          dxm=(dlong-dlonglast)*dpdlong
c        print *,' dlong ',dlong,dlonglast,dxm,dpdlong
          IF( DABS(dxm) .GT. 100000. ) THEN
c	 print *, DABS(dlong - dlonglast)
              IF( DABS(dlong - dlonglast) .GT. 180. ) THEN
                  IF( dlong .LT. 0. ) THEN
                      dtemp = 180. + (180.+dlong)
                      dxm=(dtemp-dlonglast)*dpdlong
                  ELSE
                      dtemp = 180. + (180.+dlonglast)
                      dxm=(dlong-dtemp)*dpdlong
                  ENDIF
              ENDIF
          ENDIF
C         CALCULATE THE RANGE (ASSUMING A FLAT EARTH)
          distm = DSQRT(dxm*dxm+dym*dym)
c	 print *,' dlat ',dlat,dlatlast,dym,dpdlat
c	 print *,' dlong ',dlong,dlonglast,dxm,dpdlong
c	print *,' distm=',distm
          GOTO 100
      ENDIF
c      IF( timeshot .EQ. timefix ) THEN
c          dtemp = distm
c      ELSE
          secs = DFLOAT(iday-lastday) * 24.D0 * 60.D0 * 60.D0 +
     &       DFLOAT(ihr-lasthr) * 60.D0 * 60.D0 +
     &       DFLOAT(min-lastmin) * 60.D0 +
     &       DFLOAT(isec-lastsec) +
     &       DFLOAT(mil-lastmil)/1000.D0
          IF( secs .LT. 1 .OR. secs .GT. 60 )THEN
              IF( .NOT. first ) PRINT *,
     &          ' Odd SEG-Y shot time on shot ',ishotno,' time ',
     &          iday, ihr, min, isec, secs
          ENDIF
c	 print *,' distm=',distm,' time ',(timefix-timelast)*86400.D0
          speed = distm / ((timefix-timelast)*86400.D0)
c	print *,' speed=',speed,' secs=',secs
          dtemp = speed * secs
c      ENDIF
      dfls = SNGL(dtemp)
      lastday = iday
      lasthr = ihr
      lastmin = min
      lastsec = isec
      lastmil = mil
      lastshotno = ishotno
      rlastlat = deglat
      rlastlong = deglong
c**** ibuf(36) is the "scalar to be applied to the coordinates in
c**** bytes 73-88 (lbuf(19), lbuf(20), lbuf(21), lbuf(22))
c**** lbuf(19) is "longitude in seconds of arc" of the shot
c**** lbuf(20) is "latitude in seconds of arc" of the shot
c**** ibuf(45) is the type of units - 2 = seconds of arc
      lhead(19) = NINT(dlong * 60.D0 * 60.D0 * scalar)
      lhead(20) = NINT(dlat * 60.D0 * 60.D0 * scalar)
      ihead(36) = -NINT(scalar)
      ihead(45) = 2
      first = .FALSE.
      RETURN
      END
