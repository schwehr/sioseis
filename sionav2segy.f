      SUBROUTINE sionav2segy( navfil, lhead, ihead )
c****    
c   Determine the lat/long of a trace given an sio nav file and
c   put the nav into the segy header.
c
c   The format of the SIOSEIS navigation file is:
c   year day hour minute second lat/deg lat/min long/deg long/min
c   e.g.     1997 67 12 0 0 -69 43.2954 170 23.646
c
c   Paul Henkart, Nov. 2007
c
c mod 3 Jan 08 - Use the first/last nav point if the segy trace is outside
c mod 29 Apr 10 - Use segy header word 84 for millisecond if it's greater than 4.
c
c****

      INTEGER*2 ihead(120)
      INTEGER*4 lhead(60)
      LOGICAL first
      REAL*8 dlat1, dlat2, dlong1, dlong2, dlat, dlong
      REAL*8 dday1, dday2, dday, x, a, c
      REAL*8 sec_per_day, scalar
      DATA sec_per_day/86400./, scalar/100./, first/.TRUE./
      SAVE

c**** Assume the data are sorted by shot if the rp trace number
c**** (word 7) is 0.
      IF( lhead(7) .EQ. 0 ) THEN
          isegyrecno = lhead(3)
      ELSE
          isegyrecno = lhead(6)
      ENDIF

      IF( first ) THEN
          first = .FALSE.
          last_rec = isegyrecno
          CALL get_sio_nav( navfil, year1, day, hour, rmin, sec, 
     &          dlat1, dlong1,navrec1, istat )
          day1 = day + (sec + rmin*60 + hour*60.*60.) / sec_per_day
          CALL get_sio_nav( navfil, year2, day, hour, rmin, sec, 
     &          dlat2, dlong2, navrec2, istat )
          day2 = day + (sec + rmin*60 + hour*60.*60.) / sec_per_day
          navdir = navrec2 - navrec1
          IF( navrec2 .LT. navrec1 ) THEN
              day1 = -day1
              day2 = -day2
          ENDIF
      ENDIF

      isegydir = isegyrecno - last_rec
      iyear = ihead(79)
      day = (ihead(80))
      hr = (ihead(81))
      rmin = (ihead(82))
      sec = (ihead(83))
      dday = day + (sec + rmin*60 + hr*60.*60.) / sec_per_day
c**** the shot time millisecond may be in word 84
c**** 84 is the time basis code
      IF( ihead(84) .GT. 4 ) dday = dday + DFLOAT(ihead(84))/1000.

      IF( dday .NE. 0 ) THEN
          IF( dday .GT. day2 ) THEN
  100         day1 = day2
              dlat1 = dlat2
              dlong1 = dlong2
              CALL get_sio_nav( navfil, year2, day, hour, rmin, sec,
     &              dlat2, dlong2, navrec2, istat )
              IF( istat .NE. 0 ) RETURN
              day2 = day + (sec + rmin*60 + hour*60.*60.) / sec_per_day
              IF( dday .GT. day2 ) GOTO 100
          ENDIF
          x = dday
          a = dday1
          c = dday2
      ELSE
  110     IF( isegydir .GE. 0 .AND. isegyrecno .GT. navrec2 ) THEN
              navrec1 = navrec2
              dlat1 = dlat2
              dlong1 = dlong2
              CALL get_sio_nav( navfil, year2, day, hour, rmin, sec,
     &              dlat2, dlong2, navrec2, istat )
              IF( istat .NE. 0 ) THEN
c****         the segy trace is after the first nav point
                  dlat = dlat2
                  dlong = dlong2
                  GOTO 1000
              ENDIF
              navrec2 = navrec2 * navdir
              IF( isegyrecno .GT. navrec2 ) GOTO 110
          ENDIF
c****     If the ASCII file is increasing, but the ones wanted are decreasing
c****     (like the line is shot backwards from the way it was surveyed).
          IF( isegydir .LE. 0 .AND. isegyrecno .GE. navrec2 ) THEN
              dlat = dlat2
              dlong = dlong2
              GOTO 1000
          ENDIF
          IF( isegydir .LT. 0 .AND. isegyrecno .LT. navrec1 ) THEN
              REWIND navfil     ! rewind and start from the begining
              CALL get_sio_nav( navfil, year2, day, hour, rmin, sec,
     &              dlat2, dlong2, navrec2, istat )
              IF( istat .NE. 0 ) THEN
c****         the segy trace is before the first nav point
                  REWIND navfil     ! rewind and start from the begining
                  CALL get_sio_nav( navfil, year2, day, hour, rmin, sec,
     &              dlat, dlong, navrec2, istat )
                  GOTO 1000
              ENDIF
              navrec2 = navrec2 * navdir 
              GOTO 110
          ENDIF
          x = isegyrecno
          a = navrec1
          c = navrec2
      ENDIF

      dlat = (x-c) / (a-c) * (dlat1-dlat2) + dlat2
      dlong = (x-c) / (a-c) * (dlong1-dlong2) + dlong2
 1000 CONTINUE
      last_rec = isegyrecno
c**** ibuf(36) is the "scalar to be applied to the coordinates in
c**** bytes 73-88 (lbuf(19), lbuf(20), lbuf(21), lbuf(22))
c**** lbuf(19) is "longitude in seconds of arc" of the shot
c**** lbuf(20) is "latitude in seconds of arc" of the shot
c**** ibuf(45) is the type of units - 2 = seconds of arc
      lhead(19) = NINT(dlong * 60.D0 * 60.D0 * scalar)
      lhead(20) = NINT(dlat * 60.D0 * 60.D0 * scalar)
      ihead(36) = -NINT(scalar)
      ihead(45) = 2

      RETURN
      END
