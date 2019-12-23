      SUBROUTINE ldgo_tr0( c, shotno, jyr, jd, jhr, jmin, isec, ns,
     &      ship_lat, ship_long, wdepth,
     &      tail_lat, tail_long, tail_dist, tail_bear )
c
c  mod 20 Sep 02 - Change shallowest Hydrosweep from 100 to 10
c
      INTEGER shotno, y2k
      REAL*8 ship_lat, ship_long, tail_lat, tail_long
      REAL tail_dist, tail_bear, wdepth
      CHARACTER*512 c
      REAL*8 latmin, longmin
      SAVE ierr
      DATA ierr/0/, y2k/0/, owdepth/0/
c
c mod 23 Oct 95 - The format is different!  The shot is now 6 digits!
c mod feb 97 - get the mils
c mod Dec 99 - Add some additional diagnostics when it goes bad.
c            - Anticipate 2 digit years becoming 4 digit years.
c mod Sept 00 - make ship_lat, ship_long, tail_lat, tail_long REAL*8
c mod Oct 00 - Change some of the error and warning messages.
c            - Use last Hydrosweep depth if it's out of range.

c****  This turned ugly on HP-UX.  The address of c seems to get messed
c**** up.  Even cc had problems.
c****   Then the internal read didn't work on HP-UX either.  So,......
c****
c       PRINT *,c(1:190)
c        print *,c(1:60)                                                  ! The shot number, Joe's clock, Syntron clock
c       PRINT *,c(216:400)                                               ! print the Digicourse (bird) block
c      READ( c, 1 ) shotno, jyr, jd, jhr, jmin, sec, ns
c     , latdeg, latmin
c     , ew, longdeg, longmin, wdepth
c     &   ns1, latdeg1, latmin1, ew1, longdeg1, longmin1,
c     &   tail_dist, tail_bear
c    1 FORMAT(I5,1x,I2,1x,I3,1x,I2,1x,I2,1x,F6.3,21x,H1)
c    ,1x,I2,1x,F7.4,1x)
c     &   1H1,1x,I3,1x,F7.4,1x,F6.1,1x,4x,1x,4x,3x,1x)
c     &   1H1,1x,I2,1x,F7.4,1x,1H1,1x,I3,1x,F7.4,1x,F6.1,1x,F5.1)
c    1 FORMAT(I6,1x,I2,1x,I3,1x,I2,1x,I2,1x,F6.3,21x,H1)
c    ,1x,I2,1x,F7.4,1x)
c     &   1H1,1x,I3,1x,F7.4,1x,F6.1,1x,4x,1x,4x,3x,1x)
c     &   1H1,1x,I2,1x,F7.4,1x,1H1,1x,I3,1x,F7.4,1x,F6.1,1x,F5.1)
      ierr1 = 0
      ierr2 = 0
      ierr3 = 0
      i = 1
      n = 6
      IF( c(6:6) .EQ. ' ' ) n = 5
      CALL dcode( c(i:i), n, areal, istat )
      shotno = NINT(areal)
      i = i + n + 1                                                     ! 8
      IF( c(i+2:i+2) .NE. '+' .AND. c(i+4:i+4) .NE. '+' ) THEN
          IF( c(i+2:i+2) .EQ. '-' .OR. c(i+4:i+4) .EQ. '-' ) THEN
      PRINT *,' ***  WARNING  ***  Syntron error or guns did not fire.'
          ELSE
              ierr = ierr + 1
              ierr1 = 1
              IF( ierr .LT. 20 ) THEN
                  PRINT *,' ***  WARNING  ***  Bad LDEO clock.'
                 PRINT *,c(1:80)
              ENDIF
          ENDIF
      ENDIF
      IF( c(i+4:i+4) .EQ. '+' ) y2k = 2
      n = 2 + y2k
      IF( ierr1 .EQ. 0 ) CALL dcode( c(i:i), n, areal, istat )
      jyr = NINT(areal)
      i = i + n + 1                                                     ! 11
      n = 3
      IF( ierr1 .EQ. 0 ) CALL dcode( c(i:i), n, areal, istat )
      jd = NINT(areal)
      i = i + n + 1                                                     ! 15
      n = 2
      IF( ierr1 .EQ. 0 ) CALL dcode( c(i:i), n, areal, istat )
      jhr = NINT(areal)
      i = i + n + 1                                                     ! 18
      n = 2
      IF( ierr1 .EQ. 0 ) CALL dcode( c(i:i), n, areal, istat )
      jmin = NINT(areal)
      i = i + n + 1                                                     ! 21
      n = 3
      IF( ierr1 .EQ. 0 ) CALL dcode( c(i:i), n, areal, istat )
      isec = NINT(areal)
      i = i + 3
      IF( ierr1 .EQ. 0 ) CALL dcode( c(i:i), n, areal, istat )
      ns = NINT(areal)
      i = i + n + 23 + y2k                                              ! 50
      IF( (c(i-2:i-2) .NE. 'S' .AND. c(i-2:i-2) .NE. 'N') .OR.
     &    (c(i+11:i+11) .NE. 'E' .AND. c(i+11:i+11) .NE. 'W') ) THEN
          ierr = ierr + 1
          ierr2 = 1
          IF( ierr .LT. 10 ) THEN
              PRINT *,' ***  WARNING  ***  Bad LDEO block.'
              PRINT *,c(1:80)
              PRINT *,c(81:160)
          ENDIF
      ENDIF
      n = 2
      IF( ierr2 .EQ. 0 ) CALL ddcode( c(i:i), n, ship_lat, istat )
      i = i + n + 1                                                     ! 53
      n = 7
      IF( ierr2 .EQ. 0 ) CALL ddcode( c(i:i), n, latmin, istat )
      ship_lat = ship_lat + latmin / 60.D0
      IF( c(i-5:i-5) .EQ. 'S' ) ship_lat = -ship_lat                    ! 48
      i = i + n + 3                                                     ! 63
      n = 3
      IF( ierr2 .EQ. 0 ) CALL ddcode( c(i:i), n, ship_long, istat )
      i = i + n + 1                                                     ! 67
      n = 7
      IF( ierr2 .EQ. 0 ) CALL ddcode( c(i:i), n, longmin, istat )
      ship_long = ship_long + longmin / 60.D0
      IF( c(i-6:i-6) .EQ. 'W' ) ship_long = -ship_long
      i = i + n + 1                                                     ! 75
      n = 6
      CALL dcode( c(i:i), n, wdepth, istat )
      IF( wdepth .LT. 10. .OR. wdepth .GT. 10000 ) THEN
          IF( ierr .LT. 10 ) THEN
              ierr3 = ierr3 + 1
              PRINT *,' ***  WARNING  ***  Bad LDEO Hydrosweep of ',
     &           c(i:i+6)
              wdepth = owdepth
          ENDIF
          owdepth = wdepth
      ENDIF
      i = i + n + 17                                                    ! 98
      IF( (c(i-2:i-2) .NE. 'S' .AND. c(i-2:i-2) .NE. 'N') .OR.
     &    (c(i+11:i+11) .NE. 'E' .AND. c(i+11:i+11) .NE. 'W') ) THEN
          ierr3 = 1
          ierr = ierr + 1
          IF( ierr .LT. 10 .AND. ierr2 .EQ. 0 ) THEN
              PRINT *,' ***  WARNING  ***  Bad LDEO tail buoy GPS.'
              PRINT *,c(1:80)
              PRINT *,c(81:160)
          ENDIF
      ENDIF
      n = 2
      IF( ierr3 .EQ. 0 ) CALL ddcode( c(i:i), n, tail_lat, istat )
      i = i + n + 1                                                     ! 101
      n = 7
      IF( ierr3 .EQ. 0 ) CALL ddcode( c(i:i), n, latmin, istat )
      tail_lat = tail_lat + latmin / 60.D0
      IF( c(i-5:i-5) .EQ. 'S' ) tail_lat = -tail_lat
      i = i + n + 3                                                     ! 111
      n = 3
      IF( ierr3 .EQ. 0 ) CALL ddcode( c(i:i), n, tail_long, istat )
      i = i + n + 1                                                     ! 115
      n = 7
      IF( ierr3 .EQ. 0 ) CALL ddcode( c(i:i), n, longmin, istat )
      tail_long = tail_long + longmin / 60.D0
      IF( c(i-6:i-6) .EQ. 'W' ) tail_long = -tail_long
      i = i + n + 1                                                     ! 123
c   Ran into a weird bug on ARAD where the tail buoy info was off, so
c   jump out of 129 is not a blank as the specs say!
      j = 129 + y2k * 2
      IF( c(j:j) .NE. ' ' ) RETURN
      n = 6
      IF( ierr3 .EQ. 0 ) CALL dcode( c(i:i), n, tail_dist, istat )
      i = i + n + 1                                                     ! 130
      n = 5
      CALL dcode( c(i:i), n, tail_bear, istat )
c      print *,' shot = ',shotno,jyr, jd, jhr, jmin, isec
c      print *,ship_lat, ship_long, wdepth
c      print *,tail_lat, tail_long, tail_dist, tail_bear
      RETURN
      END
c12345112112311211211234561234567890123456789011
c	  1         2         3         4         5         6         7
c1234567890123456789012345678901234567890123456789012345678901234567890
c02411 94+190:17:59:41.834 94+190:17:59:41.981 N 50 58.2225 W 172 02.8
c 671 7216.7 06.9 20.7 331 N 00 0
c030939 95+129:20:12:11.107 95+129:20:12:11.236 N 16 28.8879 W 105 35.5400 3004.1 27.6 13.9 051 N 00 00.0000 E 000 00.0000 0000.0 000.0 133404.7 256.8
c000094 97+259:19:05:00.798 97+259:19:05:00.943 N 09 08.9672 W 104 15.2408 2728.2 28.9 00.0 000 N 00 00.0000 E 000 00.0000 11612919.1 279.4 test104.5 279.40
