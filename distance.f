      PROGRAM distance
c    Compute the distance, in meters, of between two points in
c  lat/long.
c

      REAL*8 deg, rmin, dlat1, dlong1, dlat2, dlong2, dtemp, sec
      REAL*8 deltalat, deltalong, dmperlat,dmperlong
      CHARACTER*20 token
      CHARACTER*200 CBUFIN
      COMMON /SIOLN1/ CBUFIN
      COMMON /sioln2/ jchar,NCBUF

      PRINT *,' Enter the latitude of the first point (deg, min, sec)'
      rmin = 0.
      sec = 0.
	 CALL rdline
      CALL getoke( token, nchars )
      CALL ddcode( token, nchars, deg, istat )
      CALL getoke( token, nchars )
      IF( nchars .GT. 0 ) THEN
          CALL ddcode( token, nchars, rmin, istat )
          CALL getoke( token, nchars )
          IF( nchars .GT. 0 ) THEN
              CALL ddcode( token, nchars, sec, istat )
          ENDIF
      ENDIF
c	 print *,' sec=',sec,' rmin=',rmin,' deg=',deg
      IF( deg .LT. 0. ) rmin = -rmin
      IF( deg .LT. 0. ) sec = -sec
      dlat1 = deg + rmin / 60.D0 + sec / (60.D0*60.D0)
c	 print *,' dlat1=',dlat1
c
      PRINT *,' Enter the longitude of the first point (deg, min)'
      rmin = 0.
      sec = 0.
      CALL rdline
      CALL getoke( token, nchars )
      CALL ddcode( token, nchars, deg, istat )
      CALL getoke( token, nchars )
      IF( nchars .GT. 0 ) THEN
          CALL ddcode( token, nchars, rmin, istat )
          CALL getoke( token, nchars )
          IF( nchars .GT. 0 ) THEN
              CALL ddcode( token, nchars, sec, istat )
          ENDIF
      ENDIF
c      print *,' sec=',sec,' rmin=',rmin,' deg=',deg
      IF( deg .LT. 0. ) rmin = -rmin
      IF( deg .LT. 0. ) sec = -sec
      dlong1 = deg + rmin / 60.D0 + sec / (60.D0*60.D0)
c	 print *,' dlong1=',dlong1
c
      PRINT *,' Enter the latitude of the second point (deg, min)'
      rmin = 0.
      sec = 0.
      CALL rdline
      CALL getoke( token, nchars )
      CALL ddcode( token, nchars, deg, istat )
      CALL getoke( token, nchars )
      IF( nchars .GT. 0 ) THEN
          CALL ddcode( token, nchars, rmin, istat )
          CALL getoke( token, nchars )
          IF( nchars .GT. 0 ) THEN
              CALL ddcode( token, nchars, sec, istat )
          ENDIF
      ENDIF
c      print *,' sec=',sec,' rmin=',rmin,' deg=',deg
      IF( deg .LT. 0. ) rmin = -rmin
      IF( deg .LT. 0. ) sec = -sec
      dlat2 = deg + rmin / 60.D0 + sec / (60.D0*60.D0)
c	 print *,' dlat2=',dlat2
c
      PRINT *,' Enter the longitude of the second point (deg, min)'
      rmin = 0.
      sec = 0.
      CALL rdline
      CALL getoke( token, nchars )
      CALL ddcode( token, nchars, deg, istat )
      CALL getoke( token, nchars )
      IF( nchars .GT. 0 ) THEN
          CALL ddcode( token, nchars, rmin, istat )
          CALL getoke( token, nchars )
          IF( nchars .GT. 0 ) THEN
              CALL ddcode( token, nchars, sec, istat )
          ENDIF
      ENDIF
c      print *,' sec=',sec,' rmin=',rmin,' deg=',deg
      IF( deg .LT. 0. ) rmin = -rmin
      IF( deg .LT. 0. ) sec = -sec
      dlong2 = deg + rmin / 60.D0 + sec / (60.D0*60.D0)
c	 print *,' dlong2=',dlong2
c
      CALL dlendeg(dlat1,dmperlat,dmperlong)
      deltalat = (dlat2-dlat1) * dmperlat
      deltalong = (dlong2-dlong1) * dmperlong
c	 print *,' dmperlat=',dmperlat,' dmperlong=',dmperlong
c	 print *,' deltalat=',deltalat,' deltalong=',deltalong
      dtemp = DSQRT(deltalat*deltalat +deltalong*deltalong)
      PRINT *,' The distance between the two points is',dtemp,
     & ' meters.'
      temp = dtemp / 1852.D0
      PRINT *,' The distance between the two points is',temp,
     & ' nautical miles.'
      STOP
      END
