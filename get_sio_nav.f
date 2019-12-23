      SUBROUTINE get_sio_nav( navfil, year, day, hour, rmin, sec,
     &          dlat, dlong, ishotno, istat )
c****
c    Read a line from a SIO nav file
c     REAL*8 dlat, dlong
c
c    Paul Henkart, Nov 2007
c****
c
      CHARACTER*40 token
      COMMON /sioln2/ ICHAR, NCHARS1, iprint, lunpo
      INTEGER ichar, nchars1, iprint, lunpo
      REAL*8 dlat, dlong, rminlat, rminlong, dtemp
c
      CALL rline( navfil )
      IF( nchars1 .LT. 1 ) THEN
          istat = -1
          RETURN
      ENDIF
      CALL getoke( token, nchars )
      CALL dcode( token, nchars, year, istat )
      CALL getoke( token, nchars )
      CALL dcode( token, nchars, day, istat )
      CALL getoke( token, nchars )
      CALL dcode( token, nchars, hour, istat )
      CALL getoke( token, nchars )
      CALL dcode( token, nchars, rmin, istat )
      CALL getoke( token, nchars )
      CALL dcode( token, nchars, sec, istat )
      CALL getoke( token, nchars )
      CALL ddcode( token, nchars, dlat, istat )
      CALL getoke( token, nchars )
      CALL ddcode( token, nchars, rminlat, istat )
      dtemp = DABS(dlat) + rminlat/60.D0
      IF( dlat .GT. 0 ) THEN
          dlat = dtemp
      ELSE
          dlat = -dtemp
      ENDIF
      CALL getoke( token, nchars )
      CALL ddcode( token, nchars, dlong, istat )
      CALL getoke( token, nchars )
      CALL ddcode( token, nchars, rminlong, istat )
      dtemp = DABS(dlong) + rminlong/60.D0
      IF( dlong .GT. 0 ) THEN
          dlong = dtemp
      ELSE
          dlong = -dtemp
      ENDIF
      CALL getoke( token, nchars )
      CALL dcode( token, nchars, areal, istat )
      ishotno = NINT(areal)
      istat = 0
      RETURN
      END
