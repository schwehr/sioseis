      SUBROUTINE calcrp3d( ihead, lhead, fbinx, fbiny, lbinx, lbiny,
     &     bin_h, bin_w, lprint, lrpadd )
c     Find the CMP bin number for the trace.  Put the bin number in lhead(6)
c     Translate the origin so that the center of the first bin is 0.
c     Rotate the bin-center-line (the "processing-line" so that it's the x-axis.
c     Translate and rotate the shot and receiver coordinates and poof.
c
c Arguments:
c     ihead = the I*2 segy trace header.
c     lhead = the I*4 segy trace header.
c     fbinx, fbiny are the coordinates of the first bin center
c     lbinx, lbiny are the coordinates of the last bin center
c     bin_h is the bin height
c     bin_w is the bin width
c
c (C) The Regents of the University of California.
c  Paul Henkart, Scripps Oceanography, 1 April 2005
c
c  mod 15 Jun 05 - Honor the (X/Y coordinates) scalar for words 19-22
c  mod 2 May 06 - add lprint
c               - other massive changes
c mod 14 Aug 07 - g95 ca't declare and set in same statement,

      REAL*8 first_x, first_y, last_x, last_y, azimuth, az,
     &       sx, sy, rx, ry
      REAL fbinx, fbiny, lbinx, lbiny, bin_h, bin_w
      REAL*8 x, y, sinaz, cosaz, mp_x, mp_y, dtemp, dtemp1
      REAL*8 deltax, deltay, angle, cosa, sina
      INTEGER*4 lhead(60)
      INTEGER*2 ihead(120)
      LOGICAL first
      SAVE
      DATA first/.TRUE./
      INTEGER fbin_no
      REAL*8 d19, d20, d21, d22, pi, rad, deg
      DATA pi/3.14159265358979/, rad/57.2957795131/,
     &       deg/.0174532925199/, fbin_no/0/

      IF( first ) THEN
          first = .FALSE.
          first_x = fbinx
          first_y = fbiny
          last_x = lbinx
          last_y = lbiny
          az = DATAN2((last_y-first_y),(last_x-first_x))
          azimuth = az * rad
          cosaz = DCOS(-az)
          sinaz = DSIN(-az)
          cosa = DCOS(az)
          sina = DSIN(az)
      ENDIF
      lhead(6) = 0
      lhead(7) = 0
      d19 = DFLOAT(lhead(19))
      d20 = DFLOAT(lhead(20))
      d21 = DFLOAT(lhead(21))
      d22 = DFLOAT(lhead(22))
      IF( ihead(36) .NE. 0 .AND. ihead(36) .NE. 0 ) THEN
          dtemp = DFLOAT(ihead(36))
          IF( ihead(36) .LT. 0 ) dtemp = -1. / dtemp
          d19 = d19 * dtemp
          d20 = d20 * dtemp
          d21 = d21 * dtemp
          d22 = d22 * dtemp
      ENDIF
c****  translate the axis so the first is the origin
      x = d19 - first_x
      y = d20 - first_y
c****  rotate the axis so that the bin center line is now the x axis
      sx = x*cosaz - y*sinaz
      sy =  x*sinaz + y*cosaz
      x = d21 - first_x
      y = d22 - first_y
      rx = x*cosaz - y*sinaz
      ry =  x*sinaz + y*cosaz
      mp_x = (sx + rx) / 2.
      mp_y = (sy + ry) / 2.
      IF( mp_y .GT. bin_h/2 .OR. mp_y .LT. -bin_h/2. ) lhead(15) = 2
c      lhead(6) = NINT((mp_x - first_x) / bin_w ) + fbin_no
      lhead(6) = NINT(mp_x / bin_w ) + fbin_no
      deltax = sx - rx
      deltay = sy - ry
      ihead(49) =  -NINT(mp_y)
c**** we need the angle between the processing line and the streamer.
      angle = DATAN2(deltay,deltax) * rad
      IF( IAND(lprint,128) .NE. 0 ) THEN
c****     The midpoint coordinates need to be rotated back
c****     az is the processing-line angle
          dtemp = mp_x*cosa - mp_y*sina + first_x
          dtemp1 = mp_x*sina + mp_y*cosa + first_y
          fangle = REAL(ihead(48)) / 10.
          PRINT 502, lhead(3), lhead(4), lhead(10), lhead(6)+lrpadd,
     &          fangle, -mp_y, d19, d20, d21, d22, dtemp, dtemp1,
     &          angle
  502     FORMAT
     &             (1X,I8,2X,I3,2(2X,I5),2X,F6.1,2X,F5.0,2X,
     &             6(1X,F11.1),2X,F6.1)
      ENDIF
      ihead(48) = NINT(angle*10.)
      RETURN
      END
