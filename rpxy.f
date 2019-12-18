      SUBROUTINE rpxy( lhdr, ihdr, setback )
c****  Given the source (x,y) and the range from the segy header,
c****  Remove the setback from the source x,y and rewrite 19 & 20 and
c****  compute the receiver (x,y) and write it in header 21 & 22
c
c  mod 24 Sep 09 - Compute the angle differently
c  mod 15 Mar 13 - make more variables REAL*8, treat first differently
c  mod 20 Mar 13 - Make wbaldwin changes for quadrant.
c
      INTEGER*4 lhdr(60), shotno1, shotno2
      INTEGER*2 ihdr(120)
      REAL*8 d19, d20, d21, d22, dtemp, ddlat, x1_deg, y1_deg, x2_deg,
     &   y2_deg, ddlong, angle, x1_m, y1_m, x2_m, y2_m, dx, dy
      DATA x1_deg/-99999./, x2_deg/-99999./, angle/-999./
      DATA pi/3.14159265358979/, rad/57.2957795131/, deg/.0174532925199/
      SAVE
c
      IF( ihdr(45) .NE. 2 ) THEN
          PRINT *,' ***  WARNING  *** SETBACK  not applied.  Nav must be
     & in arcseconds.'
          RETURN
      ENDIF
c      print *,' lhdr=',lhdr(19),lhdr(20)
      IF( x2_deg .EQ. -99999. ) THEN     ! is it the first shot?
          shotno1 = -1
          shotno2 = lhdr(3)
          d19 = DFLOAT(lhdr(19))
          d20 = DFLOAT(lhdr(20))
          IF( ihdr(36) .NE. 0 ) THEN
              dtemp = DFLOAT(ihdr(36))
              IF( ihdr(36) .LT. 0 ) dtemp = -1. / dtemp
              d19 = d19 * dtemp
              d20 = d20 * dtemp
          ENDIF
          d19 = d19 / (60.D0 * 60.D0 )    ! convert arcseconds to degrees
          d20 = d20 / (60.D0 * 60.D0 )
          CALL dlendeg( d20,ddlat,ddlong)
          x2_deg = d19
          y2_deg = d20
          x1_deg = d19
          y1_deg = d20
          x1_m = x1_deg * ddlong  ! 1 is the same as 2
          y1_m = y1_deg * ddlat
          x2_m = d19 * ddlong
          y2_m = d20 * ddlat
      ENDIF
      IF( shotno2 .NE. lhdr(3) ) THEN
c**** Use the existing angle if the same shot as before
          shotno1 = shotno2
          shotno2 = lhdr(3)
          d19 = DFLOAT(lhdr(19))
          d20 = DFLOAT(lhdr(20))
          IF( ihdr(36) .NE. 0 ) THEN
              dtemp = DFLOAT(ihdr(36))
              IF( ihdr(36) .LT. 0 ) dtemp = -1. / dtemp
              d19 = d19 * dtemp
              d20 = d20 * dtemp
          ENDIF
          d19 = d19 / (60.D0 * 60.D0 )
          d20 = d20 / (60.D0 * 60.D0 )
          x1_deg = x2_deg
          y1_deg = y2_deg
          x2_deg = d19
          y2_deg = d20
          CALL dlendeg( d20, ddlat, ddlong )
          x1_m = x1_deg * ddlong
          y1_m = y1_deg * ddlat
          x2_m = d19 * ddlong
          y2_m = d20 * ddlat
          angle = DATAN2((y2_m-y1_m),(x2_m-x1_m)) * rad
c       print *,' heading=',angle
          IF( (x2_m-x1_m) .LT. 0 .AND. (y2_m-y1_m) .GE. 0 ) THEN
              angle = 450.D0 - angle
          ELSE
              angle = 90.D0 - angle
          ENDIF
c       print *,' angle=',angle 
         IF (angle .GT. 0 .AND. angle .LE. 180) THEN
              angle = angle + 180
          ELSE IF (angle .GT. 180 .AND. angle .LE. 360) THEN
              angle = angle - 180
          ENDIF
      ENDIF
      IF( shotno1 .LT. 0 ) RETURN
c	print *,' 1_m=',x1_m,y1_m,' 2=',x2_m,y2_m,' inverse_angle=',angle
c****
c****   Do the source coordinates
c****
      dtemp = setback
      dx = dtemp * DSIN(angle/rad)
      dy = dtemp * DCOS(angle/rad)
c	 print *,' dx=',dx,' dy=',dy,' dtemp=',dtemp,x2_m,y2_m
      d19 = x2_m + dx
      d20 = y2_m + dy
c	 print *,' d19=',d19,' d20=',d20, ihdr(36)
      d19 = d19 / ddlong * 60.D0 * 60.D0
      d20 = d20 / ddlat * 60.D0 * 60.D0
c	 print *,' d19=',d19,' d20=',d20, ihdr(36)
      IF( ihdr(36) .NE. 0 ) THEN
          dtemp = DFLOAT(ihdr(36))
          IF( ihdr(36) .LT. 0 ) dtemp = -1. * dtemp
          d19 = d19 * dtemp
          d20 = d20 * dtemp
c	print *,' d19=',d19,' d20=',d20,' dtemp=',dtemp
      ENDIF
      lhdr(19) = NINT(d19)
      lhdr(20) = NINT(d20)
c****
c****   Do the receiver coordinates
c****
      dtemp = DFLOAT(IABS(lhdr(10))) + setback
      dx = dtemp * DSIN(angle/rad)
      dy = dtemp * DCOS(angle/rad)
      d21 = x2_m + dx
      d22 = y2_m + dy
      d21 = d21 / ddlong * 60.D0 * 60.D0
      d22 = d22 / ddlat * 60.D0 * 60.D0
      IF( ihdr(36) .NE. 0 ) THEN
          dtemp = DFLOAT(ihdr(36))
          IF( ihdr(36) .LT. 0 ) dtemp = -1. * dtemp
          d21 = d21 * dtemp
          d22 = d22 * dtemp
      ENDIF
      lhdr(21) = NINT(d21)
      lhdr(22) = NINT(d22)
c	 print *,' lhdr=',lhdr(19),lhdr(20),lhdr(21),lhdr(22)
c****
c****
      RETURN
      END

