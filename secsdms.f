      SUBROUTINE secsdms( iflag, seconds, ideg, min, sec )
c    convert from geodetic degrees/minutes/seconds to seconds of an arc.
c
c ARGUMENTS:
c iflag   >=0, converts seconds to degrees/minutes/seconds.
c         <0, converts degrees/minutes/seconds to seconds.
c seconds - the REAL number of degrees to convert
c ideg    - The integer number of degrees returned.
c min     - The integer number of minutes returned.
c sec     - The REAL number of seconds returned.
c
      INTEGER ideg, min, iflag
      REAL seconds, sec, temp


      IF( iflag .GE. 0 ) THEN
          i = 1
          temp = seconds / (60.*60.)
          IF( seconds .LT. 0 ) THEN
              i = -1
              temp = -temp
          ENDIF
          ideg = IFIX(temp)
          temp = temp - ideg
          ideg = ideg * i
          min = IFIX(temp*60.)
          temp = temp - FLOAT(min)/60.
          sec = temp * 60. * 60.
      ELSE
          temp = 1.
          itemp = ideg
          IF( ideg .LT. 0 ) THEN
              temp = -1.
              ideg = -ideg
          ENDIF
          seconds = (FLOAT(ideg)*60.*60. + FLOAT(min)*60. + sec) * temp
      ENDIF
      RETURN
      END

      SUBROUTINE dsecsdms( iflag, dseconds, ideg, min, sec )
c    convert from geodetic degrees/minutes/seconds to seconds of an arc.
c
c ARGUMENTS:
c iflag   >=0, converts seconds to degrees/minutes/seconds.
c         <0, converts degrees/minutes/seconds to seconds.
c seconds - the REAL*8 number of degrees to convert
c ideg    - The integer number of degrees returned.
c min     - The integer number of minutes returned.
c sec     - The REAL number of seconds returned.
c
      INTEGER ideg, min, iflag
      REAL*8 dseconds


      IF( iflag .GE. 0 ) THEN
          i = 1
          temp = dseconds / (60.*60.)
          IF( dseconds .LT. 0 ) THEN
              i = -1
              temp = -temp
          ENDIF
          ideg = IFIX(temp)
          temp = temp - ideg
          ideg = ideg * i
          min = IFIX(temp*60.)
          temp = temp - FLOAT(min)/60.
          sec = temp * 60. * 60.
      ELSE
          temp = 1.
          itemp = ideg
          IF( ideg .LT. 0 ) THEN
              temp = -1.
              ideg = -ideg
          ENDIF
          dseconds = (FLOAT(ideg)*60.*60. + FLOAT(min)*60. + sec) * temp
      ENDIF
      RETURN
      END

