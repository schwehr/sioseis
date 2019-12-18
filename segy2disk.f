      PROGRAM segy2disk
c   reads an SEGY tape and dumps it to disk, making a new disk file
c  for each "SEGY file".
c    Make sure to use Berkeley file mark handling.  Use the b and n
c  options.  n = no rewind,  b = Berkeley
c
      INTEGER maxwrds
      PARAMETER (maxwrds = 20000)
      CHARACTER*80 filename, token
      INTEGER lunin, lunout, istat, nargs, iargc,
     &        nbytes, lhead1, lhead2, lbuf, day, hour,
     &        min, ndone
      INTEGER*2 ibuf
      REAL areal
      DIMENSION lbuf(10000), lhead1(800), lhead2(100), ibuf(1)
      EQUIVALENCE (lbuf(1),ibuf(1))
      DATA lunin/0/, lunout/10/, ndone/0/, nbytes/0/
c
      nargs = iargc()
      IF( nargs .LT. 1 ) THEN
          PRINT *,' Incorrect segy2disk usage.'
          PRINT *,' Usage:     segy2disk device-name'
          STOP
      ENDIF
      CALL getarg( 1, token )
c      CALL dcode( token, 1, areal, istat )
c      lunin = NINT(areal)
      CALL ASTAPE( lunin, token, istat )
c      CALL magtap( lunin, lbuf, maxwrds, 30, istat )                    ! rewind the tape
    1 CALL magtap( lunin, lhead1, 1600, 21, istat )
      CALL magtap( lunin, lhead2, 200, 21, istat )
   10 CALL magtap( lunin, lbuf, maxwrds, 21, istat )
      IF( istat .EQ. -1 .AND. nbytes .GE. 0 ) THEN
c****     read the first record after the EOF
c****     SGI gives bad status if you ask for more bytes than are in
c****     the array, even if the tape record is smaller than the array.
          CALL magtap( lunin, lhead1, 1600, 21, istat )
          IF( istat .EQ. -1 .OR. istat .EQ. 0 ) THEN
              CALL magtap( lunin, lbuf, maxwrds, 10, istat )
              STOP
          ENDIF
          IF( istat .NE. 1600 ) THEN
              CALL magtap( lunin, lhead1, maxwrds, 25, istat )          ! skip back 1 record
          ELSE
c****        read the binary header
              CALL magtap( lunin, lhead2, 200, 21, istat )
          ENDIF
c****     read the first trace
          CALL magtap( lunin, lbuf, maxwrds, 21, istat )
          ndone = 0
          CALL frefil( 2, lunout, istat )
      ENDIF
      nbytes = istat * 2
      IF( ndone .EQ. 0 ) THEN
          day = ibuf(80)
          hour = ibuf(81)
          min = ibuf(82)
        WRITE(filename,'(3hday,I3.3,1H-,I2.2,I2.2,6Hz.segy)')
     &      day,hour,min
c          WRITE( filename, 100 ) day, hour, min
c  100     FORMAT( 'JD', I3.3,'-',I2.2,I2.2,'z' )
          CALL getfil( 3, lunout, filename, jstat )
          IF( jstat .NE. 0 ) lunout = -1
          IF( lunout .GT. 0 ) PRINT *,filename
          IF( lunout .GT. 0 ) CALL wrdiscb( lunout, lhead1, 3200 )
          IF( lunout .GT. 0 ) CALL wrdiscb( lunout, lhead2, 400 )
      ENDIF
      IF( ibuf(58) .NE. (nbytes-240)/4 ) THEN
          PRINT *,' shot ',lbuf(3),' trace ',lbuf(4),
     &    ' had header nsamps = ',ibuf(58),' but only ',(nbytes-240)/4,
     &    ' read'
          nbytes = nbytes / 4 * 4
          ibuf(58) = (nbytes-240)/4
          ibuf(15) = 2                                                  ! kill it and zero fill
c****     zero fill because there seems to be garbage in the trace
          IF( nbytes .GT. 240 ) THEN
              DO 120, i = 1, ibuf(58)
  120         lbuf(60+i) = 0.
          ENDIF
      ENDIF
      IF( nbytes .GT. 240 .AND. lunout .GT. 0 )
     *    CALL wrdiscb( lunout, lbuf, nbytes )
      ndone = ndone + 1
      GOTO 10
      END
