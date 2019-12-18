      PROGRAM segd2disk
c    Usage:     segd2disk device-name output-directory'
c   Read a SEGD tape and dump it to disk, making a new disk file
c  for each "SEGD file" or shot.
c    Make sure to use Berkeley file mark handling.  Use the b and n
c  options.  n = no rewind,  b = Berkeley
c
      INTEGER maxwrds
      PARAMETER (maxwrds = 65535)
      CHARACTER*80 filename, token, dirname
      INTEGER lunin, lunout, istat, nargs, iargc,
     &        nbytes, fileno
      DIMENSION lbuf(33000)
      DATA lunin/0/, lunout/10/, fileno/1/
c
      nargs = iargc()
      IF( nargs .LT. 2 ) THEN
          PRINT *,' Incorrect segd2disk usage.'
          PRINT *,' Usage:     segd2disk device-name output-directory'
          STOP
      ENDIF
      CALL getarg( 1, token )
      CALL ASTAPE( lunin, token, istat )
c      CALL magtap( lunin, lbuf, maxwrds, 30, istat )                    ! rewin
      CALL getarg( 2, dirname)
      CALL bldgname( dirname,fileno, filename )
      CALL getfil( 3, lunout, filename, jstat )
   10 CONTINUE
      CALL magtap( lunin, lbuf, maxwrds, 21, istat )
      IF( istat .EQ. -1 .AND. nbytes .GE. 0 ) THEN
          CALL frefil( 2, lunout, istat )
          CALL magtap( lunin, lbuf, maxwrds, 21, istat )
          IF( istat .EQ. -1 ) THEN
              PRINT *,' Detected double EOF.  Last file was ',filename
              STOP
          ENDIF
          fileno = fileno + 1
          CALL bldgname( dirname,fileno, filename )
          CALL getfil( 3, lunout, filename, jstat )
      ENDIF
      nbytes = istat * 2
      CALL wrdiscb( lunout, lbuf, nbytes )
      GOTO 10
      END

