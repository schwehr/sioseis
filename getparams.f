      SUBROUTINE getparams( ilun, olun, nplots, ro, lprint,
     &          header, sllun, rmargin )
c
c     SIO2HP converts up to 9 SIOSEIS plotfiles to HP-RTL (HP Raster
c  Transfer Language) used by the HP DesignJet plotters.
c
c  PARAMETER DICTIONARY
c  --------- ----------
c  IPATH - The list of up to 9 SIOSEIS plotfiles, each pathname
c          separated by a blank or new line.
c          REQUIRED.
c  OPATH - The output file pathname.
c          REQUIRED.
c  MARGIN - The length of the top margin.
c          Preset = 0.
c  COLOR - A logical switch (yes or no) indicating whether the plots
c          named in IPATH are color or not.  This is needed only on
c          color plotters (DesignJet 650C, models 2858 and 2859) when
c          plotting a black and white plot.
c  RO    - Rotate Coodrinate System.  Rotates the plotter's coordinate
c          system 90, 180, and 270 degress counterclockwise.
c          Allowable values are: 0, 90, 180, 270
c          Preset = 0.
c  header
c  slpath
c  END   - Terminate the SIO2HP parameters.
c
      CHARACTER*80 token, name, token1
      LOGICAL lprint, header
      INTEGER nplots, ierror, ro, i
      INTEGER nchars, olun, ilun(9), istat, icolor, sllun
      REAL areal, rmargin
      COMMON /sioln2/ ICHAR, NCHAR, iprint
      INTEGER ichar, nchar, iprint
      DATA name/' '/, ierror/0/

      iprint = 1
      nplots = 0
      ro = 0
      icolor = 3
      rmargin = .1
      lprint = .FALSE.
      header = .FALSE.
      sllun = 0
  100 CONTINUE
      CALL rdline
  110 CALL getoke( token, nchars )
      IF( nchars .EQ. 0 ) GOTO 100
      token1 = token
      CALL upcase( token, nchars )
      IF( token(1:5) .EQ. 'IPATH' ) THEN
          IF( nplots .EQ. 0 ) THEN
              name = token
              CALL getoke( token1, nchars )
          ENDIF
          IF( nchars .LE. 0 ) THEN
              CALL rdline
              GOTO 110
          ENDIF
  130     nplots = nplots + 1
          token1( nchars+1:nchars+1) = CHAR(0)
          CALL getfil( 4, ilun(nplots), token1, istat )                  ! open the plot file
          IF( istat .LT. 0 ) ierror = ierror + 1
          GOTO 110
      ENDIF
      IF( token(1:6) .EQ. 'SLPATH' ) THEN
          name = token
  210     CALL getoke( token1, nchars )
          IF( nchars .LE. 0 ) THEN
              CALL rdline
              GOTO 210
          ENDIF
          token1( nchars+1:nchars+1) = CHAR(0)
          CALL getfil( 2, sllun, token, istat )                         ! open the side label file
          OPEN( UNIT=sllun, FILE=token1, STATUS='OLD' )
          GOTO 110
      ENDIF
      IF( token(1:5) .EQ. 'OPATH' ) THEN
          name = token
  220     CALL getoke( token, nchars )
          IF( nchars .LE. 0 ) THEN
              CALL rdline
              GOTO 220
          ENDIF
          token( nchars+1:nchars+1) = CHAR(0)
          CALL getfil( 3, olun, token, istat )
          IF( istat .LT. 0 ) ierror = ierror + 1
          GOTO 110
      ENDIF
      IF( token(1:2) .EQ. 'RO' ) THEN
          name = token
  320     CALL getoke( token, nchars )
          IF( nchars .LE. 0 ) THEN
              CALL rdline
              GOTO 320
          ENDIF
          CALL dcode( token, nchars, areal, istat )
          ro = areal
          IF( ro .EQ. 0 .OR. ro .EQ. 90 .OR. ro .EQ. 180 .OR. 
     &        ro .EQ. 270 ) GOTO 110
          PRINT *,' ***  ERROR  ***  Illegal RO.'
          ierror = ierror + 1
          GOTO 110
      ENDIF
      IF( token(1:5) .EQ. 'COLOR' ) THEN
          name = token
  420     CALL getoke( token, nchars )
          IF( nchars .LE. 0 ) THEN
              CALL rdline
              GOTO 420
          ENDIF
          CALL upcase( token, nchars )
          IF( token(1:1) .EQ. 'N') icolor = 0
          IF( token(1:1) .EQ. 'Y') icolor = 1
          GOTO 110
      ENDIF
      IF( token(1:6) .EQ. 'LPRINT' ) THEN
          lprint = .TRUE.
          GOTO 110
      ENDIF
      IF( token(1:6) .EQ. 'HEADER' ) THEN
  430     CALL getoke( token, nchars )
          IF( nchars .LE. 0 ) THEN
              CALL rdline
              GOTO 430
          ENDIF
          CALL upcase( token, nchars )
          IF( token(1:1) .EQ. 'N') header = .FALSE.
          IF( token(1:1) .EQ. 'Y') header = .TRUE.
          GOTO 110
      ENDIF
      IF( token(1:6) .EQ. 'MARGIN' ) THEN
          name = token
  440     CALL getoke( token, nchars )
          IF( nchars .LE. 0 ) THEN
              CALL rdline
              GOTO 440
          ENDIF
          CALL dcode( token, nchars, areal, istat )
          rmargin = areal
          IF( rmargin .LT. 0 .OR. rmargin .GT. 36. ) THEN
              PRINT *,' ***  ERROR  ***  MARGIN must be >0 and < 36.'
              ierror = ierror + 1
          ENDIF
          GOTO 110
      ENDIF

      IF( name(1:5) .EQ. 'IPATH' ) THEN
          nplots = nplots + 1
          token1( nchars+1:nchars+1) = CHAR(0)
          CALL getfil( 4, ilun(nplots), token1, istat )
          IF( istat .LT. 0 ) ierror = ierror + 1
          GOTO 110
      ENDIF

      IF( token(1:3) .NE. 'END' ) THEN
          PRINT *,' ***  ERROR  ***  Illegal SIO2HP parameter: ',token
          ierror = ierror + 1
      ENDIF
      IF( lprint ) THEN
          PRINT *,' nplots= ',nplots,' ilun ', (ilun(i),i=1,nplots)
          PRINT *,' olun= ',olun,' ro= ', ro,' icolor= ',icolor
      ENDIF
      IF( ierror .GT. 0 ) STOP
      RETURN
      END
