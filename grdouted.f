      SUBROUTINE grdouted
c
c     The edit phase of SIOSEIS process grdout, which writes a GMT
c  grd formated file.
c
c  Written by Paul Henkart, January 2001
c  Copyright (C) 2001 The Regents of the University of California
c  ALL RIGHTS RESERVED.
c
c  mod 18 Jul 03 - Use REAL*8 for gmt variables.
c  mod 13 Jan 04 - Add parameter IPAD
c  mod 22 Jan 08 - Add error when opath not given.
c  mod 24 Jan 08 - Add parameters TYPE, Y_ORDER, Z_ID, NCID, T_INDEX, NAN_VALUE,
c                                 XY_OFF
c
      PARAMETER ( npars = 28 )
      CHARACTER*80 token, title, xunits, yunits, zunits, comment,
     &             command
      CHARACTER*7 names(npars)
      DIMENSION scr(npars), lscr(npars)
      EQUIVALENCE (scr(1),lscr(1))
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /grdout/ luno, set(2), idummy, dx, lformat, lprint, xmin,
     &       xmax, ymin, ymax, zmin, zmax, xinc, yinc,
     &       zscale, xunits, yunits, zunits, title, comment, command,
     &       ipad, y_order, z_id, t_index(3)
      REAL*8 dx, xmin, xmax, ymin, ymax, zmin, zmax, xinc, yinc, zscale,
     &       dreal, xy_off, nan
      INTEGER y_order, z_id, t_index
      CHARACTER*256 name
      COMMON /grdname/ name
      DATA names/ 'OPATH ', 'LPRINT', 'SET   ','DX    ','FORMAT',
     &            'XMIN  ', 'XMAX  ', 'YMIN  ','YMAX  ','ZMIN  ',
     &            'ZMAX  ', 'XINC  ', 'YINC  ','ZSCALE','TITLE',
     &            'XUNITS', 'YUNITS', 'ZUNITS','COMMENT','COMMAND',
     &            'HDRPAD', 'TYPE  ', 'Y_ORDER','Z_ID','NCID',
     &            'T_INDEX','NAN','XY_OFF'/
      DATA nset/0/, nindex/0/
c****
c****    Set the parameter presets and various variable presets
c****
      luno = 0
      lprint = 4
      set(1) = -1.
      dx = 0
      lformat = 1
      xmin = 0.
      xmax = 0.
      ymin = 0.
      ymax = 0.
      zmin = 999999.
      zmax = 0.
      xinc = 0.
      yinc = 0.
      zscale = 1.
      xunits = 'km' // CHAR(0)
      yunits = 'secs' // CHAR(0)
      zunits = 'amplitude' // CHAR(0)
      title = ' ' // CHAR(0)
      comment = 'Processed by SIOSEIS' // CHAR(0)
      command = ' ' // CHAR(0)
      ipad = -1
      y_order = -1
      z_id = 0
      ncid = 0
      t_index(1) = 0
      t_index(2) = 0
      t_index(3) = 0
c      nan = huge(1.)
      xy_off = 0
c****
c****     get the user's parameters
c****
      ntokes = 0                                                        ! count the tokens
  100 CONTINUE
      CALL getoke( token, nchars )                                      ! get a token and it's length
      CALL upcase( token, nchars )                                      ! convert parameter names to upper case
      IF( nchars .EQ. 0 ) THEN                                          ! anything there?
          CALL rdline                                                   ! nope, get another line
          ntokes = 0
          GOTO 100
      ENDIF
  110 ntokes = ntokes + 1
      DO 200 nparam = 1, npars
         IF( token(1:nchars) .EQ. names(nparam) ) THEN                  ! find the parameter name in our list
  120        CALL getoke( token, nchars )                               ! get the value
             IF( nchars .EQ. 0 ) THEN
                 CALL rdline
                 ntokes = 0
                 GOTO 120
             ENDIF
             ntokes = ntokes + 1
             IF( names(nparam) .EQ. 'OPATH' ) THEN
                 CALL getfil( 3, luno, token, istat )
                 name = token
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'XUNITS' ) THEN
                 xunits = token // CHAR(0)
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'YUNITS' ) THEN
                 yunits = token // CHAR(0)
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'ZUNITS' ) THEN
                 zunits = token // CHAR(0)
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'TITLE' ) THEN
                 title = token // CHAR(0)
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'COMMAND' ) THEN
                 command = token // CHAR(0)
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'COMMENT' ) THEN
                 comment = token // CHAR(0)
                 GOTO 100
             ENDIF
             CALL upcase( token, nchars )
             IF( names(nparam) .EQ. 'HDRPAD' ) THEN
                 IF( token(1:1) .EQ. 'N' ) ipad = 0
                 GOTO 100
             ENDIF
             CALL ddcode( token, nchars, dreal, istat )                 ! convert the alpha number to REAL*8
             IF( istat .NE. 2 ) ierror = ierror + 1                     ! was the an error decoding it?
             IF( names(nparam) .EQ. 'DX' ) dx = dreal
             IF( names(nparam) .EQ. 'FORMAT' ) lformat = NINT(dreal)
             IF( names(nparam) .EQ. 'LPRINT' ) lprint = NINT(dreal)
             IF( names(nparam) .EQ. 'XMIN' ) xmin = dreal
             IF( names(nparam) .EQ. 'XMAX' ) xmax = dreal
             IF( names(nparam) .EQ. 'YMIN' ) ymin = dreal
             IF( names(nparam) .EQ. 'YMAX' ) ymax = dreal
             IF( names(nparam) .EQ. 'ZMIN' ) zmin = dreal
             IF( names(nparam) .EQ. 'ZMAX' ) zmax = dreal
             IF( names(nparam) .EQ. 'XINC' ) xinc = dreal
             IF( names(nparam) .EQ. 'YINC' ) yinc = dreal
             IF( names(nparam) .EQ. 'ZSCALE' ) zscale = dreal
             IF( names(nparam) .EQ. 'Y_ORDER' ) y_order = NINT(dreal)
             IF( names(nparam) .EQ. 'Z_ID') z_id = NINT(dreal)
             IF( names(nparam) .EQ. 'NCID') ncid = NINT(dreal)
             IF( names(nparam) .EQ. 'NAN') nan = dreal
             IF( names(nparam) .EQ. 'XY_OFF' ) xy_off = dreal
             IF( names(nparam) .EQ. 'SET') THEN
                 nset = nset + 1
                 set(nset) = dreal
                 IF( nset .EQ. 1 ) GOTO 120
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'T_INDEX' ) THEN
                 nindex = nindex + 1
                 t_index(nindex) = NINT(dreal)
                 IF( nindex .EQ. 3 ) GOTO 120
                 GOTO 100
             ENDIF
             nindex = 0
             nset = 0
             GOTO 100
         ENDIF
  200 CONTINUE
      IF( token(1:nchars) .NE. 'END') THEN
          PRINT *,' ***  ERROR  ***  No such parameter as ',
     *      token(1:nchars)
          ierror = ierror + 1
          GOTO 100
      ENDIF
c****
c****    finish up the parameter reading
c****
 2000 CONTINUE
      CALL getoke( token, nchars )                                      ! get the next token
      IF( nchars .LE. 0 ) THEN
          IF( now .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL rdline                                                   ! get a new line of parameters
          ntokes = 0
          GOTO 2000
      ENDIF
      CALL upcase( token, nchars )
      ntokes = ntokes + 1
      IF( token(1:nchars) .NE. 'END' .OR. nchars .NE. 3 ) GOTO 100
      IF( ipad .EQ. -1 ) THEN
          ipad = 1
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) ipad = 0
      ENDIF
      IF( luno .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  OPATH must be given.'
          ierror = ierror + 1
      ENDIF
      IF( lformat .LT. 1 .OR. lformat .GT. 2 ) THEN
          PRINT *,' ***  ERROR  ***  GRDVER must be 1 or 2.'
          ierror = ierror + 1
      ENDIF
      RETURN
      END
