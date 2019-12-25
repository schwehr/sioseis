      SUBROUTINE xstared
c     Process XSTAR converts EdgeTech's XSTAR chirp data into a
c  more useful format.  The enhancements are:
c  0)  Look for pings with only 1 ducer (trace).
c  1)  Perform a complex modulus to convert to amplitude.
c  2)  Change the sample interval and data length to account
c      for the modulus.  (the sample interval is 83 us)
c  3)  Perform a stack of both traces in each shot.
c  4)  Make the shot number appear in the correct SEG-Y location.
c  5)  Make the shot trace number 1.
c  6)  Use the NMEA time for the shot time.
c
c Copyright (C), The Regents of the University of California
c Written by Paul Henkart, Scripps Institution of Oceanography, 2001
c
c mod Dec 2002 - Add parameters ftr, ltr, type
c         type 0 = gstar, 1 = 1 trace xstar, 2 = 2 trace xstar
c                Add parameter weights
c mod 15 Feb 03 - Weights was preset 1 2 rather than 1 1
c               - ltr didn't work
c mod 19 Feb 03 - Add parameter DUMMIES (preset = yes)
c mod 25 Mar 03 - Allow type 3 for the Triton 3 trace data.
c mod 1 Dec  04 - Add parameter MKREAL
c mod 11 Jan 05 - Change dummies into 0, 1, 2 from yes/no and add 2 to
c                 mean use the ping if 1 of the ducers is missing.
c mod 25 Aug 05 - Add type 5 - Xstar version 5
c mod 30 Jun 06 - Allow type 4 (unsummed output).
c
      PARAMETER ( npars = 12 )
      CHARACTER*7 names(npars)
      CHARACTER*80 token
      DIMENSION scr(npars), lscr(npars)
      EQUIVALENCE (scr(1),lscr(1))
      INTEGER ftr, type, dummies
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /xstar/ lunxyz, deltad, lprint, depth, wireout, lunbin,
     &               ftr, ltr, type, weights(2), dummies, mkreal
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, ntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr,
     *               nrskip, nfiles
      DATA names/ 'XYZPATH', 'DELTAD ', 'LPRINT ', 'DEPTH  ' ,'WIREOUT',
     &            'BINPATH', 'FTR    ', 'LTR    ', 'TYPE   ', 'WEIGHTS',
     &            'DUMMIES', 'MKREAL'/

      lunxyz = 0
      deltad = .01
      lprint = 0
      depth = 0.
      wireout = 0.
      lunbin = 0
      ftr = -1
      ltr = -1
      type = -1
      nw = 0
      weights(1) = 1.
      weights(2) = 1.
      dummies = 1
      mkreal = 0
c****
c****     get the user's parameters
c****
      ntokes = 0
  100 CONTINUE
      CALL getoke( token, nchars )
      CALL upcase( token, nchars )
      IF( nchars .EQ. 0 ) THEN
          CALL rdline
          ntokes = 0
          GOTO 100
      ENDIF
  110 ntokes = ntokes + 1
      DO 200 nparam = 1, npars
         IF( token(1:nchars) .EQ. names(nparam) ) THEN
  120        CALL getoke( token, nchars )
             IF( nchars .EQ. 0 ) THEN
                 CALL rdline
                 ntokes = 0
                 GOTO 120
             ENDIF
             ntokes = ntokes + 1
             IF( names(nparam) .EQ. 'XYZPATH' ) THEN
                 CALL getfil( 2, lunxyz, token, istat )
                 OPEN( UNIT=lunxyz, FILE=token, STATUS='OLD', ERR=130 )
                 GOTO 100
  130            PRINT *,' ***  ERROR  ***  Can not find file ',token
                 STOP
             ENDIF
             IF( names(nparam) .EQ. 'BINPATH' ) THEN
                 CALL getfil( 4, lunbin, token, istat )
                 IF( istat .NE. 0 ) THEN
                     PRINT *,' ***  ERROR  ***  Could not open file:',
     &                   token
                     ierror = ierror + 1
                 ENDIF
                 GOTO 100
             ENDIF
             CALL upcase( token, nchars )
             IF( names(nparam) .EQ. 'TYPE' ) THEN
                 IF( token(1:5) .EQ. 'GSTAR' ) THEN
                     type = 0
                 ELSE
                     CALL dcode( token, nchars, areal, istat )
                     type = NINT(areal)
                 ENDIF
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'DUMMIES' ) THEN
                 IF( token(1:2) .EQ. 'NO' ) THEN
                     dummies = 0
                     GOTO 100
                 ENDIF
                 IF( token(1:2) .EQ. 'YES' ) GOTO 100
                 CALL dcode( token, nchars, areal, istat )
                 dummies = NINT(areal)
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'MKREAL' ) THEN
                 IF( token(1:3) .EQ. 'YES' ) mkreal = 1
                 GOTO 100
             ENDIF
             CALL dcode( token, nchars, areal, istat )
             IF( istat .NE. 2 ) ierror = ierror + 1
             IF( names(nparam) .EQ. 'DELTAD' ) deltad = areal
             IF( names(nparam) .EQ. 'LPRINT' ) lprint = NINT(areal)
             IF( names(nparam) .EQ. 'DEPTH' ) depth = areal
             IF( names(nparam) .EQ. 'WIREOUT' ) wireout = areal
             IF( names(nparam) .EQ. 'FTR' ) ftr = NINT(areal)
             IF( names(nparam) .EQ. 'LTR' ) ltr = NINT(areal)
             IF( names(nparam) .EQ. 'WEIGHTS' ) THEN
                 nw = nw + 1
                 weights(nw) = areal
             ENDIF
             GOTO 100
         ENDIF
c****    the token is not in the parameter list.  Might be weights.
         IF( names(nparam) .EQ. 'WEIGHTS' .AND. nw .NE. 0 .AND.
     &       token(1:3) .NE. 'END' ) THEN
             nw = nw + 1
             CALL dcode( token, nchars, weights(nw), istat )
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
      CALL getoke( token, nchars )
! get the next token
      IF( nchars .LE. 0 ) THEN
          IF( now .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL rdline
! get a new line of parameters
          ntokes = 0
          GOTO 2000
      ENDIF
      CALL upcase( token, nchars )
      ntokes = ntokes + 1
      IF( token(1:nchars) .NE. 'END' .OR. nchars .NE. 3 ) GOTO 100
c****
c****  Do error checking and presets
c****
      IF( type .LT. 0 .OR. type .GT. 5 ) THEN
          PRINT *,' ***  ERROR  ***  TYPE of Edgetech is REQUIRED.'
          PRINT *,' 0, 1, 2, 3, 4, 5 are permissible.'
          ierror = ierror + 1
      ENDIF
      ntrcs = 1
      IF( type .EQ. 2 .OR. type .EQ. 4 ) ntrcs = 2
      IF( ftr .LT. 0 ) ftr = 1
      IF( ltr .LT. 0 ) ltr = ntrcs
      RETURN
      END
