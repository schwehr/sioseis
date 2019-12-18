      SUBROUTINE swelled( lscr, scr )
c
c  Copyright (C) 2009, The Regents of the University of California
c  ALL RIGHTS RESERVED.
c
c  Written by Paul Henkart
c  mod 6 Nov 09 - remove ihdr & lhdr
c               - Add INDEX
c
      PARAMETER ( npars = 5 )
      CHARACTER*7 names(npars)
      DIMENSION scr(1111), lscr(1111)
      CHARACTER*20 token
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /swell/ lun_params, nparams
      INTEGER hdr
      DATA names / 'N', 'WEIGHTS', 'LPRINT', 'HDR', 'INDEX'/
      DATA n/0/, nweights/0/, hdr/0/, index/0/
c****
c****    Set the parameter presets and various variable presets
c****
      CALL getfil( 1, lun_params, token, istat )
c****
c****     get the user's parameters
c****
      nparams = npars
      nlists = 0
      ntokes = 0                                                        ! count the tokens
      DO i = 1, npars
         lscr(i) = 0
      ENDDO
  100 CONTINUE
      CALL getoke( token, nchars )                                      ! get a token and it's length
  101 CALL upcase( token, nchars )                                      ! convert parameter names to upper case
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
             CALL upcase( token, nchars )
             CALL dcode( token, nchars, areal, istat )                  ! convert the alpha number to an internal machine number
             IF( istat .NE. 2 ) ierror = ierror + 1                     ! was the an error decoding it?
             IF( names(nparam) .EQ. 'N' ) n = areal
             IF( names(nparam) .EQ. 'LPRINT' ) lprint = areal
             IF( names(nparam) .EQ. 'INDEX' ) index = NINT(areal)
             IF( names(nparam) .EQ. 'HDR' ) hdr = areal
             IF( names(nparam) .EQ. 'WEIGHTS') THEN
                 nweights = nweights + 1
                 scr(npars+nweights) = areal
             ENDIF
             GOTO 100
         ENDIF
  200 CONTINUE
      IF( token(1:nchars) .NE. 'END' .AND. nweights .NE. 0 ) THEN
          CALL dcode( token, nchars, areal, istat )
          IF( istat .NE. 2 ) ierror = ierror + 1
          nweights = nweights + 1
          scr(npars+nweights) = areal
          GOTO 100
      ENDIF
      IF( token(1:nchars) .NE. 'END' ) THEN
          PRINT *,' ***  ERROR  ***  No such parameter as ',
     *      token(1:nchars)
          ierror = ierror + 1
          GOTO 100
      ENDIF
c****
c****    Do some ERROR checking
c****
      IF( nlists .GT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  Only one SWELL list is permitted.'
          ierror = ierror + 1
      ENDIF
c****
c****   Write the parameter list to disk
c****
      IF( nweights .EQ. 0 ) THEN
          DO i = 1, n
             scr(npars+i) = 1.
          ENDDO
      ELSE
          n = nweights
      ENDIF
      lscr(1) = n
      lscr(2) = lprint
      lscr(3) = index
      lscr(4) = hdr
      CALL wrdisc( lun_params, scr, npars+nweights )
      IF( IAND(lprint,1) .NE. 0 ) THEN
          PRINT *,(lscr(i),i=1,npars-1)
          PRINT *,(scr(i),i=npars+1,npars+n)
      ENDIF
      nlists = nlists + 1
c****
c****    finish up the parameter reading
c****
 2000 CONTINUE
      CALL getoke( token, nchars )                                       ! get the next token
      IF( nchars .LE. 0 ) THEN
          IF( now .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL rdline                                                          ! get a new line of parameters
          ntokes = 0
          GOTO 2000
      ENDIF
      CALL upcase( token, nchars )
      ntokes = ntokes + 1
      IF( token(1:nchars) .NE. 'END' .OR. nchars .NE. 3 ) GOTO 101
      RETURN
      END
