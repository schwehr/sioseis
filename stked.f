      SUBROUTINE stked( scr, lscr )
c
c                       PROCESS STK
c                       ------- ---
c
c
c
c  Copyright (C) 1999 The Regents of the University of California
c  ALL RIGHTS RESERVED.
c
c  mod 11 Mar 03 - Add parameter PANEL
c  mod 5 Jan 10 - Make new parameter TRIMOUT the same as TRIM
c
      PARAMETER ( npars = 6)                                            ! the number of user parameters
      PARAMETER ( multiv = npars )
      CHARACTER*80 token
      CHARACTER*7 names(npars), lastname
      DIMENSION scr(111), lscr(111)
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /trim/ lunparams, nlists, nwrds, luntrim
      INTEGER lprint
      DATA names / 'LPRINT', 'TRIM  ', 'TRIMIN','TRIMOUT','PANEL  ',
     &       'XXT'/
c**** 
c****    Set the parameter presets and various variable presets
c****
      lprint = 0
      savein = 0.
      saveout = 0.
      nxxts = 0
      panel = 0.
      nlists = 0
      CALL getfil( 1, lunparams, token, istat )                          ! get a file for the parameters
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
             CALL upcase( token, nchars )
             CALL dcode( token, nchars, areal, istat )                  ! convert the alpha number to an internal machine number
             IF( istat .NE. 2 ) ierror = ierror + 1                     ! was the an error decoding it?
             IF( names(nparam) .EQ. 'LPRINT' ) lprint = NINT(areal)
             IF( names(nparam) .EQ. 'TRIM' ) savein = areal
             IF( names(nparam) .EQ. 'TRIMOUT' ) savein = areal
             IF( names(nparam) .EQ. 'TRIMIN' ) saveout = areal
             IF( names(nparam) .EQ. 'PANEL' ) panel = areal
             IF( names(nparam) .EQ. 'XXT' ) THEN
                 nxxts = nxxts + 1
                 scr(npars) = nxxts
                 scr(npars+nxxts) = ABS(areal)
             ENDIF 
             lastname = names(nparam)
             GOTO 100
          ENDIF
  200 CONTINUE
      IF( lastname .EQ. 'XXT' .AND. token(1:nchars) .NE. 'END' ) THEN
          CALL upcase( token, nchars )
          CALL dcode( token, nchars, areal, istat )
          nxxts = nxxts + 1
          scr(npars) = nxxts
          scr(npars+nxxts) = ABS(areal)
          GOTO 100
      ENDIF    
      IF( token(1:nchars) .NE. 'END' ) THEN
          PRINT *,' ***  ERROR  ***  Unknown STK parameter named ',
     &      token(1:nchars)
          ierror = ierror + 1
      ENDIF
c****
c****    Do some ERROR checking
c****
      IF( savein+saveout+nxxts+panel .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  No parameters given for STK.'
          ierror = ierror + 1
      ENDIF
      IF( savein .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  TRIM must be between 0 and 100.'
          ierror = ierror + 1
      ENDIF
      IF( saveout .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  TRIMIN must be between 0 and 100.'
          ierror = ierror + 1
      ENDIF
      IF( nxxts .GT. 90 ) THEN
          PRINT *,' ***  ERROR  ***  Too many XXTs.  90 max.'
          ierror = ierror + 1
      ENDIF
      IF( mod(nxxts,3) .NE. 0 ) THEN
          PRINT *,' ***  ERROR  ***  XXT must be triples.'
          ierror = ierror + 1
      ENDIF
      DO i = 1, nxxts, 3
         IF( scr(npars+i) .LT. 0 .OR. scr(npars+i+1) .LT. 0 ) THEN
             PRINT *,' ***  ERROR  ***  XXT ranges must be positive.'
             ierror = ierror + 1
         ENDIF
      ENDDO
c****
c****   Write the parameter list to disk
c****
      lscr(1) = lprint
      scr(2) = savein
      scr(3) = saveout
      scr(4) = panel
      lscr(multiv) = nxxts
      nwrds = npars + nxxts
      nlists = nlists + 1
      CALL wrdisc( lunparams, scr, nwrds )
      IF( IAND(lprint,1) .NE. 0 ) THEN
          PRINT *,lscr(1),scr(2),scr(3),lscr(4)
          IF(nxxts .NE. 0 ) PRINT *,(scr(i),i=npars+1,nwrds)
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
      CALL getfil( 1, luntrim, token, istat )                           ! get a file to hold the data
      RETURN
      END
