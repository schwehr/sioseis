      SUBROUTINE cfiled( scr, lscr )
c
c                       PROCESS COFILT
c                       ------- ------
c
c  Document date: 10 August 1998
c
c
c  PARAMETER DICTIONARY
c  --------- ----------
c
c
c  Copyright (C) 1998 The Regents of the University of California
c  ALL RIGHTS RESERVED.
c
      PARAMETER ( npars = 17)                                           ! the number of user parameters
      PARAMETER ( nmulti = 14 )
      PARAMETER ( MAX_NWTS = 81 )
      CHARACTER*80 token
      CHARACTER*6 names(npars), lastname
      DIMENSION scr(111), lscr(111)
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /cofilt/ lun, nlists, nwrds, multi
      INTEGER lprint, nxpad, ftr, ltr, prestk, type, nxwin, ixsign,
     &        vsign
      REAL dx, winlen, spower, redvel, set(2), weight(MAX_NWTS),
     &     vmmi(3), ummi(3)
      DATA names / 'LPRINT', 'DX    ', 'NXPAD ', 'WINLEN', 'FTR   ',
     &             'LTR   ', 'PRESTK', 'TYPE  ', 'SPOWER', 'NXWIN ',
     &             'REDVEL', 'VSIGN ', 'XSIGN ',
     &             'SET   ', 'WEIGHT', 'VMMI  ', 'UMMI  ' /
c**** 
c****    Set the parameter presets and various variable presets
c****
      lprint = 0
      dx = 0.
      nxpad = 0
      winlen = 0.
      ftr = 0
      ltr = 999999
      prestk = 0
      type = 0
      spower = 0.
      nxwin = 0
      redvel = 0.
      vsign = 0
      ixsign = 0
      set(1) = 0.
      set(2) = 9999.
      nset = 0
      nweights = 0
      DO i = 1, 3
         vmmi(i) = 0.
         ummi(i) = 0.
      ENDDO
      nvmmi = 0
      nummi = 0
      nlists = 0
      CALL getfil( 1, lun, token, istat )                               ! get a file for the parameters
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
             IF( names(nparam) .NE. 'SET' .AND.
     &           names(nparam) .NE. 'WEIGHT' .AND.
     &           names(nparam) .NE. 'VMMI' .AND.
     &           names(nparam) .NE. 'UMMI' ) THEN
                 IF( names(nparam) .EQ. 'LPRINT' ) lprint = NINT(areal)
                 IF( names(nparam) .EQ. 'DX' ) dx = areal
                 IF( names(nparam) .EQ. 'NXPAD' ) nxpad = NINT(areal)
                 IF( names(nparam) .EQ. 'WINLEN' ) winlen = areal
                 IF( names(nparam) .EQ. 'FTR' ) ftr = NINT(areal)
                 IF( names(nparam) .EQ. 'LTR' ) ltr = NINT(areal)
                 IF( names(nparam) .EQ. 'PRESTK' ) prestk = NINT(areal)
                 IF( names(nparam) .EQ. 'TYPE' ) type = NINT(areal)
                 IF( names(nparam) .EQ. 'SPOWER' ) spower = areal
                 IF( names(nparam) .EQ. 'NXWIN' ) nxwin = NINT(areal)
                 IF( names(nparam) .EQ. 'REDVEL' ) redvel = areal
                 IF( names(nparam) .EQ. 'VSIGN' ) vsign = NINT(areal)
                 IF( names(nparam) .EQ. 'XSIGN' ) ixsign = NINT(areal)
                 lastname = names(nparam)
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'SET' ) THEN
                 nset = nset + 1
                 set(nset) = areal
             ENDIF
             IF( names(nparam) .EQ. 'WEIGHT' ) THEN
                 nweights = nweights + 1
                 weight(nweights) = areal
             ENDIF
             IF( names(nparam) .EQ. 'VMMI' ) THEN
                 nvmmi = nvmmi + 1
                 vmmi(nvmmi) = areal
             ENDIF
             IF( names(nparam) .EQ. 'UMMI' ) THEN
                 nummi = nummi + 1
                 ummi(nummi) = areal
             ENDIF
             lastname = names(nparam)
             GOTO 100
          ENDIF
  200 CONTINUE
      IF( token(1:nchars) .EQ. 'END' ) GOTO 300
      IF( lastname .NE. 'SET' .AND.
     &    lastname .NE. 'WEIGHT' .AND.
     &    lastname .NE. 'VMMI' .AND.
     &    lastname .NE. 'UMMI' ) THEN
          PRINT *,' ***  ERROR  ***  No such parameter as ',
     &          token(1:nchars)
          ierror = ierror + 1
          GOTO 100
      ENDIF
      CALL upcase( token, nchars )
      CALL dcode( token, nchars, areal, istat )
      IF( istat .NE. 2 ) ierror = ierror + 1
      IF( lastname .EQ. 'SET' ) THEN
          nset = nset + 1
          set(nset) = areal
      ENDIF
      IF( lastname .EQ. 'WEIGHT' ) THEN
          nweights = nweights + 1
          weight(nweights) = areal
      ENDIF
      IF( lastname .EQ. 'VMMI' ) THEN
          nvmmi = nvmmi + 1
          vmmi(nvmmi) = areal
      ENDIF
      IF( lastname .EQ. 'UMMI' ) THEN
          nummi = nummi + 1
          ummi(nummi) = areal
      ENDIF
      GOTO 100
c****
c****    Do some ERROR checking
c****
  300 CONTINUE
      IF( nlists .GT. 1 ) THEN
          PRINT *,' ***  ERROR  ***  Only one list permitted.'
          ierror = ierror + 1
      ENDIF
      IF( vsign .EQ. 0 .AND. redvel .NE. 0. ) THEN
          PRINT *,' ***  ERROR  ***  If Vsign=0, then REDVEL must=0.'
          ierror = ierror + 1
      ENDIF
      IF( nxwin .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  Bad NXWIN of ',nxwin
          ierror = ierror + 1
      ENDIF
c      IF(nxpad .EQ. 0 ) nxpad = nxwin / 2
      IF( nxwin .EQ. 0. ) THEN
          PRINT *,' ***  ERROR  ***  NXWIN must be given.'
          ierror = ierror + 1
      ENDIF
      IF( dx .EQ. 0. ) THEN
          PRINT *,' ***  ERROR  ***  DX must be given.'
          ierror = ierror + 1
      ENDIF
      IF( nvmmi + nummi .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  VMMI or UMMI must be given.'
          ierror = ierror + 1
      ENDIF
      IF( type .GT. 0 .AND. nweights .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  WEIGHTS not given .'
          ierror = ierror + 1
      ENDIF
      IF( nweights .GT. 0 .AND. nweights .NE. nxwin ) THEN
          PRINT *,' ***  ERROR  ***  There must be ',nxwin,' WEIGHTS.'
          ierror = ierror + 1
      ENDIF
c****
c****   Write the parameter list to disk
c****
      lscr(1) = lprint
      scr(2) = dx
      lscr(3) = nxpad
      scr(4) =  winlen
      lscr(5) = ftr
      lscr(6) = ltr
      lscr(7) = prestk
      lscr(8) = type
      scr(9) = spower
      lscr(10) = nxwin
      scr(11) = redvel
      lscr(12) = vsign
      lscr(13) = ixsign
      lscr(14) = nweights
      scr(nmulti+1) = set(1)
      scr(nmulti+2) = set(2)
      DO i = 1, MAX_NWTS
         scr(nmulti+2+i) = weight(i)
      ENDDO
      DO i = 1, 3
         scr(nmulti+2+MAX_NWTS+i) = vmmi(i)
      ENDDO
      DO i = 1, 3
         scr(nmulti+2+MAX_NWTS+3+i) = ummi(i)
      ENDDO
      nwrds = nmulti + 2 + MAX_NWTS + 3 + 3
      multi = nmulti
      nlists = nlists + 1
      CALL wrdisc( lun, scr, nwrds )
      IF( IAND(lprint,1) .NE. 0 ) THEN
          PRINT *,lscr(1),scr(2),lscr(3),scr(4),(lscr(i),i=5,8)
          PRINT *,scr(9),lscr(10),scr(11),lscr(12),lscr(13),set
          PRINT *,(weight(i),i=1,8)
          PRINT *,vmmi,ummi
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
      RETURN
      END
