      SUBROUTINE xcored( buf, lbuf, ibuf, scr1, lscr1, iscr1 )
c
c                       PROCESS CORR
c                       ------- ----
c
c  Document date: 
c
c
c  PARAMETER DICTIONARY
c  --------- ----------
c
c
c  Written by Paul Henkart, Scripps Institution of Oceanography, August 1998
c  Copyright (C) 1998 The Regents of the University of California
c  ALL RIGHTS RESERVED.
c
c  mod 5 Apr 99 - Add parameter CATS
c  mod 8 June 00 - 32 bit integer pilot was wrong (thanks to g77)
c  mod 1 Dec 08 - ppath didn't work on Intel
c
      PARAMETER ( nmulti = 0 )
      PARAMETER ( npars = 10)                                           ! the number of user parameters
      CHARACTER*80 token
      CHARACTER*6 names(npars)
      DIMENSION buf(111), lbuf(111), ibuf(111),
     &         scr1(111), lscr1(111), iscr1(111)
      INTEGER*2 ibuf, iscr1
      DIMENSION scr(npars+2), lscr(npars+2)
      EQUIVALENCE (scr(1),lscr(1))
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /readt/ ilun, numhdr, nsamps, ihunit, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr,
     *               nrskip, nfiles

      COMMON /corr/ lun, nlists, nwrds
      INTEGER lprint, pilot, psno, ptr
      DIMENSION setp(2), setd(2)
      DATA names / 'LPRINT', 'PILOT ', 'NLAGS ', 'SETP  ', 'SETD  ',
     &             'PPATH ', 'PSNO  ', 'PTR   ', 'DOUBLE', 'CATS  '/
c**** 
c****    Set the parameter presets and various variable presets
c****
      lprint = 0
      nlists = 0
      pilot = 1
      nlags = 0
      setp(1) = 0.
      setp(2) = 0.
      setd(1) = 0.
      setd(2) = 0.
      psno = 0
      ptr = 0
      idouble = 0
      lunpilot = 0
      icats = 0
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
             IF( names(nparam) .EQ. 'PPATH' ) THEN
                 CALL getoke( token, nchars )
                 CALL getfil( 4, lunpilot, token, istat )
                 pilot = 0                                              ! change the preset from pilot trace 1
                 GOTO 100
             ENDIF
  120        CALL getoke( token, nchars )                               ! get the value
             IF( nchars .EQ. 0 ) THEN
                 CALL rdline
                 ntokes = 0
                 GOTO 120
             ENDIF
             ntokes = ntokes + 1
             CALL upcase( token, nchars )
             IF( names(nparam) .EQ. 'DOUBLE' ) THEN
                 IF( token(1:1) .EQ. 'Y' ) idouble = 1
                 IF( token .EQ. 'ON' ) idouble = 1
                 IF( token .EQ. '1' ) idouble = 1
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'CATS' ) THEN
                 IF( token(1:1) .EQ. 'Y' ) icats = 1
                 IF( token .EQ. 'ON' ) icats = 1
                 IF( token .EQ. '1' ) icats = 1
                 GOTO 100
             ENDIF
             CALL dcode( token, nchars, areal, istat )                  ! convert the alpha number to an internal machine number
             IF( istat .NE. 2 ) ierror = ierror + 1                     ! was the an error decoding it?
             IF( names(nparam) .EQ. 'LPRINT' ) lprint = NINT(areal)
             IF( names(nparam) .EQ. 'PILOT' ) pilot = NINT(areal)
             IF( names(nparam) .EQ. 'NLAGS' ) nlags = NINT(areal)
             IF( names(nparam) .EQ. 'PSNO' ) psno = NINT(areal)
             IF( names(nparam) .EQ. 'PTR' ) ptr = NINT(areal)
             IF( names(nparam) .EQ. 'SETP' ) THEN
                 setp(1) = areal
                 CALL getoke( token, nchars )
                 CALL dcode( token, nchars, setp(2), istat )
             ENDIF
             IF( names(nparam) .EQ. 'SETD' ) THEN
                 setd(1) = areal
                 CALL getoke( token, nchars )
                 CALL dcode( token, nchars, setd(2), istat )
             ENDIF
             GOTO 100
          ENDIF
  200 CONTINUE
      IF( token(1:nchars) .NE. 'END' ) THEN
          PRINT *,' ***  ERROR  ***  No such parameter as ',
     &          token(1:nchars)
          ierror = ierror + 1
          GOTO 100
      ENDIF
c****
c****    Do some ERROR checking
c****
  300 CONTINUE
      IF( (pilot .LT. 1 .OR. pilot .GT. 2000) .AND. psno .EQ. 0 ) THEN
          PRINT *,' ***  WARNING  ***  PILOT ',pilot,' may be wrong.',
     &       pilot,psno
          iwarn = iwarn + 1
      ENDIF
c****
c****  Now write the parameters to disk
c****
      lscr(1) = lprint
      lscr(2) = pilot
      lscr(3) = nlags
      scr(4) = setp(1)
      scr(5) = setp(2)
      scr(6) = setd(1)
      scr(7) = setd(2)
      lscr(8) = lunpilot
      lscr(9) = psno
      lscr(10) = ptr
      lscr(11) = idouble
      lscr(12) = icats
      nwrds = npars + 2
      CALL wrdisc( lun, scr, nwrds )
      nlists = nlists + 1
      IF( IAND(lprint,1) .NE. 0 ) THEN
          PRINT *,(lscr(i),i=1,3),(scr(i),i=4,7)
          PRINT *, lscr(8),lscr(9),lscr(10), lscr(11), lscr(12)
      ENDIF
c****
c****    finish up the parameter reading
c****
 2000 CONTINUE
      CALL getoke( token, nchars )
      IF( nchars .LE. 0 ) THEN
          IF( now .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL rdline
          ntokes = 0
          GOTO 2000
      ENDIF
      CALL upcase( token, nchars )
      ntokes = ntokes + 1
      IF( token(1:nchars) .NE. 'END' .OR. nchars .NE. 3 ) GOTO 110
c****
c****  read pilot trace if it's in another file
c****
      IF( lunpilot .NE. 0 ) THEN
          CALL podiscb( lunpilot, 0, 3200 )
          CALL rddiscb( lunpilot, iscr1, 400, istat )
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap16(iscr1,200)
          ifmt_pilot = iscr1(13)
          IF( ifmt_pilot .NE. 2 .AND. ifmt_pilot .NE. 1 .AND.
     &        ifmt_pilot .NE. 5 ) THEN
              PRINT *,' ***  ERROR  ***  PPATH must be SEG-Y formats ',
     &              ' 32 bit integer, IBM or IEEE floating point.'
              ierror = ierror + 1
          ENDIF
          IF( psno .EQ. 0 .OR. ptr .EQ. 0 ) THEN
              PRINT *,' ***  ERROR  ***  PSNO and PTR must be given.'
              ierror = ierror + 1
          ENDIF
          IF( ierror .EQ. 0 ) THEN
 3000         CONTINUE
              CALL rddiscb( lunpilot, lscr1, 240, istat )
              IF( istat .NE. 240 ) THEN
                  PRINT *,' ***  ERROR  ***  Could not find the pilot.'
                  ierror = ierror + 1
                  RETURN
              ENDIF
              IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     &            CALL swap16( iscr1(58), 1 )
              nsamps = iscr1(58)
              CALL rddisc( lunpilot, lscr1(numhdr+1), nsamps, istat )
              IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     &            CALL swap32( scr1, 4 )
              IF( lscr1(3) .NE. psno .OR. lscr1(4) .NE. ptr ) GOTO 3000
              DO i = 1, nsamps
                 scr1(i) = scr1(numhdr+i)
              ENDDO
c****   assume no 16 bit integers!
              IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     &            CALL swap32( scr1, nsamps )
              IF( ifmt_pilot .EQ. 1 ) CALL ibm2fp( scr1, nsamps, scr1 )
              IF( ifmt_pilot .EQ. 2 ) THEN                              ! 32 bit integer
                  DO i = 1, nsamps
                     scr1(i) = FLOAT(lscr1(i))
                  ENDDO
              ENDIF
              CALL frefil( 2, lunpilot, istat )                         ! close and release the user's file
              CALL getfil( 1, lunpilot, token, istat )                  ! get a temporary file
              CALL podisc( lunpilot, 0, 0 )
              CALL wrdisc( lunpilot, scr1, nsamps )
           ENDIF
      ENDIF
c****
c****
c****
      RETURN
      END
