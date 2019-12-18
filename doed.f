      SUBROUTINE doed( idisko, ibuf, buf, lbuf, scr, iscr, lscr )
c                                                                            
c                          PROCESS DISKOX, where X = { A, B, C, D }
c                          ------- ------
c
c   
c  Copyright (C) by The Regents of The University of California, 1988
c  Written by Paul Henkart, Scripps Institution of Oceanography, La Jolla, Ca.
c  ALL RIGHTS RESERVED.
c
c  mod 7 Jan 90 - add the Cray to the ranks of Unix!
c  mod 22 May 1990 - Allow OFMT 4 - UTIG 16 bit floating point
c  mod 19 Feb 91 - add parameter secs and decimf
c  mod 19 Jan 92 - change fno and lno to DEFAULT rather than just preset
c  mod ???? (94?) - Do not create output file unless in procs list!
c  mod 17 May 95 - Add the SU and SEGY format options.
c  29 Jan. 96 - Change lrange preset from 999999 to NONE
c  5 June 96 - Change ftr preset from 1 to 0
c  21 Aug 96 - Add parameter REWIND
c  26 Aug 96 - Add check for lno given when noinc given.
c  17 Mar 97 - Add FLINC
c  20 Jul 97 - Allow negative fno/lno.  Change lno preset to -999999
c  7 June 99 - Add parameter BIG for creating files > 2GB.
c  12 Jul 99 - Allow 10 diskox processes!
c  20 Sep 02 - Change BIG preset from NO to YES (0 to 1)
c  18 Mar 03 - Add parameter TRACE0, write trace 0 
c  1 May 03 - Add parameter EXTHDR whether to write the Rev 1 Extended
c             Textual records.  yes/no
c  mod 4 Jun 03 - Add FORMAT BINARY, no headers at all
c  mod 27 Apr 06 - gfortran chokes on internal reads that were necessary for VMS
c  mod 1 July 08 - Allow big DECIMF (up to 1000)
c  mod 18 Jul 08 - Increase opath and token to 200 characters.
c  mod 17 Jun 09 - Add RENUM and RETRAC
c  mod 12 Jun 12 - RENUM was bad. (ajh spotted)
c  mod 19 Jul 12 - FON stopped working because of 12 Jun 12 change
c  mod 29 Nov 12 - Warn that IBM output may/will not work (gcc47 on Mac fails)
c  mod 30 Jan 13 - Remove that warning because LRSHIFT was changed to zero vacated bits
c
c  *** Programmer notes:
c   idisko is set by the contoller (contro). DISKOA = 1, DISKOB = 2, etc.
c
      PARAMETER ( npars = 27 )                                          ! the number of user parameters
      PARAMETER ( maxdo = 10 )
      DIMENSION buf(111), scr(111), set(2)
      INTEGER*2 ibuf(111), iscr
      INTEGER lbuf(111), lscr(111)
      CHARACTER*200 token
      CHARACTER*6 names(npars)
      CHARACTER*1 types(npars)                                          ! the type of parameter
      REAL vals(npars)                                                  ! holds the REAL parameter values
      DIMENSION lvals(npars)                                            ! holds the INTEGER parameter values
c
      COMMON /readt/ ilun, numhdr, numdat, iunhdr, ireel, intrcs, ifmt,
     *       nskip, sec, lrenum, isrcf, idtype
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /porder/ num, iorder(40)
      COMMON /diskox/ junit(maxdo), nlists(maxdo), nwrds
c
      LOGICAL first
      SAVE first
      INTEGER posaft, ofmt, fon, ftr, trinc, fno, ontrcs, decimf, 
     &        rewind, flinc, big, renum, retrac
      REAL lrange
      CHARACTER*6 fmt
      CHARACTER*200 path
c
      EQUIVALENCE ( opath, lvals(1) ),
     2            ( lprint, lvals(2) ),
     3            ( posaft, lvals(3) ),
     4            ( ofmt, lvals(4) ),
     5            ( ftr, lvals(5) ),
     6            ( ltr, lvals(6) ),
     7            ( fno, lvals(7) ),
     8            ( lno, lvals(8) ),
     9            ( fon, lvals(9) ),
     *            ( trinc, lvals(10) ),
     1            ( noinc, lvals(11) ),
     2            ( ontrcs, lvals(12) ),
     3            ( format, lvals(13) ),
     5            ( frange, vals(15) ),
     6            ( lrange, vals(16) ),
     7            ( mass, lvals(17) ),
     8            ( secs, vals(18) ),
     9            ( decimf, lvals(19) )
      EQUIVALENCE ( spath, lvals(20) ),
     1            ( rewind, lvals(21) ),
     2            ( flinc, lvals(22) ),
     3            ( big, lvals(23) ),
     4            ( trace0, lvals(24) ),
     5            ( exthdr, lvals(25) ),
     7            ( renum, lvals(26) ),
     6            ( retrac, lvals(27) )
      DATA names /'OPATH ', 'LPRINT', 'POSAFT', 'OFMT  ', 'FTR   ',
     *            'LTR   ', 'FNO   ', 'LNO   ', 'FON   ', 'TRINC ',
     *            'NOINC ', 'ONTRCS', 'FORMAT', 'SET   ', 'FRANGE',
     *            'LRANGE', 'MASS  ', 'SECS  ', 'DECIMF', 'SPATH ',
     *            'REWIND', 'FLINC ', 'BIG   ', 'TRACE0', 'EXTHDR',
     *            'RENUM ', 'RETRAC'/
      DATA types /'A',11*'L', 'A',3*'F','L','F','L',2*'A','L',3*'A',
     &     2*'L'/
      DATA first/.TRUE./
c**** 
c****    Set the parameter presets and various variable presets
c****
      IF( first ) THEN
          first = .FALSE.
          DO i = 1, MAXDO
             nlists(i) = 0
          ENDDO
      ENDIF
      path = ' '
      lprint = 0
      posaft = 0
      ofmt = 5
      ftr = 0
      ltr = 0
      fno = -999999
      lno = -999999
      trinc = 1
      noinc = 1
      fon = 0
      ontrcs = 0
      fmt = 'SIO'
      set(1) = 0.
      set(2) = 0.
      frange = 0.
      lrange = 999999.
      mass = 1
      secs = 0.
      decimf = 1
      lunsort = 0
      iformat = 0
      rewind = 0
      flinc = 0
      big = 1
      itrace0 = 0
      iexthdr = 1
      renum = 0
      retrac = -1
      DO 10 i = 1, num
         IF( iorder(i) .GE. 42 .AND. iorder(i) .LE. 45 ) iwrite = 1
         IF( iorder(i) .GE. 75 .AND. iorder(i) .LE. 80 ) iwrite = 1
   10 CONTINUE
      CALL getfil( 1, junit(idisko), token, istat )                     ! get a file for the DISKO parameters
c****
c****     get the user's parameters -  there must be something, at least an "end"
c****
      ntokes = 0                                                        ! count the tokens
  100 CONTINUE
      ns = 0
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
             ns = ns + 1
             IF( names(nparam) .EQ. 'OPATH ' ) THEN
                 path = token(1:nchars)
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 7 .OR.
     *               icompt .EQ. 3 .OR. icompt .EQ. 5 )                 ! UNIX needs a NULL terminator
     *           path(nchars+1:nchars+1) = CHAR(0)
                 IF( icompt .EQ. 5 .AND. nchars .GT. 8 ) THEN
                     PRINT *,' ***  ERROR  ***  Cray pathnames must',
     *                      'be 8 or fewer characters long.'
                     ierror = ierror + 1
                 ENDIF
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'SPATH' ) THEN
                 CALL getfil(4, lunsort, token, istat )
                 IF( istat .NE. 0 ) THEN
                     PRINT *,' ***  ERROR  *** Could not open file',
     &                      token
                     ierror = ierror + 1
                 ENDIF
                 GOTO 100
             ENDIF
             CALL upcase( token, nchars )
             IF( types(nparam) .EQ. 'A' ) THEN
                 IF( names(nparam) .EQ. 'FORMAT' ) THEN
                     fmt = token(1:nchars)
                 ENDIF
                 IF( names(nparam) .EQ. 'REWIND' ) THEN
                     IF( token(1:nchars) .EQ. 'YES' ) rewind = 1
                     IF( token(1:nchars) .EQ. 'NO' ) rewind = 0
                     IF( token(1:nchars) .EQ. 'ON' ) rewind = 1
                     IF( token(1:nchars) .EQ. 'OFF' ) rewind = 0
                     IF( token(1:nchars) .EQ. '1' ) rewind = 1
                     IF( token(1:nchars) .EQ. '0' ) rewind = 0
                  ENDIF
                 IF( names(nparam) .EQ. 'BIG' ) THEN
                     IF( token(1:nchars) .EQ. 'YES' ) big = 1
                     IF( token(1:nchars) .EQ. 'NO' ) big = 0
                     IF( token(1:nchars) .EQ. 'ON' ) big = 1
                     IF( token(1:nchars) .EQ. 'OFF' ) big = 0
                     IF( token(1:nchars) .EQ. '1' ) big = 1
                     IF( token(1:nchars) .EQ. '0' ) big = 0
                  ENDIF
                  IF( names(nparam) .EQ. 'TRACE0' ) THEN
                     IF( token(1:nchars) .EQ. 'YES' ) itrace0 = 1
                     IF( token(1:nchars) .EQ. 'NO' ) itrace0 = 0
                  ENDIF
                  IF( names(nparam) .EQ. 'EXTHDR' ) THEN
                     IF( token(1:nchars) .EQ. 'YES' ) iexthdr = 1
                     IF( token(1:nchars) .EQ. 'NO' ) iexthdr = 0
                  ENDIF
                  GOTO 100
             ENDIF
             CALL dcode( token, nchars, areal, istat )                  ! convert the alpha number to an internal machine number
             IF( istat .NE. 2 ) ierror = ierror + 1                     ! was the an error decoding it?
             IF( names(nparam) .EQ. 'SET' ) THEN                        ! set is an array
                 set(ns) = areal
                 IF( ns .EQ. 1 ) GOTO 120
                 GOTO 100
             ENDIF
             IF( types(nparam) .EQ. 'L' ) THEN
                 lvals(nparam) = areal                                  ! convert the real to INTEGER*4
             ELSE
                 vals(nparam) = areal                                   ! move the real to the parameter
             ENDIF
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
c****    Do the parameter validity checks
c****
      IF( path .EQ. ' ' ) THEN
          PRINT *,' ***  ERROR  ***  OPATH must be given.'
          ierror = ierror + 1
      ENDIF
      IF( fno .LT. 0 .AND. fno .NE. -999999 ) THEN
          PRINT *,' ***  WARNING  ***  FNO is negative.'
          iwarn = iwarn + 1
      ENDIF
      IF( lno .LT. 0 .AND. lno .NE. -999999 ) THEN
          PRINT *,' ***  WARNING  ***  LNO is negative.'
          iwarn = iwarn + 1
      ENDIF
      IF( noinc .LT. 0 ) THEN
          PRINT *,' ***  WARNING  ***  NOINC is negative.'
          iwarn = iwarn + 1
      ENDIF
      IF( noinc .NE. 1 .AND. lno .EQ. -999999 ) THEN
          PRINT *,' ***  ERROR  ***  LNO must be given with NOINC.'
          ierror = ierror + 1
      ENDIF
      IF( ftr .LT. -999999 .OR. ftr .GT. 10000 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal FTR value of ',ftr
          ierror = ierror + 1
      ENDIF
      IF( ltr .LT. 0 .OR. ltr .GT. 10000 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal LTR value of ',ltr
          ierror = ierror + 1
      ENDIF
      IF( trinc .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  TRINC must be positive.'
          ierror = ierror + 1
      ENDIF
      IF( ofmt .LT. 1 .OR. ofmt .GT. 9 ) THEN
          PRINT *,' ***  ERROR  ***  OFMT must be 1, 2, 3, 4, or 5'
          ierror = ierror + 1
      ENDIF
      IF( ofmt .EQ. 1 ) THEN
          PRINT *,' **  WARNING  ** Old/obsolete IBM format requested.'
          PRINT *,' ***  USE OFMT 5 - IEEE output is valid SEG-Y.'
          PRINT *,' ***  IBM floating point was last used ~1990.'
          iwarn = iwarn + 1
      ENDIF
      IF( ontrcs .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  ONTRCS must be positive.'
          ierror = ierror + 1
      ENDIF
      IF( ontrcs .GT. 0 .AND. retrac .EQ. -1 ) retrac = 1
      IF( ontrcs .GT. 0 .AND. fon .LE. 0 ) fon = 1
      IF( fmt .NE. 'SEGY' .AND. fmt .NE. 'SIO' .AND. 
     &    fmt .NE. 'SU' .AND. fmt .NE. 'BINARY' ) THEN
          PRINT *,' ***  ERROR  ***  FORMAT must be SU or BINARY.'
          ierror = ierror + 1
      ENDIF
      IF( fmt .EQ. 'SIO' ) iformat = 1
      IF( fmt .EQ. 'SEGY' ) THEN
          iformat = 1
          ofmt = 1
      ENDIF
      IF( fmt .EQ. 'SU' ) THEN
          iformat = 3
          ofmt = 5
      ENDIF
      IF( fmt .EQ. 'BINARY' ) THEN
          iformat = 4
          ofmt = 5
      ENDIF
      IF( set(1) .LT. 0. .OR. set(1) .GT. 100 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal SET value of ',set(1)
          ierror = ierror + 1
      ENDIF
      IF( set(2) .LT. 0. .OR. set(2) .GT. 100 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal SET value of ',set(1)
          ierror = ierror + 1
      ENDIF
      IF( mass .LT. 0 .OR. mass .GT. 3 ) THEN
          PRINT *,' ***  ERROR  ***  MASS must be 0, 1, 2, or 3.'
          ierror = ierror + 1
      ENDIF
      IF( decimf .LE. 0 .OR. decimf .GT. 1000 ) THEN
          PRINT *,' ***  ERROR  ***  DECIMF must be between 1 and 1000.'
          ierror = ierror + 1
      ENDIF
      IF( secs .LT. 0. .OR. secs .GT. 100 ) THEN
          PRINT *,' ***  ERROR  ***  SECS must be positive and < 100.'
          ierror = ierror + 1
      ENDIF
      IF( secs .GT. 0. .AND. set(2) .GT. 0. ) THEN
          PRINT *,' ***  ERROR  ***  SECS and SET are not compatible.'
          ierror = ierror + 1
      ENDIF
      IF( flinc .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  FLINC must be positive.'
          ierror = ierror + 1
      ENDIF
c
      IF( iwrite .EQ. 0 ) GOTO 2000
c****
c****    Write the DISKO parameters to a disc file  and get another list!
c****
      lscr(26) = fno
      lscr(27) = lno
      lscr(28) = noinc
      lscr(29) = ftr
      lscr(30) = ltr
      lscr(31) = trinc
      lscr(32) = fon
c****  renum is the same as FON, and has precedence
      IF( renum .NE. 0 ) lscr(32) = renum
      lscr(33) = ofmt
      lscr(34) = ontrcs
      lscr(35) = posaft
      lscr(36) = iformat
      lscr(37) = lprint
      scr(38) = set(1)
      scr(39) = set(2)
      scr(40) = frange
      scr(41) = lrange
      lscr(42) = mass
      scr(43) = secs
      lscr(44) = decimf
      lscr(45) = lunsort
      lscr(46) = rewind
      lscr(47) = flinc
      lscr(48) = big
      lscr(49) = itrace0
      lscr(50) = iexthdr
      lscr(51) = retrac
      nwrds = 51
      CALL wrdiscb( junit(idisko), path, 200 )
      CALL wrdisc( junit(idisko), lscr(26), nwrds-25 )
      nlists(idisko) = nlists(idisko) + 1
c
      IF( IAND(lprint,1) .NE. 0 ) THEN
          PRINT *,' idisko=',idisko
          PRINT *,path
          PRINT *,(lscr(i),i = 26,37)
          PRINT *,scr(38),scr(39),(scr(i),i=40,41)
          PRINT *,lscr(42), scr(43), (lscr(i),i=44,51)
      ENDIF
c****
c****     set the DEFAULTS
c****
      posaft = 0
      fno = -999999
      lno = -999999
c****
c****    finish up the parameter reading
c****
 2000 CONTINUE
      CALL getoke( token, nchars )                                       ! get the next token
      CALL upcase( token, nchars )
      ntokes = ntokes + 1
      IF( nchars .LE. 0 ) THEN
          IF( now .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL rdline                                                          ! get a new line of parameters
          ntokes = 0
          GOTO 2000
      ENDIF
      IF( token(1:nchars) .NE. 'END' .OR. nchars .NE. 3 ) GOTO 110

      RETURN
      END
