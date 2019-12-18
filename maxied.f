      SUBROUTINE maxied( buf, ibuf, lbuf )
c
c                       PROCESS MAXIN
c                       ------- -----
c
c  Document date: 3 July 1992
c
c      Process MAXIN reads MicroMAX (TM) disk files.  The MicroMAX files
c  must have been created on a Compaq MicroVAX because SIOSEIS assumes a
c  data format of IEEE floating point in Compaq byte order (little endian).
c
c
c  PARAMETER DICTIONARY
c  --------- ----------
c  IPATH  - The input MicroMAX pathname (filename).  MicroMAX filename
c           suffixes are applied by process MAXIN.  e.g.  ipath test
c           assumes the data is in test.dat and the two MicroMAX headers
c           are test.hd1 and test.hd2.
c           REQUIRED.    e.g. ipath 05118813
c
c  shot/rp parameters:
c  ------- ----------
c  FNO    - The first shot/rp number the parameter list applies to.
c           Preset = the first shot/rp in file IPATH.    e.g.   fno 101
c  LNO    - The last shots/rp number the parameter list applies to.
c           DEFAULT = the last shot/rp in file IPATH.   e.g lno 101
c  NOINC  - The increment between FNO and LNO.  Only honored when FNO and
c           LNO are used.
c           Preset = 1      e.g   noinc 2
c  FTR    - The first trace of each shot/rp to read from disk file IPATH.
c           Traces less than FTR will not be read.
c           Preset = the first trace of every shot/rp   e.g.  ftr 11
c  LTR    - The last trace of each shot/rp to read from disk file IPATH.  Traces
c           greater than LTR will not be read.
c           Preset = the last trace of each shot/rp    e.g.  ltr 11
c  TRINC  - The trace increment between FTR and LTR.
c           Preset = 1       e.g.   trinc 2
c
c
c
c  Copyright (C) 1992 Seismic Reflection Processors, Solana Beach, CA.
c  ALL RIGHTS RESERVED.
c
c****   mod 21 oct 96 - remove some bad attempt at allowing multiple
c****               parameter lists
c
      PARAMETER ( npars = 8 )                                           ! the number of user parameters
      PARAMETER ( VAXUNIX = 2 )
      PARAMETER ( VAXVMS = 4 )
      DIMENSION buf(111), ibuf(111), lbuf(111)
      INTEGER*2 ibuf
      CHARACTER*80 token
      CHARACTER*6 names(npars)
      DIMENSION scr(10), lscr(10)
      EQUIVALENCE (scr(1),lscr(1))
      CHARACTER*80 cheadr(40)
      EQUIVALENCE (ivms,cheadr(1))
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, jntrcs
c      COMMON /maxin/ lunparams, nwrds, nlists
      COMMON /maxin/ luni, lunhd1, lunhd2, fno, lno, noinc, ftr, ltr,
     &       trinc, lprint
      DATA names /'IPATH ', 'LPRINT', 'FNO   ', 'LNO   ', 'NOINC ',
     *            'FTR   ', 'LTR   ', 'TRINC' /
c**** 
c****    Set the parameter presets and various variable presets
c****
      luni = 0
      lprint = 0
      fno  = 0
      lno = 0
      noinc = 1
      ftr = -12345
      ltr = 0
      trinc = 1
      CALL getfil( 1, lunparams, token, istat )                         ! get a unit for the parameters
c****
c****     get the user's parameters
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
             IF( names(nparam) .EQ. 'IPATH' ) THEN
                 token(nchars+1:nchars+4) = '.dat'
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 7 .OR.
     *                icompt .EQ. 3 .OR. icompt .EQ. 5 )                ! UNIX needs a NULL terminator
     *                   token(nchars+5:nchars+5) = CHAR(0)
                 CALL getfil( 4, luni, token, istat )
                 IF( istat .NE. 0 ) THEN
                     PRINT *,' ***  ERROR  ***  Could not open file ',
     &                      token
                     ierror = ierror + 1
                 ENDIF
                 token(nchars+1:nchars+4) = '.hd1'
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 7 .OR.
     *                icompt .EQ. 3 .OR. icompt .EQ. 5 )                ! UNIX needs a NULL terminator
     *                   token(nchars+5:nchars+5) = CHAR(0)
                 CALL getfil( 4, lunhd1, token, istat ) 
                 IF( istat .NE. 0 ) THEN 
                     PRINT *,' ***  ERROR  ***  Could not open file ',
     &                      token 
                     ierror = ierror + 1 
                 ENDIF
                 token(nchars+1:nchars+4) = '.hd2'
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 7 .OR.
     *                icompt .EQ. 3 .OR. icompt .EQ. 5 )                ! UNIX needs a NULL terminator
     *                   token(nchars+5:nchars+5) = CHAR(0)
                 CALL getfil( 4, lunhd2, token, istat ) 
                 IF( istat .NE. 0 ) THEN 
                     PRINT *,' ***  ERROR  ***  Could not open file ',
     &                      token 
                     ierror = ierror + 1 
                     GOTO 100 
                 ENDIF
                 GOTO 100
             ENDIF
             CALL upcase( token, nchars )
             CALL dcode( token, nchars, areal, istat )                  ! convert the alpha number to an internal machine number
             IF( istat .NE. 2 ) ierror = ierror + 1                     ! was the an error decoding it?
             IF( names(nparam) .EQ. 'FNO' ) fno = areal
             IF( names(nparam) .EQ. 'LNO' ) lno = areal
             IF( names(nparam) .EQ. 'NOINC' ) noinc = areal
             IF( names(nparam) .EQ. 'FTR' ) ftr = areal
             IF( names(nparam) .EQ. 'LTR' ) ltr = areal
             IF( names(nparam) .EQ. 'TRINC' ) noinc = areal
             IF( names(nparam) .EQ. 'LPRINT' ) lprint = NINT(areal)
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
c****    Do some ERROR checking
c****
      IF( luni .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  IPATH must be given.'
          ierror = ierror + 1
      ENDIF
      IF( fno .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  FNO must be positive.'
          ierror = ierror + 1
      ENDIF
      IF( lno .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  LNO must be positive.'
          ierror = ierror + 1
      ENDIF
      IF( noinc .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  NOINC must be positive.'
          ierror = ierror + 1
      ENDIF
      IF( lno .NE. 0 .AND. lno .LT. fno ) THEN
          PRINT *,' ***  ERROR  ***  FNO must be .LE. LNO.'
          ierror = ierror + 1
      ENDIF
      IF( (ftr .LT. 0 .OR. ftr .GT. 10000) .AND. ftr .NE. -12345) THEN
          PRINT *,' ***  ERROR  ***  Illegal FTR value of ',ftr
          ierror = ierror + 1
      ENDIF
      IF( ltr .LT. 0 .OR. ltr .GT. 10000 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal LTR value of ',ltr
          ierror = ierror + 1
      ENDIF
      IF( ltr .NE. 0 .AND. ltr .LT. ftr ) THEN
          PRINT *,' ***  ERROR  ***  FTR must be .LE. LTR.'
          ierror = ierror + 1
      ENDIF
      IF( trinc .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  TRINC must be positive.'
          ierror = ierror + 1
      ENDIF
      IF( IAND(lprint,1) .NE. 0 ) THEN
          PRINT *,' fno = ',fno,' lno = ',lno,' noinc = ',noinc
          PRINT *,' ftr = ',ftr,' ltr = ',ltr,' trinc = ',trinc
      ENDIF
c****
c****    Read *****.hdr1 and set the SEGY headers
c****    Assume this is a IEEE machine and the data is IEEE
c****
      CALL rddisc( lunhd1, buf, 52, istat )
      IF( icompt .NE. VAXVMS .AND. icompt .NE. VAXUNIX )
     &        CALL swap32( buf, 52 )
      IF( icompt .EQ. VAXVMS ) CALL pc2dr( buf(8), 1, buf(8) )
      si = buf(8)
      numdat = lbuf(9)
      icdpsrt = lbuf(47)
      jntrcs = lbuf(49)
      ifmt = lbuf(52)
      CALL getfil( 1, ihunit, token, istat )
      DO  510 i=1,40
          cheadr(i) = ' '
          WRITE ( cheadr(i), 500) i
  500     FORMAT('c ',i2)
          CALL ascebc( cheadr(i), 80, cheadr(i) )
  510 CONTINUE
      IF( icompt .NE. 4 ) THEN
          CALL wrdisc( ihunit, cheadr, 800)
      ELSE
          CALL wrdisc( ihunit, ivms, 800)
      ENDIF
      DO 600 i = 1,200
  600 ibuf(i) = 0
      ibuf(7) = jntrcs
      ibuf(15) = icdpsrt + 1
      CALL wrdisc(ihunit,ibuf,100)
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
      IF( token(1:nchars) .NE. 'END' .OR. nchars .NE. 3 ) GOTO 100
      RETURN
      END
