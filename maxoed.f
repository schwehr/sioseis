      SUBROUTINE maxoed( buf, ibuf, lbuf )
c
c                       PROCESS MAXOUT
c                       ------- ------
c
c  Document date: 3 July 1992
c
c      Process MAXIN writes MicroMAX (TM) disk files.  MicroMAX does
c  not have a standard for representing data words (floating point).
c  Process MAXOUT permits the user to specify three different data
c  word representations.  If the output data word is different from
c  the data word format of the computer generating the file (the
c  machine SIOSEIS is running on), then the MicroMAX file can not be
c  read on the current computer.
c      
c
c  PARAMETER DICTIONARY
c  --------- ----------
c  OPATH  - The output MicroMAX pathname (filename).  MicroMAX filename
c           suffixes are applied by process MAXIN.  e.g.  opath test
c           will put the data in test.dat and will create the two 
c           MicroMAX headers, test.hd1 and test.hd2.
c           REQUIRED.    e.g. opath 05118813
c  FORMAT - The output file data word format.
c         = VMS, The output will be formated for a VMS VAX
c           (Dec floating point)
c         = ULTRIX, The output will be formated for a DecStation or PC.
c           (IEEE floating point, little endian)
c         = SUN, the output will be formated for an IEEE big endian
c           computer.
c
c
c  Copyright (C) 1992 Seismic Reflection Processors, Solana Beach, CA.
c  ALL RIGHTS RESERVED.
c
c
      PARAMETER ( npars = 4 )                                           ! the number of user parameters
      INTEGER prime, vaxunix, apollo, vaxvms, cray, convex, ieee
      PARAMETER ( prime = 1, vaxunix = 2, apollo = 3, vaxvms = 4,
     &            cray = 5, convex = 6, ieee = 7 )
      DIMENSION buf(1), ibuf(1), lbuf(1)
      INTEGER*2 ibuf
      CHARACTER*80 token
      CHARACTER*7 names(npars)
      DIMENSION scr(npars), lscr(npars)
      EQUIVALENCE (scr(1),lscr(1))
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, jntrcs
      COMMON /maxout/ luno, lunhd1, lunhd2, swap, lprint, machine
      LOGICAL swap
      DATA names /'OPATH  ', 'LPRINT ', 'SWAP   ','MACHINE' /
c**** 
c****    Set the parameter presets and various variable presets
c****
      luno = 0
      lprint = 0
      swap = .FALSE.
      machine = icompt
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
             IF( names(nparam) .EQ. 'OPATH' ) THEN
                 token(nchars+1:nchars+4) = '.dat'
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 7 .OR.
     *                icompt .EQ. 3 .OR. icompt .EQ. 5 )                ! UNIX needs a NULL terminator
     *                   token(nchars+5:nchars+5) = CHAR(0)
                 CALL getfil( 3, luno, token, istat )
                 IF( istat .NE. 0 ) THEN
                     PRINT *,' ***  ERROR  ***  Could not open file ',
     &                      token
                     ierror = ierror + 1
                 ENDIF
                 token(nchars+1:nchars+4) = '.hd1'
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 7 .OR.
     *                icompt .EQ. 3 .OR. icompt .EQ. 5 )                ! UNIX needs a NULL terminator
     *                   token(nchars+5:nchars+5) = CHAR(0)
                 CALL getfil( 3, lunhd1, token, istat ) 
                 IF( istat .NE. 0 ) THEN 
                     PRINT *,' ***  ERROR  ***  Could not open file ',
     &                      token 
                     ierror = ierror + 1 
                 ENDIF
                 token(nchars+1:nchars+4) = '.hd2'
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 7 .OR.
     *                icompt .EQ. 3 .OR. icompt .EQ. 5 )                ! UNIX needs a NULL terminator
     *                   token(nchars+5:nchars+5) = CHAR(0)
                 CALL getfil( 3, lunhd2, token, istat ) 
                 IF( istat .NE. 0 ) THEN 
                     PRINT *,' ***  ERROR  ***  Could not open file ',
     &                      token 
                     ierror = ierror + 1 
                     GOTO 100 
                 ENDIF
                 GOTO 100
             ENDIF
             CALL upcase( token, nchars )
             IF( names(nparam) .EQ. 'SWAP' ) THEN
                 IF( token(1:5) .EQ. 'TRUE' ) THEN
                     swap = .TRUE.
                 ELSEIF( token(1:5) .NE. 'FALSE' ) THEN
                     PRINT *,' ***  ERROR  ***  Bad swap of ',token
                 ENDIF
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'FORMAT' ) THEN
                 IF( token .NE. 'ULTRIX'.AND. token .NE. 'VMS' .AND.
     &               token .NE. 'SUN' ) THEN
                        PRINT *,'  ***  ERROR  ***  Illegal FORMAT ',
     &                     token
                     ierror = ierror + 1
                 ENDIF
                 IF( token .EQ. 'ULTRIX' ) machine = vaxunix
                 IF( token .EQ. 'VMS' ) machine = vaxvms
                 IF( token .EQ. 'SUN' ) machine = ieee
             ENDIF
             CALL dcode( token, nchars, areal, istat )                  ! convert the alpha number to an internal machine number
             IF( istat .NE. 2 ) ierror = ierror + 1                     ! was the an error decoding it?
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
      IF( luno .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  OPATH must be given.'
          ierror = ierror + 1
      ENDIF
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
