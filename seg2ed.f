      SUBROUTINE seg2ed( buf, lbuf, ibuf )
c
c                       PROCESS SEG2IN
c                       ------- ------
c
c  Document date: 17 August 1998
c
c  little endian - little end first - DEC and INTEL
c
c  PARAMETER DICTIONARY
c  --------- ----------
c
c
c  Written by Paul Henkart, Scripps Institution of Oceanography, August 1998
c  Copyright (C) 1998 The Regents of the University of California
c  ALL RIGHTS RESERVED.
c
c mod 26 Apr 01,  If byte swapping, do the data format code also
c Also, don't put the sample interval in the header since it's not known yet.
c mod 1 June 01 - oops, that change made ifmt bad
c mod 29 Sep 08 - FFILEN no longer worked (I4 now gives leading blanks)
c mod 21 Mar 09 - Allow 5 digits! (I5)
c
      PARAMETER ( nmulti = 0 )
      PARAMETER ( npars = 6)                                            ! the number of user parameters
      CHARACTER*80 token
      CHARACTER*6 names(npars), lastname
      DIMENSION buf(111), lbuf(111), ibuf(111)
      INTEGER*2 ibuf
      DIMENSION scr(npars), lscr(npars)
      EQUIVALENCE (scr(1),lscr(1))
      COMMON /readt/ ilun, numhdr, numdat, ihunit, ireeln, ntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr,
     *               nrskip, nfiles
      COMMON /porder/ num, iorder(40)
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /seg2in/ lun, nlists, nwrds, multi, iswap
      INTEGER lprint, ftr, ltr, ffilen, lfilen
      CHARACTER*100 path
      DATA names / 'LPRINT', 'FFILEN', 'LFILEN', 'FTR   ', 'LTR   ',
     &             'IPATH ' /
c**** 
c****    Set the parameter presets and various variable presets
c****
      lprint = 0
      nlists = 0
      ffilen = 99999
      lfilen = 99999
      ftr = -12345
      ltr = 0
      path = ' '
      CALL getfil( 1, lun, token, istat )                               ! get a file for the parameters
      iread = 0
      DO i = 1, num
         IF( iorder(i) .EQ. 72 ) iread = 1
      ENDDO
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
             IF( names(nparam) .EQ. 'IPATH' ) THEN
                 path = token
                 GOTO 100
             ENDIF
             CALL upcase( token, nchars )
             CALL dcode( token, nchars, areal, istat )                  ! convert the alpha number to an internal machine number
             IF( istat .NE. 2 ) ierror = ierror + 1                     ! was the an error decoding it?
             IF( names(nparam) .EQ. 'LPRINT' ) lprint = NINT(areal)
             IF( names(nparam) .EQ. 'FFILEN' ) ffilen = NINT(areal)
             IF( names(nparam) .EQ. 'LFILEN' ) lfilen = NINT(areal)
             IF( names(nparam) .EQ. 'FTR' ) ftr = NINT(areal)
             IF( names(nparam) .EQ. 'LTR' ) ltr = NINT(areal)
             lastname = names(nparam)
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
      IF( (ftr .LT. 0 .OR. ftr .GT. 10000) .AND. ftr .NE. -12345 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal FTR value of ',ftr
          ierror = ierror + 1
      ENDIF
      IF( ltr .LT. 0 .OR. ltr .GT. 10000 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal LTR value of ',ltr
          ierror = ierror + 1
      ENDIF
      IF( path .NE. ' ' .AND. ffilen .NE. 99999 ) THEN
          PRINT *,' ***  ERROR  ***  IPATH and FFILEN are incompatible.'
          ierror = ierror + 1
      ENDIF
      IF( ffilen .EQ. 99999 .AND. lfilen .NE. 99999 ) THEN
          PRINT *,' ***  ERROR  ***  FFILEN and LFILEN must be given.'
          ierror = ierror + 1
      ENDIF
      IF( lfilen .EQ. 99999 .AND. ffilen .NE. 99999 ) THEN
          PRINT *,' ***  ERROR  ***  FFILEN and LFILEN must be given.'
          ierror = ierror + 1
      ENDIF
c****
c****  Create the SEG-Y headers
c****
      IF( iread .EQ. 1 .AND. ierror .EQ. 0 .AND. nlists .EQ. 0 ) THEN
          IF( path .NE. ' ' ) THEN
              token = path
          ELSE
              token = ' '
              WRITE( token, 3000 ) ffilen
              token(10:10) = CHAR(0)
              DO i = 1, 4
                 IF( token(1:1) .EQ. ' ' ) token(1:9) = token(2:10)
              ENDDO
          ENDIF
c 3000     FORMAT('SEG',I5.5,'.DAT')
 3000     FORMAT(I5,'.DAT')
          CALL getfil( 4, ilun, token, istat )                          ! open the first SEG2 file
          IF( istat .NE. 0 ) THEN
              PRINT *,' ***  ERROR  ***  Can not open SEG2 file:',token
              ierror = ierror + 1
              GOTO 2000
          ENDIF
          CALL rddiscb( ilun, buf, 32, istat )
          iswap = 0
          IF( ibuf(1) .EQ. 21818 ) iswap = 1                            ! 553a hex
          IF( iswap .NE. 0 ) CALL swap16( ibuf, 4 )                     ! don't swap everything
          IF( ibuf(1) .NE. 14933 ) THEN                                 ! must be 3a55 by now
              PRINT *,' ***  ERROR  ***  Input file is not SEG2.'
              PRINT *,' ***  ERROR  *** Must start with 3a55 or 553a.'
              ierror = ierror + 1
          ENDIF
          irevnum = ibuf(2)
          nbblk = ibuf(3) * 2
          ntrcs = ibuf(4)
          IF( ltr .GT. ntrcs ) THEN
              PRINT *,' ***  WARNING  ***  LTR ',ltr,
     &                ' exceeds the number traces of ',ntrcs
              iwarn = iwarn
              ltr = ntrcs
          ENDIF
c****     Now read the trace pointers
          CALL rddisc( ilun, lbuf, ntrcs, istat )
          IF( iswap .NE. 0 ) CALL swap32( lbuf, ntrcs )
c****     Get the first SEG2 trace descriptor
          CALL podiscb( ilun, 1, lbuf(1) )
          CALL rddiscb( ilun, ibuf, 32, istat )
          IF( iswap .NE. 0 ) THEN
              CALL swap16( ibuf, 2 )
              CALL swap32( lbuf(2), 2 )
              CALL swap16( ibuf(7), 1 )
          ENDIF
          IF( ibuf(1) .NE. 17442 ) THEN                                 ! 4422 hex
              PRINT *,' ***  ERROR  ***  SEG2 trace descriptor error.'
              ierror = ierror + 1
          ENDIF
          nbblk = ibuf(2) * 2
          numdat = lbuf(3)
c          ifmt = ibuf(7) / 256
          ifmt = ibuf(7)
          IF( ifmt .LT. 1 .OR. ifmt .GT. 4 ) THEN
              PRINT *,' ***  ERROR  ***  Bad SEG2 data format of:',ifmt
              ierror = ierror + 1
          ENDIF
          
          CALL getfil( 1, ihunit, token, istat )                        ! get an SEGY header file
          DO  i=1,40
              token = ' '
              WRITE (token, '(2Hc ,I2)') i
              CALL ascebc(token, 80, token )
          ENDDO
          DO i = 1, numhdr
             lbuf(i) = 0
          ENDDO
          ibuf(7) = ntrcs
          ibuf(15) = 1
          ibuf(31) = idtype
          CALL wrdisc(ihunit,ibuf,100)
      ENDIF
c****
c****   May need to get a trace descriptor in order to get the sample interval!
c****


c****
c****  Now write the parameters to disk
c****
      lbuf(1) = lprint
      lbuf(2) = ffilen
      lbuf(3) = lfilen
      lbuf(4) = ftr
      lbuf(5) = ltr
      nwrds = npars
      CALL wrdisc( lun, buf, nwrds )
      nlists = nlists + 1
      multi = nmulti
      CALL wrdiscb( lun, path, 100 )
      IF( IAND(lprint,1) .NE. 0 ) THEN
          PRINT *,(lbuf(i),i=1,nwrds)
          PRINT *,path
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
c****   create a trace header and leave
c****
      DO i = 1, numhdr
         lbuf(i) = 0
      ENDDO
      ibuf(58) = numdat
c     si hasn't been set yet.
c      ibuf(59) = NINT(si*100000.)
c      buf(49) = si
      RETURN
      END
