      SUBROUTINE died( ibuf, buf, lbuf, scr, iscr, lscr )
c                                                       
c   
c  Copyright (C) by The Regents of The University of California, 1988
c  Written by Paul Henkart, Scripps Institution of Oceanography, La Jolla, Ca.
c  ALL RIGHTS RESERVED.
c  mod 20 Jul 18 - remove ODEC nav message since it is now converted to arcseconds
c                - FORMAT SWAPPED and ODEC were swapping the binary header and shouldn't
c  mod 30 Aug 16 - Add message about REAL source/receiver x,y when ODEC
c  mod 5 Nov 15 - Added FORMAT NIUST (jform = 15)
c  mod 15 Oct 14 - set si in common in case another edit (like ploted) wants it
c  mod 17 Oct 12 - Make FNO default to first in file.
c  mod 19 Apr 12 - Make random not given preset = -1
c  mod 31 Aug 11 - Attempt to allow file names with "\ " in them (fails in diskio.c)
c  mod 30 Aug 11 - Print warning when nsamps in binhdr and trace header differ.
c  mod 21 Mar 11 - Add LNTODO and equivalence FNTODO to NTODO
c  mod 20 Jan 11 - Set random to 0 if secs is given.
c  mod 22 Jul 10 - make header byte swaps same as diex
c  mod 27 May 10 - Add FORMAT ASCII and BINARY
c  mod 2 Dec 09 - Increment error count if dcode is bad.
c  mod 25 Jun 09 - Change ALLNO preset from yes to no.
c  mod 19 Jun 09 - Add parameter RANDOM
c  mod 18 Jun 09 - Issue warning if retrac > 1
c  mod 16 Feb 09 - Create error when NOINC is given and SEGY header ntrcs is 0
c  mod 5 Feb 09 - Allow/honor IPATH of 200 characters
c  mod 11 Dec 08 - Allow WAV files.
c  mod 27 Jun 08 - Allow ipath and token to be 200 characters
c  mod 28 Nov 07 - Set retrac to 1 if given because diex sets it to 1 on
c                  every record but the first.
c  mod 18 Jun 07 - Only use SEGY REV when SEG-Y format (jform = 1)
c  mod 27 Sep 06 - Linux ODEC should NOT byte swap the binary header
c  mod 29 Jul 06 - Change LGMT from a default to a preset (don't reset)
c  mod 22 May 06 - Add checks for "edit only" when irun = 0
c  mod 27 Apr 06 - gfortran chokes on internal reads that were necessary for VMS
c  mod 22 Sep 05 - Put path in common so others know the file name
c  mod 25 Aug 05 - Add Edgetech5 - jform 11
c  mod 21 May 04 - Zero out some of the binary header on Edgetech
c  mod 16 Jul 03 - Add FORMAT UTIG-OBS (jform 10)
c  mod 28 May 03 - Tighten the check for Rev 1 so that crap in the
c                  binary header doesn't trigger the extended headers.
c  mod 5 May 03 - Take care of SEG-Y Rev 1 Extended Textual Headers
c  mod 1 Apr 03 - Save the first 20 words of the binary header
c  mod 7 Jan 03 - Don't allow FNO/LNO on ODEC or EDGETECH
c  mod 17 Dec 02 - Remove the following
c  mod 18 Nov 02 - Assume when EDGETECH is used, it has 2 traces because
c                  Driscoll's does!  Geostar doesn't use parameter
c         EDGETECH and only has 1 trace.  Lizaralde's Xstar has 1 trace.
c  mod 15 Nov 02 - Don't warn about RETRAC/RENUM when Edgetech
c  mod 9 Aug 02 - Make parameter FORMAT DSS-240 to read some old
c                 screw DSS-240 data.
c  mod June 00 - Add parameter ntodo.
c  mod 29 Sep 99 - Removed parameter MASS and added parameter
c                  ALT and made ALT preset = 2.
c  mod 22 Sep 99 - Add format EdgeTech
c  mod 15 Jul 99 - Add format knudsen
c  mod 3 Apr 99 - Remove Fortran INQUIRE - caused core dump on Ewing
c                 Solaris 5.6 if the file does NOT exist.
c 22 Sept 98 - Make format ODEC work.
c 31 Aug 98 - Add parameter nsamps for overriding the SEG-Y trace header
c  mod 26 Aug 98 - Add format ODEC and make format SWAPPED pure.
c  mod 24 Mar 97 - Add FORMAT NOHEAD for headerless SEG-Y.  Require IFMT too.
c  mod 9 Mar 97  - Add FORMAT SWAPPED for little-endian SEG-Y files.
c  mod 5 Oct. 96 - Give warning when rp no = 0 and rp tr no <> 0
c  mod 19 Mar 96 - Add parameters a11no, alltr, tr, no
c  mod 19 May 95 - Allow the SU format.
c  mod 5 Apr 95 - Add check for max trace length
c  mod 3 June 94 - Add error if NOINC is given and FNO/LNO are not.
c  mod 9 Mar. 94 - Allow INTEGER user parameters to be super big
c                  (CALL lcode rather tha dcode on integer parameters).
c  mod 9 Jan. 94 - Add noinc 99999 and trinc 99999
c  mod 19 Oct 93 - Change documentation for SORT
c  mod 10 Nov 92 - Require ftr and ltr to be given when trinc is given.
c                - remove Cray MASS parameter.
c  mod 7 July 92 - Add stack to the sort parameter possiblities.
c  mod 24 Feb 92 - remove the version stuff to read header of 1st trace
c  mod 17 Jan 92 - ltr was DEFAULTED to 0 rather than PRESET.
c  mod 5 Dec 91 - SPATH must be opened at execute time for VMS!
c  mod 27 Nov 91 - allow ntrgat 0 to be 0 lbuf(51) on all traces
c  mod 29 July 91 - add spath
c  mod 9 jan 91 - ipath2 didn't work on shot sorted data
c  mod 3 May 90 - move si override to AFTER the trace header is read!
c  mod 3 May 90 - Add IRIS (PASSCAL) modified SEGY format
c  mod 2 May 90 - weird Unix Apollo INQUIRE file name problem
c  mod 1 May 90 - Make Apollo (icompt=3) a Unix machine
c  mod 10 Jan 90 - rewite doc of secinc - diex didn't use it at all!
c  mod 7 Jan 90 - add the Cray to the ranks of Unix!
c  mod 16 Nov 89 - make the trace header all zero when SSC input
c  mod 16 Nov 89 - the sort parameter needed upcase
c
c
      PARAMETER ( npars = 44 )                                          ! the number of user parameters
      DIMENSION buf(1111), scr(1111)
      INTEGER*2 ibuf(1111), iscr(1111)
      INTEGER lbuf(1111), lscr(1111)           
      CHARACTER*80 cheadr(40)
      CHARACTER*200 token
      CHARACTER*6 names(npars)
      CHARACTER*1 types(npars)                                          ! the type of parameter
      REAL vals(npars)                                                  ! holds the REAL parameter values
      DIMENSION lvals(npars)                                            ! holds the INTEGER parameter values
c
      COMMON /edits/ ierror, iwarn, irun, now, icompt, isite, maxsamps,
     &     nbperw
      COMMON /porder/ num, iorder(100)
      COMMON /diskin/ junit, nlists, nwrds, lun2
      COMMON /diskin1/ spath, path
      CHARACTER*80 spath
      CHARACTER*200 path
      COMMON /readt/ lun, numhdr, numdat, ihunit, ireeln, jntrcs, 
     *               jfmt, nskip, secs_died, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr,
     *               nrskip, nfiles, irewind, delay1, segyrev, si_died
      COMMON /binhdr/ ibinhdr(200)
      INTEGER*2 ibinhdr
c
      INTEGER fno, ftr, fday, fgmt, gmtinc, fsec, secinc, renum, decimf,
     *        trinc, rewind, retrac, allno, alltr, notype, noindex,
     &        itrtype, itrindex, alt, random, fntodo
      DIMENSION set(2)
c
      EQUIVALENCE ( ipath, lvals(1) ),
     2            ( lprint, lvals(2) ),
     3            ( fno, lvals(3) ),
     4            ( lno, lvals(4) ),
     5            ( noinc, lvals(5) ),
     6            ( ftr, lvals(6) ),
     7            ( ltr, lvals(7) ),
     8            ( fday, lvals(8) ),
     9            ( lday, lvals(9) ),
     *            ( fgmt, lvals(10) ),
     1            ( lgmt, lvals(11) ),
     2            ( gmtinc, lvals(12) ),
     3            ( fsec, lvals(13) ),
     4            ( lsec, lvals(14) ),
     5            ( secinc, lvals(15) ),
     6            ( renum, lvals(16) ),
     7            ( secs, vals(17) ),
     8            ( ifmt, lvals(18) ),
     9            ( si, vals(19) )
      EQUIVALENCE ( decimf, lvals(20) ),
     1            ( delay, vals(21) ),
     2            ( intrcs, lvals(22) ),
     3            ( ntrgat, lvals(23) ),
     4            ( trinc, lvals(24) ),
     5            ( alt, lvals(25) ),
     6            ( set, vals(26) ),
     7            ( rewind, lvals(27) ),
     8            ( forgat, vals(29) ),
     9            ( mintrs, lvals(30) ),
     *            ( retrac, lvals(32) ),
     1            ( sort, lvals(33) ),
     2            ( ascii, vals(34) ),
     3            ( allno, lvals(36) ),
     4            ( alltr, lvals(37) ),
     5            ( nsamps, lvals(40) ),
     6            ( ntodo, lvals(41) ),
     2            ( random, lvals(42) ),
     3            ( fntodo, lvals(43) ),
     4            ( lntodo, lvals(44) )
      DATA names /'IPATH ', 'LPRINT', 'FNO   ', 'LNO   ', 'NOINC ',
     *            'FTR   ', 'LTR   ', 'FDAY  ', 'LDAY  ', 'FGMT  ',
     *            'LGMT  ', 'GMTINC', 'FSEC  ', 'LSEC  ', 'SECINC',
     *            'RENUM ', 'SECS  ', 'IFMT  ', 'SI    ', 'DECIMF',
     *            'DELAY ', 'NTRCS ', 'NTRGAT', 'TRINC ', 'ALT   ',
     *            'SET   ', 'REWIND', 'FORMAT', 'FORGAT', 'MINTRS',
     *            'IPATH2', 'RETRAC', 'SORT  ', 'ASCII ', 'SPATH ',
     *            'ALLNO ', 'ALLTR ', 'NO    ', 'TR    ', 'NSAMPS',
     *            'NTODO ', 'RANDOM', 'FNTODO', 'LNTODO' /
      DATA types /'A',15*'L', 'F', 'L', 'F', 'L', 'F', 4*'L','F','A',
     *            'A', 'F','L','A','L','A','F','A',4*'A',5*'L'/
      DATA iread/0/
c**** 
c****    Set the parameter presets and various variable presets
c****
      path = ' '
      spath = ' '
      lprint = 0
      fno = 0
      lno = 0
      noinc = 99999
      ftr = -12345
      ltr = 0
      trinc = 99999
      fday = 0
      lday = 366
      fgmt = 0
      lgmt = 2500
      gmtinc = 1
      fsec = 0
      lsec = 60
      secinc = 0
      renum = 0
      secs = 0.
      ifmt = 0
      si = 0.
      decimf = 1
      delay = -1.
      intrcs = 0
      ntrgat = -1
      alt = 2
      nlists = 0
      set(1) = 0.
      set(2) = 0.
      rewind = 0
      jform = 1                                                         ! file format; 1=SEGY, 2=SSC
      segyrev = 0
      forgat = 0.
      mintrs = 0
      lun2 = 0
      retrac = 0
      isort = 0
      ascii = 0.
      lunsort = 0
      notype = 0
      noindex = 0
      itrtype = 0
      itrindex = 0
      allno = 0
      alltr = 1
      nsamps = 0
      ntodo = 0
      random = -1
      fntodo = 0
      lntodo = 0
c
      DO 10 i = 1, num
         IF( iorder(i) .EQ. 23 ) iread = 1
   10 CONTINUE
      CALL getfil( 1, junit, token, istat )                             ! get a file for the DISK parameters 
      CALL getfil( 1, ihunit, token, istat )                            ! get a file for the SEGY tape headers 
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
             ntokes = ntokes + 1
             IF( nchars .EQ. 0 ) THEN
                 CALL rdline
                 ntokes = 0
                 GOTO 120
             ENDIF
             ns = ns + 1
             IF( types(nparam) .EQ. 'A' ) THEN
                 IF( names(nparam) .EQ. 'IPATH' ) THEN
                     path = ' '      ! make sure it's all blanks
                     path = token(1:nchars)
                     mode = 4
                     itemp = nchars
                     IF( token(nchars:nchars) .EQ. '\' ) THEN
                         mode = 5
  123                    itemp = itemp + 1
                         path(itemp:itemp) = ' '
                         itemp = itemp + 1
                         CALL getoke( token, nchars )
                         path(itemp:itemp+nchars-1) = token(1:nchars)
                         itemp = itemp + nchars - 1
                         IF( token(nchars:nchars) .EQ. '\' ) GOTO 123
                         nchars = itemp
                     ENDIF
                     IF( icompt .EQ. 2 .OR. icompt .EQ. 7 .OR.
     *                   icompt .EQ. 3 .OR. icompt .EQ. 4 )             ! UNIX needs a NULL terminator
     *                   path(nchars+1:nchars+1) = CHAR(0)
                     IF( path .EQ. '/dev/null' .AND. nlists .EQ. 1) THEN
                         PRINT *,
     &          ' ***  ERROR  ***  /dev/null may not be the first file.'
                         ierror = ierror + 1
                     ENDIF
                     GOTO 100
                 ENDIF
                 IF( names(nparam) .EQ. 'IPATH2' ) THEN
                     CALL getfil(4, lun2, token, istat )
                     IF( istat .NE. 0 ) THEN 
                         PRINT *,' ***  ERROR  *** Could not open file',
     &                      token
                         ierror = ierror + 1
                     ENDIF
                     GOTO 100 
                 ENDIF
                 IF( names(nparam) .EQ. 'SPATH' ) THEN
                     spath = token
                     GOTO 100 
                 ENDIF
c****            Now UPCASE the variable
                 CALL upcase( token, nchars )
                 IF( names(nparam) .EQ. 'FORMAT' ) THEN
                     IF( token(1:nchars) .NE. 'SSC' .AND. 
     *                   token(1:nchars) .NE. 'IRIS' .AND.
     *                   token(1:nchars) .NE. 'SEGY' .AND.
     *                   token(1:nchars) .NE. 'SU' .AND.
     *                   token(1:nchars) .NE. 'NOHEAD' .AND.
     *                   token(1:nchars) .NE. 'ODEC' .AND.
     *                   token(1:nchars) .NE. 'KNUDSEN' .AND.
     *                   token(1:nchars) .NE. 'EDGETECH' .AND.
     *                   token(1:nchars) .NE. 'EDGETECH5' .AND.
     *                   token(1:nchars) .NE. 'UTIG-OBS' .AND.
     *                   token(1:nchars) .NE. 'SWAPPED' .AND.
     *                   token(1:nchars) .NE. 'ASCII' .AND.
     *                   token(1:nchars) .NE. 'BINARY' .AND.
     &                   token(1:nchars) .NE. 'WAV' .AND.
     &                   token(1:nchars) .NE. 'NIUST' ) THEN
                         PRINT *,' ***  ERROR  ***  Illegal FORMAT.'
                         ierror  = ierror
                     ENDIF
                     IF( token(1:nchars) .EQ. 'SEGY' ) jform = 1
                     IF( token(1:nchars) .EQ. 'SSC' ) jform = 2
                     IF( token(1:nchars) .EQ. 'IRIS' ) jform = 3
c                    jform = 4 means LDGO DSS-240 format (pre 1992 Ewing)
                     IF( token(1:nchars) .EQ. 'DSS-240' ) jform = 4
                     IF( token(1:nchars) .EQ. 'SWAPPED' ) jform = 5
                     IF( token(1:nchars) .EQ. 'SU' ) THEN
                         jform = 6
                         ifmt = 5
                     ENDIF
                     IF( token(1:nchars) .EQ. 'NOHEAD' ) jform = 6
                     IF( token(1:nchars) .EQ. 'ODEC' ) jform = 7
                     IF( token(1:nchars) .EQ. 'KNUDSEN' ) jform = 8
                     IF( token(1:nchars) .EQ. 'EDGETECH' ) jform = 9
                     IF( token(1:nchars) .EQ. 'UTIG-OBS' ) jform = 10
                     IF( token(1:nchars) .EQ. 'EDGETECH5' ) jform = 11
                     IF( token(1:nchars) .EQ. 'WAV' ) jform = 12
                     IF( token(1:nchars) .EQ. 'ASCII' ) jform = 13
                     IF( token(1:nchars) .EQ. 'BINARY' ) jform = 14
                     IF( token(1:nchars) .EQ. 'NIUST' ) jform = 15
                 ENDIF
                 IF( names(nparam) .EQ. 'SORT' ) THEN
                     IF( token(1:nchars) .EQ. 'SHOT' ) isort = 1
                     IF( token(1:nchars) .EQ. 'CDP' ) isort = 2
                     IF( token(1:nchars) .EQ. 'STACK' ) isort = 3
                     GOTO 100
                 ENDIF
                 IF( names(nparam) .EQ. 'NO' ) THEN
                     IF( token(1:1) .EQ. 'I' ) THEN
                         notype = 1
                     ELSEIF( token(1:1) .EQ. 'L' ) THEN
                         notype = 2
                     ELSEIF( token(1:1) .EQ. 'R' ) THEN
                         notype = 3
                     ELSE
                         PRINT *,' ***  ERROR  ***  NO value must ',
     &                   'start with I or L or R.'
                         ierror = ierror + 1
                     ENDIF
                     token(1:nchars) = token(2:nchars)
                     nchars = nchars - 1
                     CALL dcode( token, nchars, areal, istat )
                     IF( istat .NE. 2 ) THEN
                         ierror = ierror + 1
                         GOTO 100
                     ENDIF
                     noindex = NINT(areal)
                 ENDIF
                 IF( names(nparam) .EQ. 'TR' ) THEN
                     IF( token(1:1) .EQ. 'I' ) THEN
                         itrtype = 1
                     ELSEIF( token(1:1) .EQ. 'L' ) THEN
                         itrtype = 2
                     ELSEIF( token(1:1) .EQ. 'R' ) THEN
                         itrtype = 3
                     ELSE
                         PRINT *,' ***  ERROR  ***  TR value must ',
     &                   'start with I or L or R.'
                         ierror = ierror + 1
                     ENDIF
                     token(1:nchars) = token(2:nchars)
                     nchars = nchars - 1
                     CALL dcode( token, nchars, areal, istat )
                     IF( istat .NE. 2 ) THEN
                         ierror = ierror + 1
                         GOTO 100
                     ENDIF
                     itrindex = NINT(areal)
                 ENDIF
                 IF( names(nparam) .EQ. 'ALLNO' ) THEN
                     IF( token(1:nchars) .EQ. 'YES' ) allno = 1
                     IF( token(1:nchars) .EQ. 'NO' ) allno = 0
                     IF( token(1:nchars) .EQ. 'ON' ) allno = 1
                     IF( token(1:nchars) .EQ. 'OFF' ) allno = 0
                     IF( token(1:nchars) .EQ. '1' ) allno = 1
                     IF( token(1:nchars) .EQ. '0' ) allno = 0
                 ENDIF
                 IF( names(nparam) .EQ. 'ALLTR' ) THEN
                     IF( token(1:nchars) .EQ. 'YES' ) alltr = 1
                     IF( token(1:nchars) .EQ. 'NO' ) alltr = 0
                     IF( token(1:nchars) .EQ. 'ON' ) alltr = 1
                     IF( token(1:nchars) .EQ. 'OFF' ) alltr = 0
                     IF( token(1:nchars) .EQ. '1' ) alltr = 1
                     IF( token(1:nchars) .EQ. '0' ) alltr = 0
                 ENDIF
                 IF( names(nparam) .EQ. 'REWIND' ) THEN
                     IF( token(1:nchars) .EQ. 'YES' ) rewind = 1
                     IF( token(1:nchars) .EQ. 'NO' ) rewind = 0
                     IF( token(1:nchars) .EQ. 'ON' ) rewind = 1
                     IF( token(1:nchars) .EQ. 'OFF' ) rewind = 0
                     IF( token(1:nchars) .EQ. '1' ) rewind = 1
                     IF( token(1:nchars) .EQ. '0' ) rewind = 0
                 ENDIF
                 GOTO 100
             ENDIF
             CALL upcase( token, nchars )
             IF( names(nparam) .EQ. 'SET' ) THEN
                 CALL dcode( token, nchars, set(ns), istat )
                 IF( istat .NE. 2 ) ierror = ierror + 1
                 IF( ns .EQ. 1 ) GOTO 120
                 GOTO 100
             ENDIF
             IF( types(nparam) .EQ. 'L' ) THEN
                 CALL lcode( token, nchars,lvals(nparam) , istat )
             ELSE
                 CALL dcode( token, nchars, vals(nparam), istat )
             ENDIF
             IF( istat .NE. 2 ) ierror = ierror + 1
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
          PRINT *,' ***  ERROR  ***  IPATH must be given.'
          ierror = ierror + 1
      ENDIF
      IF( fno .LT. 0 ) THEN
          PRINT *,' ***  WARNING  ***  FNO shoul be positive.'
          iwarn = iwarn + 1
      ENDIF
      IF( lno .LT. 0 ) THEN
          PRINT *,' ***  WARNING  ***  LNO should  be positive.'
          iwarn = iwarn + 1
      ENDIF
      IF( noinc .LT. 0 ) THEN
          PRINT *,' ***  WARNING  ***  NOINC should be positive.'
          iwarn = iwarn + 1
      ENDIF
      IF( lno .NE. 0 .AND. lno .LT. fno ) THEN
          PRINT *,' ***  WARNING  ***  FNO should be .LE. LNO.'
          iwarn = iwarn + 1
      ENDIF
      IF( noinc .NE. 99999 .AND. fno .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  FNO must be given with NOINC.'
          ierror = ierror + 1
      ENDIF
c      IF( noinc .NE. 99999 .AND. ltr .NE. 0 ) THEN
c          PRINT *,' ***  ERROR  *** LTR must NOT be given with NOINC.'
c          ierror = ierror + 1
c      ENDIF
      IF( (ftr .LT. 0 .OR. ftr .GT. 10000) .AND. ftr .NE. -12345) THEN
          PRINT *,' ***  WARNING  ***  FTR value of ',ftr,' may be bad.'
          iwarn = iwarn + 1
      ENDIF
      IF( ltr .LT. 0 .OR. ltr .GT. 10000 ) THEN
          PRINT *,' ***  WARNING  ***  LTR value of ',ltr,' may be bad.'
          iwarn = iwarn + 1
      ENDIF
      IF( ltr .NE. 0 .AND. ltr .LT. ftr ) THEN
          PRINT *,' ***  WARNING  ***  FTR should be .LE. LTR.'
          iwarn = iwarn + 1
      ENDIF
      IF( trinc .LT. 1 ) THEN
          PRINT *,' ***  WARNING  ***  TRINC should be positive.'
          iwarn = iwarn + 1
      ENDIF
      IF( trinc .NE. 1 .AND. (ftr .EQ. -12345 .OR. ltr .EQ. 0) .AND.
     &    trinc .NE. 99999 ) THEN
          PRINT *,' ***  ERROR  ***  FTR and LTR must be given.'
          ierror = ierror + 1
      ENDIF
      IF( ifmt .LT. 0 .OR. ifmt .GT. 9 ) THEN
          PRINT *,' ***  ERROR  ***  IFMT must be 1, 2, 3, or 5'
          ierror = ierror + 1
      ENDIF
      IF( ntodo .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  NTODO must be positive.'
          ierror = ierror + 1
      ENDIF
      IF( fntodo .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  FNTODO must be positive.'
          ierror = ierror + 1
      ENDIF
      IF( lntodo .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  LNTODO must be positive.'
          ierror = ierror + 1
      ENDIF
      IF( nsamps .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  NSAMPS must be positive.'
          ierror = ierror + 1
      ENDIF
      IF( intrcs .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  INTRCS must be positive.'
          ierror = ierror + 1
      ENDIF
      IF( alt .LT. 1 .OR. alt .GT. 2 ) THEN
          PRINT *,' ***  ERROR  ***  ALT must be 1, or 2.'
          ierror = ierror + 1
      ENDIF
      IF( lun2 .NE. 0 .AND. alt .EQ. 1 ) THEN
          IF( fno+lno .EQ. 0 .OR. ftr .EQ. -12345 ) THEN
              PRINT *,' ***  ERROR  ***  FNO and FTR must be given ',
     &                'when using IPATH2'
              ierror = ierror + 1
          ENDIF
       ENDIF
       IF( jform .EQ. 6 .AND. ifmt .EQ. 0 ) THEN
           PRINT *,' ***  ERROR  ***  IFMT must be given with NOHEAD.'
           ierror = ierror + 1
       ENDIF
       IF( jform .EQ. 7 .OR. jform .EQ. 9 .OR. jform .EQ. 11 ) THEN
           IF( fno+lno+ltr .NE. 0 .OR. ftr .NE. -12345 ) THEN
               PRINT *, ' ***  WARNING  *** FNO/LNO and FTR/LTR are igno
     &red with ODEC and EDGETECH formats.'
               iwarn = iwarn + 1
               fno  = 0
               lno = 0
               noinc = 99999
               ftr = -12345
               ltr = 0
               trinc = 99999
           ENDIF
      ENDIF
c      IF( jform .EQ. 7 ) PRINT *,
c     &' ***  WARNING  ***  ODEC nav (x,y coords) is IEEE floating point'
      IF( retrac .NE. 0 ) THEN
          IF( retrac .NE. 1 .AND. lbuf(7) .EQ. 0 ) THEN
              PRINT *,
     &           ' ***  WARNING  ***  RETRAC other than 1 may be wrong.'
          PRINT *,' RETRAC is based on the number of traces per record.'
              iwarn = iwarn + 1
          ENDIF
      ENDIF
      IF( jform .EQ. 13 .OR. jform .EQ. 14 ) THEN
          ifmt = 5
          random = 0
          IF( si .EQ. 0. ) THEN
              PRINT *, ' ***  WARNING  ***  SI not given. Using .001'
              iwarn = iwarn + 1
              si = .001
          ENDIF
      ENDIF
      IF( ierror .NE. 0 .OR. iread .EQ. 0 ) GOTO 2000
      IF( secs .NE. 0 ) random = 0
      IF( secs .NE. 0 ) secs_died = secs
c****
c****    Write the DISKIN parameters to a disc file  and get another list!
c****
      lscr(26) = lprint
      lscr(27) = fno
      lscr(28) = lno
      lscr(29) = noinc
      lscr(30) = ftr
      lscr(31) = ltr
      lscr(32) = fday
      lscr(33) = lday
      lscr(34) = fgmt
      lscr(35) = lgmt
      lscr(36) = gmtinc
      lscr(37) = fsec
      lscr(38) = lsec
      lscr(39) = secinc
      lscr(40) = renum
      scr(41) = secs
      lscr(42) = ifmt
      scr(43) = si
      lscr(44) = decimf
      scr(45) = delay
      lscr(46) = intrcs
      lscr(47) = ntrgat
      lscr(48) = trinc
      lscr(49) = alt
      scr(50) = set(1)
      scr(51) = set(2)
      lscr(52) = rewind
      lscr(53) = jform
      scr(54) = forgat
      lscr(55) = mintrs
      lscr(56) = retrac
      lscr(57) = isort
      scr(58) = ascii
      lscr(59) = lunsort
      lscr(60) = allno
      lscr(61) = alltr
      lscr(62) = notype
      lscr(63) = noindex
      lscr(64) = itrtype
      lscr(65) = itrindex
      lscr(66) = nsamps
      lscr(67) = ntodo
      lscr(68) = random
      IF( fntodo .NE. 0 ) lscr(67) = fntodo
      lscr(69) = lntodo
      nwrds = 69
      CALL wrdiscb( junit, path, 200)
      CALL wrdisc( junit, lscr(26), nwrds-25 )
      nlists = nlists + 1
c
      IF( IAND(lprint,1) .NE. 0 ) THEN
          PRINT *,path
          PRINT *,(lscr(i),i = 26,40)
          PRINT *,scr(41),lscr(42),scr(43),lscr(44),scr(45)
          PRINT *,(lscr(i),i=46,49),scr(50),scr(51),lscr(52),lscr(53)
          PRINT *, scr(54), (lscr(i),i=55,57),scr(58),lscr(59)
          PRINT *,(lscr(i),i=60,nwrds)
      ENDIF
c****
c****   get the first trace if DISKIN is in the procs list, other processes
c****  may want the trace header or other info (like sample rate, number of
c****  traces, record length).
c****
      IF( irun .NE. 0 .AND. iread .EQ. 1 .AND. nlists .EQ. 1 ) THEN
          CALL getfil( mode, lun, path, istat )
          IF( istat .NE. 0 ) THEN
              PRINT *,' ***  ERROR  ***  Could not open file ',path
              ierror = ierror + 1
              GOTO 2000
          ENDIF
          IF( jform .EQ. 12 ) THEN                                      ! WAV file doesn't have SEGY headers!
              CALL rddiscb( lun, scr, 44, istat )
              IF( icompt .NE. 2 .AND. icompt .NE. 4 ) THEN
                  CALL swap16( iscr(12), 1 )
                  CALL swap16( iscr(17), 2 )
                  CALL swap32( lscr(7), 1 )
                  CALL swap32( lscr(11), 1 )
              ENDIF
              intrcs = iscr(12)
              si = 1. /  FLOAT(lscr(7))
              isi = NINT(si * 1000000. )
              nbperw = 4
              nbytes = lscr(11)
              IF( iscr(17) .EQ. 2 ) THEN
                  ifmt = 3
                  nbperw = 2
              ENDIF
              numdat = lscr(11) / nbperw
          ENDIF
          IF( jform .EQ. 12 .OR. jform .EQ. 13 .OR. jform .EQ. 14 )
     &        GOTO 2000       ! segy header creation is done in diex
          CALL rddisc( lun, scr, 800, istat )                       ! get the EBCDIC header
          CALL wrdisc( ihunit, scr, 800 )                           ! write it to the header file
          CALL rddisc( lun, scr, 100, istat )                        ! the binary header
c****     If DEC or the user specified, swap the bytes
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) THEN
              IF( jform .NE. 5 .AND. jform .NE. 7) CALL swap16(iscr,200)
          ELSE
              IF( jform .EQ. 5 .OR. jform .EQ. 7) CALL swap16(iscr,200)
          ENDIF
          IF( jform .EQ. 9 .OR. jform .EQ. 11 ) THEN
              iscr(151) = 0
              iscr(153) = 0
          ENDIF
          CALL wrdisc( ihunit, scr, 100 )                           ! write it to the header file
          DO i = 1, 200
             ibinhdr(i) = iscr(i)
          ENDDO
          jntrcs = iscr(7)
          IF( intrcs .NE. 0 ) THEN
              jntrcs = intrcs
              iscr(7) = intrcs
          ENDIF
          jfmt = iscr(13)
          idtype = iscr(31)
          nk = iscr(32)
          IF( jform .EQ. 1 ) segyrev = REAL(iscr(151)) / 256.
c****     take care of the Rev 1 3200-byte Textual Header Extension records
c****     But watch out for junk filled binary headers!
          IF( iscr(153) .GT. 0 .AND. iscr(151) .LT. 512 .AND.
     &        segyrev .GE. 1. .AND. segyrev .LT. 2. .AND.
     &        iscr(153) .LE. 10 ) THEN
              DO i = 1, iscr(153)
                 CALL rddiscb( lun, ibuf, 3200, istat )
                 CALL wrdiscb( ihunit, ibuf, 3200 )
              ENDDO
          ENDIF
          IF( iscr(153) .GT. 10 .AND. segyrev .GT. 0. ) PRINT *,
     &    ' WARNING, dropping ',iscr(153),' SEGY Rev1 extended headers.'
c****     -1 means an unknown number of Extension records
          IF( iscr(153) .EQ. -1 .AND. segyrev .GT. 0. ) THEN
              ibinhdr(153) = 0
 1550         CALL rddiscb( lun, cheadr, 3200, istat )
              ibinhdr(153) = ibinhdr(153) + 1
              CALL wrdiscb( ihunit, cheadr, 3200 )
              CALL upcase( cheadr(1), 11 )
              IF( cheadr(1)(1:11) .NE. '((SEG: EndText))' ) GOTO 1550
              CALL podiscb( ihunit, 0, 3505 )
              CALL wrdiscb( ihunit, ibinhdr(153), 2 )
          ENDIF
          IF( icompt .NE. 5 ) THEN                                      ! is it a Cray?
              CALL rddisc( lun, ibuf, numhdr, istat )
c*****     below looks wrong for format swapped (jform 5).  Don't swap format SWAPPED
c              IF( icompt .EQ. 2 .OR. icompt .EQ. 4 .OR.jform.EQ.5) THEN
              IF( (icompt .EQ. 2 .OR. icompt .EQ. 4) .AND. 
     *            jform .NE. 5 .AND. jform .NE. 7 ) THEN         !  DON'T SWAP odec OR swapped
                  CALL swap32( lbuf, 7 )
                  CALL swap16( ibuf(15), 4 )
                  CALL swap32( lbuf(10), 8 )
                  CALL swap16( ibuf(35), 2 )
                  CALL swap32( lbuf(19), 4 )
                  CALL swap16( ibuf(45), 40 )
                  CALL swap32( lbuf(46), 15 )
              ENDIF
              IF( icompt .NE. 2 .AND. icompt .NE. 4.AND.jform.EQ.7) THEN! is it ODEC?
                  CALL swap32( lbuf, 7 )
                  CALL swap16( ibuf(15), 4 )
                  CALL swap32( lbuf(10), 8 )
                  CALL swap16( ibuf(35), 2 )
                  CALL swap32( lbuf(19), 4 )
                  CALL swap16( ibuf(45), 40 )
                  CALL swap32( lbuf(46), 15 )
              ENDIF
              IF( jform .EQ. 7 ) THEN                                   ! ODEC again!
                  lbuf(3) = ibuf(3)
                  lbuf(4) = 1
              ENDIF
              numdat = ibuf(58)
              IF( ibuf(58) .EQ. 32767 ) numdat = lbuf(58)
              buf(49) = FLOAT(ibuf(59)*decimf) / 1000000.
          ELSE
              CALL rddisc( lun, buf, 30, istat )
              CALL i22i8( ibuf(15), iscr, 4 )
              numdat = iscr(2)
              ibuf(118) = numdat
              micros = iscr(3)
              ibuf(119) = micros * decimf
              buf(49) = FLOAT(micros) / 1000000.
          ENDIF
          IF( numdat .NE. ibinhdr(11) .AND. nsamps .EQ. 0 ) THEN
              PRINT *,
     &      ' ***  WARNING  ***  Number of samples per trace mismatch.'
              PRINT *,' Binary header says:',ibinhdr(11),
     &        ' Trace header says:',numdat
              iwarn = iwarn + 1
          ENDIF
          IF( buf(49) .EQ. .00781 ) buf(49) = .0078125
c****    SEGY has max of 32767 samps (16bit int), but IRIS sets ibuf(58)
c****    to 32767 and then uses lbuf(58)
          IF( numdat .GT. maxsamps ) THEN
              PRINT *,' ***  ERROR  ***  Too many samples for SIOSEIS.'
              PRINT *,' Requesting ',numdat,' samples, ',
     &           ' SIOSEIS max is ',maxsamps
              PRINT *,' SIOSEIS could be recompiled with maxsam bigger.'
              ierror = ierror + 1
          ENDIF
          numdat = numdat / decimf
          CALL frefil( -2, lun, istat )                                 ! close, free, don't delete the data file
      ENDIF
c****
c****  take care of some trace header overrides since some proceses (plot)
c****  look at the trace header
c****
      IF( si .NE. 0 ) THEN
          ibuf(59) = si * 1000000.
          buf(49) = si
      ENDIF
c****  plot wants the sample interval and number of samples
      si_died = REAL(ibuf(59))/1000000
c**** 
c****    Check the first trace for having rp = 0 and rp trace no <> 0
c****
      IF( lbuf(6) .EQ. 0 .AND. lbuf(7) .NE. 0 .AND. irun .NE. 0 ) THEN
          PRINT *,' ***  WARNING  ***  RP is zero and RP trace number',
 
     &         ' is non-zero.' 
          PRINT *,' DISKIN thinks this file is sorted by RP.'
          PRINT *,' Use DISKIN parameters NO and TR or SORT to read',
     &         ' properly.'
          PRINT *,' Use process HEADER to set the rp trace number to 0.'
      ENDIF
      IF( lbuf(3) .EQ. 0 .AND. lbuf(4) .EQ. 0 .AND. lbuf(6) .EQ. 0 .AND.
     &    lbuf(7) .EQ. 0 .AND. renum .EQ. 0 .AND. retrac .EQ. 0 .AND.
     &    jform .NE. 9 .AND. jform .NE. 13 .AND. jform .NE. 14 .AND. 
     &    irun .NE. 0 ) THEN
          PRINT *,' ***  WARNING  ***  The shot/rp number and trace ',
     &       'numbers are all zero!   Try parameters RENUM and RETRAC.'
      ENDIF
c****
c****  Do some other segy header and user parameter combinations
c****
      IF( noinc .NE. 99999 .AND. ltr .NE. 0 .AND. jntrcs .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  *** NOINC given when SEGY ntrcs = 0.'
          PRINT *,' Use DISKIN parameter NTRCS or FTR/LTR.'
         ierror = ierror + 1
      ENDIF
c****
c****    set the defaults
c****
      fno = 0
      lno = 0
c****
c****    finish up the parameter reading
c****
 2000 CONTINUE
      CALL getoke( token, nchars )                                       ! get the next token
      CALL upcase( token, nchars )
      ntokes = ntokes + 1
      IF( nchars .LE. 0 ) THEN
          IF( now .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL rdline                                                   ! get a new line of parameters
          ntokes = 0
          GOTO 2000
      ENDIF
c**** set fgmt to 0 so that if multiple days we get all of the second
c**** day.  Keep lgmt as the user's lgmt in case there are multiple
c**** lists.
      fgmt = 0                                                          ! fgmt and lgmt are defaults
c      lgmt = 2500
      IF( token(1:nchars) .NE. 'END' .OR. nchars .NE. 3 ) GOTO 110
c****
c****  This next kludge is because plot edit uses the trace header to construst
c****  the plot header.  I haven't read the SSC file, so I don't have a header
c****  to make.  Screw SSC, just make it all zero.  The EBCDIC header is also
c****  needed by ploted, but screw that too, let it decide that it is non EBCDIC
c****
      IF( jform .EQ. 2 ) THEN
          DO 2010 i = 1, 60
 2010     lbuf(i) = 0
      ENDIF
      RETURN
      END
