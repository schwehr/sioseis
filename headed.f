      SUBROUTINE headed( numhdr )
c
c
c  Copyright (C) The Regents of the University of California
c  Written by:  Paul Henkart, Scripps Institution of Oceanography
c
c  mod 24 Jan 92 - Allow successive lists to have the SAME fno/lno.
c  mod 16 Aug 92 - VMS didn't work becuase of disk I/O in EBCDIC header
c  mod 19 Oct 92 - Add parameter HEADER.
c  mod 15 Mar 95 - Add parameter BHDR
c  mod 16 Mar 95 - Add parameter XN1
c  mod 22 May 95 - Correct bugs with ihdr/lhdr/hdr when BHDR was done.
c  mod 10 Jun 95 - Add parameter CLEAN
c  mod 5 Apr 96  - Don't write to header unit if process header parameters
c                  precede the parameters for an input process such as diskin.
c  mod 23 Oct 96 - Added error check for bad parameter HEADER
c  mod 23 Dec 96 - Allow math operations on up to 10 header values.
c  mod 11 Mar 98 - Don't do BHDR if header is NOT in the procs list
c  mod 27 Jun 00 - HDR converted it to integer.
c  mod 20 June 2001 - Allow multiple header processes and make this reentrant.
c  mod 11 Mar 03 - Create parameter INTERP and make it the same as SPAT.
c                - Make the preset for INTERP  NO  when doing equations.
c  mod 6 May 03 - Add parameter REV1 PATHNAME
c  mod 2 June 03 - Only do the binary header stuff if the proper process
c                  HEADER is in the procs list.
c                - variable itype was being used for too many things!
c  mod 27 Apr 06 - BHDR didn't work on Linux because wrdiscb had extra arg
c                - gfortran (linux) doesn't allow GOTO into differnt blocks.
c  mod 25 May 06 - Allow negative fno and lno.
c  mod 24 Jul 06 - Change lno preset to 9999987
c  mod 12 Feb 07 - Add parameter SWAP
c  mod 28 Nov 07 - Add NOINC and TRINC
c  mod 7 May 08 - Change fno/lno presets so that if if only one list and fno/lno
c                 are not given, then do all (fno =0, lno = 99999987).
c                - If fno is given, but lno is not, then set lno = fno.
c  mod 8 Jan 09 - Reel header changes (CXX) didn't work on HEADER2 & HEADER3
c  mod 6 Jun 11 - swap i109 r60 = i109  choked because r60 looks like a swap index
c                 No cure if the r06 is and I or L
c  mod 7 Sep 12 - Use REAL*8 when reading LHDR or L*
c  mod 5 Feb 14 - READ statement had an extra ")" that new compiler choked on
c  mod 14 Apr 14 - add B (byte) to "equation"
c                - Add D (double or 64 bit) to swap
c                - add D (double) to "equation"
c  mod 4 Sep 14 - R on right hand side of equation failed.
c
      PARAMETER ( npars = 21 )                                          ! the number of user parameters
      PARAMETER ( maxrep = 10 * 7 )
      PARAMETER (maxswap = 50 * 2 )
      PARAMETER ( MAXHDR = 3 )
      CHARACTER*80 token, htype, csave
c
      CHARACTER*6 names(npars), pname
      DIMENSION scr(493), lscr(493), ivms(20), iswap(maxswap)         ! 493 = 13+120+120+120+120
      EQUIVALENCE (scr(1),lscr(1)), (token, ivms)
      CHARACTER*80 cheadr(40)
      COMMON /READT/ILUN,NUMTRHDR,NUMDAT,IUNHDR,IREELM,INTRCS,JFMT,NSKIP,
     *    SSECS,RRENUM,ISRCF,IDTYPE,nfskp,jform,itxsi,itxdel
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /header/ iunit(MAXHDR), nlists(MAXHDR), nwrds
      COMMON /porder/ num, iorder(100)
      COMMON /binhdr/ ibhead(200)
      INTEGER*2 ibhead
      LOGICAL iexist
      INTEGER fno, lno, ftr, ltr, type, clean, noinc, trinc
      INTEGER ihdr(120), lhdr(120), hdr(120), dhdr(120), chdr(120)   ! hold the header indices
      INTEGER ireplace(maxrep)
      REAL replace(maxrep)
      REAL*8 dreal
      EQUIVALENCE (ireplace(1),replace(1))
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr, 
     *                 isecptr,  igmtptr,  ldelsptr, lsmusptr,lemusptr,
     *                 lsisptr,  lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr,   ifoldptr, icvleptr, lespnptr
      DATA pname/' '/, nihdr/0/, nlhdr/0/, nhdr/0/, ndhdr/0/, nrep/-7/
      DATA names /'FNO   ','LNO   ','FTR   ','LTR   ','IHDR  ',
     &            'LHDR  ','HDR   ','LPRINT','C     ','SPAT  ' ,
     &            'TYPE  ','LTYPE ','ITYPE ','HEADER','BHDR  ',
     &            'CLEAN ','INTERP','REV1  ','SWAP  ','NOINC ',
     &            'TRINC ' /
c****
c****    Set the parameter presets and various variable presets
c****
      fno = -1
      lno = 9999987
      ftr = 1
      ltr = 9999987
      lprint = 0
      ispat = -1
      interp = -1
      type = 1                                                          ! REPLACE
      ltype = 1                                                         ! REPLACE
      itype = 1                                                         ! REPLACE
      nbhdr = 0
      clean = 0
      lunrev1 = 0
      nswap = 0
      noinc = 9999999
      trinc = 9999
      DO i = 1, 369
         lscr(i) = 0
      ENDDO
      nlists(numhdr) = 0
      DO i = 1, 120
         hdr(i) = 0.
         lhdr(i) = 0
         ihdr(i) = 0
         dhdr(i) = 0
         chdr(i) = 0
      ENDDO
      DO i = 1, maxrep
         ireplace(i) = 0
      ENDDO
      DO i = 1, maxswap
         iswap(i) = 0
      ENDDO
      CALL getfil( 1, iunit(numhdr), token, istat )                     ! get a parameter disk file
c****
c****  Get the SEGY header (which is on disk in EBCDIC)
c****    the VMS version of diskio can't read into characetr arrays
c****
      IF( iunhdr .GT. 0 ) THEN
          CALL podisc( iunhdr, 1, 0)
          DO 50 i = 1, 40
             CALL rddisc( iunhdr, ivms, 20, istat )
             cheadr(i) = token
   50     CONTINUE
      ENDIF
c****
c****     get the user's parameters
c****
  100 CONTINUE
      CALL getoke( token, nchars )                                      ! get a token and it's length
      CALL upcase( token, nchars )                                      ! convert parameter names to upper case
      IF( nchars .EQ. 0 ) THEN                                          ! anything there?
          CALL rdline                                                   ! nope, get another line
          GOTO 100
      ENDIF
  110 CONTINUE
      IF( token(1:1) .EQ. 'C' .AND. nchars .EQ. 3 ) THEN
          pname = ' '
          READ( token, '(1X,I2)' ) i
          cheadr(i)(1:4) = token(1:3)
          CALL getoke( token, nchars )
          cheadr(i)(5:80) = token(1:nchars)
          CALL ascebc( cheadr(i), 80, cheadr(i) )
          GOTO 100
      ENDIF
  129 CONTINUE
      DO 200 nparam = 1, npars
         IF( token(1:nchars) .EQ. names(nparam) ) THEN                  ! find the parameter name in our list
             IF( last_bhdr .EQ. 1 ) GOTO 128
             nbhdr = 0
             pname = token(1:nchars)
             IF( pname .EQ. 'IHDR' .OR. pname .EQ. 'LHDR' .OR.
     &           pname .EQ. 'HDR' .OR. pname .EQ. 'HEADER' .OR.
     &           pname .EQ. 'SWAP' ) GOTO 100
  120        CALL getoke( token, nchars )                               ! get the value
             IF( nchars .EQ. 0 ) THEN
                 CALL rdline
                 GOTO 120
             ENDIF
             ntokes = ntokes + 1
             IF( names(nparam) .EQ. 'REV1' ) THEN
                 INQUIRE( FILE=token, EXIST=iexist)
                 IF( iexist ) THEN
                    CALL getfil( 2, lunrev1, token, istat )
                    OPEN( UNIT=lunrev1, FILE=token )
                 ELSE
                     lunrev1 = -1
                 ENDIF
                 GOTO 100
             ENDIF
             CALL upcase( token, nchars )
             IF( names(nparam) .EQ. 'SPAT' ) THEN
                 IF( token(1:1) .EQ. 'N' ) ispat = 0
                 IF( token(1:1) .EQ. 'Y' ) ispat = 1
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'INTERP' ) THEN
                 IF( token(1:1) .NE. 'N' .AND. token(1:1) .NE. 'Y') THEN
                     PRINT *,' ***  ERROR  ***  Bad INTERP (yes or no).'
                     ierror = ierror + 1
                 ENDIF
                 IF( token(1:1) .EQ. 'N' ) interp = 0
                 IF( token(1:1) .EQ. 'Y' ) interp = 1
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'TYPE' ) THEN
                 IF( token(1:nchars) .EQ. 'REPLACE' ) type = 1
                 IF( token(1:nchars) .EQ. 'ADD' ) type = 2
                 IF( token(1:nchars) .EQ. 'MULTIPLY' ) type = 3
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'LTYPE' ) THEN
                 IF( token(1:nchars) .EQ. 'REPLACE' ) ltype = 1
                 IF( token(1:nchars) .EQ. 'ADD' ) ltype = 2
                 IF( token(1:nchars) .EQ. 'MULTIPLY' ) ltype = 3
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'ITYPE' ) THEN
                 IF( token(1:nchars) .EQ. 'REPLACE' ) itype = 1
                 IF( token(1:nchars) .EQ. 'ADD' ) itype = 2
                 IF( token(1:nchars) .EQ. 'MULTIPLY' ) itype = 3
                 GOTO 100
             ENDIF
  128        CONTINUE
             IF( names(nparam) .EQ. 'BHDR' ) THEN
                 IF( last_bhdr .EQ. 1 ) THEN
                     nchars = isave
                     token = csave
                 ENDIF
                 nbhdr = nbhdr + 1
                 CALL podiscb( iunhdr, 0, 3200 )
                 CALL rddiscb( iunhdr, ibhead, 400, istat )
                 CALL dcode( token, nchars, areal, istat )
                 IF( istat .NE. 2 ) THEN
                     nbhdr = 0
                     CALL upcase( token, nchars )
                     ierror = ierror + 1
                     GOTO 200
                 ENDIF
                 index = NINT(areal)
  130            CALL getoke( token, nchars )
                 IF( nchars .EQ. 0 ) THEN
                     CALL rdline
                     GOTO 130
                 ENDIF
                 CALL dcode( token, nchars, areal, istat )
                 IF( istat .NE. 2 ) THEN
                     nbhdr = 0
                     CALL upcase( token, nchars )
                     ierror = ierror + 1
                     GOTO 200
                 ENDIF
                 IF( index .LE. 0 .OR. index .GT. 200 ) THEN
                      PRINT *,' ***  ERROR  ***  Bad SEGY index.'
                      ierror = ierror + 1
                      GOTO 100
                 ENDIF
                 ibhead(index) = NINT(areal)
                 DO i = 1, num
                    IF( (iorder(i) .EQ. 42 .AND. numhdr .EQ. 1) .OR.
     &                  (iorder(i) .EQ. 84 .AND. numhdr .EQ. 2) .OR.
     &                  (iorder(i) .EQ. 85 .AND. numhdr .EQ. 3) ) THEN
                        CALL podiscb( iunhdr, 0, 3200 )
                        CALL wrdiscb( iunhdr, ibhead, 400 )
                        GOTO 100
                    ENDIF
                 ENDDO
                 GOTO 100
             ENDIF
             CALL dcode( token, nchars, areal, istat )                  ! convert the alpha number to an internal machine number
             IF( istat .NE. 2 ) ierror = ierror + 1
             IF( names(nparam) .EQ. 'FNO' ) fno = NINT(areal)
             IF( names(nparam) .EQ. 'LNO' ) lno = NINT(areal)
             IF( names(nparam) .EQ. 'NOINC' ) noinc = NINT(areal)
             IF( names(nparam) .EQ. 'FTR' ) ftr = NINT(areal)
             IF( names(nparam) .EQ. 'LTR' ) ltr = NINT(areal)
             IF( names(nparam) .EQ. 'TRINC' ) trinc = NINT(areal)
             IF( names(nparam) .EQ. 'LPRINT' ) lprint = NINT(areal)
             IF( names(nparam) .EQ. 'CLEAN' ) clean = NINT(areal)
             GOTO 100
         ENDIF
  200 CONTINUE
      IF( token(1:nchars) .NE. 'END' ) THEN
          IF( pname .EQ. 'HDR' .OR. pname .EQ. 'IHDR' .OR. 
     &        pname .EQ. 'LHDR' ) THEN
              IF( token(1:1) .EQ. 'L' .OR.  token(1:1) .EQ. 'R' .OR.
     &            token(1:1) .EQ. 'I' ) GOTO 230
              CALL ddcode( token, nchars, dreal, istat )
              IF( istat .NE. 2 ) ierror = ierror + 1
              IF( pname .EQ. 'IHDR' ) THEN
                  nihdr = nihdr + 1
                  ihdr(nihdr) = NINT(dreal)
              ENDIF
              IF( pname .EQ. 'LHDR' ) THEN
                  nlhdr = nlhdr + 1
                  lhdr(nlhdr) = NINT(dreal)
              ENDIF
              IF( pname .EQ. 'HDR' ) THEN
                  nhdr = nhdr + 1
                  hdr(nhdr) = dreal
              ENDIF
              IF( pname .EQ. 'DHDR' ) THEN
                  ndhdr = ndhdr + 1
                  dhdr(ndhdr) = dreal
              ENDIF
              GOTO 100
          ENDIF
          IF( pname .EQ. 'SWAP' ) THEN
              IF( token(1:1) .EQ. 'I' .OR. token(1:1) .EQ. 'L' .OR.
     &            token(1:1) .EQ. 'D' ) THEN
                  nswap = nswap + 1
                  IF( token(1:1) .EQ. 'I' ) iswap(nswap) = 1
                  IF( token(1:1) .EQ. 'L' ) iswap(nswap) = 2
                  IF( token(1:1) .EQ. 'D' ) iswap(nswap) = 3
                  token(1:nchars) = token(2:nchars+1)
                  CALL dcode( token, nchars-1, areal, istat )
                  nswap = nswap + 1
                  iswap(nswap) = NINT(areal)
                  IF( areal .LT. 1 .OR. areal .GT. 120 ) THEN
                      PRINT *,' ***  ERROR  ***  SWAP index must be > 0
     &and < 121.'
                      ierror = ierror + 1
                  ENDIF
                  GOTO 100
c                  nswap = nswap -1
c                  PRINT *,' ***  ERROR  ***  SWAP indices must start wit
c     &h I or L.'
c                  ierror = ierror + 1
              ENDIF
          ENDIF
          IF( pname .EQ. 'HEADER' .OR. pname .EQ. 'HEADER2' .OR.
     &        pname .EQ. 'HEADER3' ) THEN
              IF( token(1:nchars) .EQ. 'SHOTNO' .OR.
     &            token(1:nchars) .EQ. 'SHOTTR' .OR.
     &            token(1:nchars) .EQ. 'RPNO' .OR.
     &            token(1:nchars) .EQ. 'RPTR' .OR.
     &            token(1:nchars) .EQ. 'RANGE' .OR.
     &            token(1:nchars) .EQ. 'FOLD' .OR.
     &            token(1:nchars) .EQ. 'DELAY' .OR.
     &            token(1:nchars) .EQ. 'SI' .OR.
     &            token(1:nchars) .EQ. 'WBT' .OR.
     &            token(1:nchars) .EQ. 'WBD'
     &            ) THEN
                  htype = token(1:nchars)
  220             CALL getoke( token, nchars )                              ! get the value
                  IF( nchars .EQ. 0 ) THEN
                      CALL rdline
                      GOTO 220
                  ENDIF
                  ntokes = ntokes + 1
                  CALL upcase( token, nchars )
                  CALL dcode( token, nchars, areal, istat )
                  IF( istat .NE. 2 ) ierror = ierror + 1
                  IF( htype .EQ. 'SHOTNO' ) THEN
                      nlhdr = nlhdr + 1
                      lhdr(nlhdr) = lshotptr
                      nlhdr = nlhdr + 1
                      lhdr(nlhdr) = NINT(areal)
                  ENDIF
                  IF( htype .EQ. 'SHOTTR' ) THEN
                      nlhdr = nlhdr + 1
                      lhdr(nlhdr) = lshtrptr
                      nlhdr = nlhdr + 1
                      lhdr(nlhdr) = NINT(areal)
                  ENDIF
                  IF( htype .EQ. 'RPNO' ) THEN
                      nlhdr = nlhdr + 1
                      lhdr(nlhdr) = lrpnptr
                      nlhdr = nlhdr + 1
                      lhdr(nlhdr) = NINT(areal)
                  ENDIF
                  IF( htype .EQ. 'RPTR' ) THEN
                      nlhdr = nlhdr + 1
                      lhdr(nlhdr) = lrptrptr
                      nlhdr = nlhdr + 1
                      lhdr(nlhdr) = NINT(areal)
                  ENDIF
                  IF( htype .EQ. 'FOLD' ) THEN
                      nihdr = nihdr + 1
                      ihdr(nihdr) = ifoldptr
                      nihdr = nihdr + 1
                      ihdr(nihdr) = NINT(areal)
                  ENDIF
                  IF( htype .EQ. 'RANGE' ) THEN
                      nlhdr = nlhdr + 1
                      lhdr(nlhdr) = ldisptr
                      nlhdr = nlhdr + 1
                      lhdr(nlhdr) = NINT(areal)
                  ENDIF
                  IF( htype .EQ. 'DELAY' ) THEN
                      nihdr = nihdr + 1
                      ihdr(nihdr) = idelmptr
                      nihdr = nihdr + 1
                      ihdr(nihdr) = NINT(areal * 1000.)
                      nhdr = nhdr + 1
                      hdr(nhdr) = ldelsptr
                      nhdr = nhdr + 1
                      hdr(nhdr) = NINT(areal)
                  ENDIF
                  IF( htype .EQ. 'SI' ) THEN
                      nihdr = nihdr + 1
                      ihdr(nihdr) = isiptr
                      nihdr = nihdr + 1
                      ihdr(nihdr) = NINT(areal * 100000.) 
                      nhdr = nhdr + 1
                      hdr(nhdr) = lsisptr
                      nhdr = nhdr + 1
                      hdr(nhdr) = NINT(areal)
                  ENDIF
                  IF( htype .EQ. 'WBT' ) THEN
                      nhdr = nhdr + 1
                      hdr(nhdr) = lwbtsptr
                      nhdr = nhdr + 1
                      hdr(nhdr) = NINT(areal)
                  ENDIF
                  IF( htype .EQ. 'WBD' ) THEN
                      nlhdr = nlhdr + 1
                      lhdr(nlhdr) = lwbdptr
                      nlhdr = nlhdr + 1
                      lhdr(nlhdr) = NINT(areal)
                  ENDIF
              ELSE
                  PRINT *,' ***  ERROR  ***  ',token(1:nchars),
     &               ' is not a legal type for parameter HEADER.'
                  ierror = ierror + 1
              ENDIF
              GOTO 100
          ENDIF
          IF( nhdr .GT. 120 .OR. nihdr .GT. 120 .OR. nlhdr .GT.120) THEN
              PRINT *,' ***  ERROR  ***  60 entries max.'
              ierror = ierror + 1
              nhdr = 0
              nihdr = 0
              nlhdr = 0
          ENDIF
  230     IF( token(1:1) .EQ. 'I' .OR. token(1:1) .EQ. 'L' .OR.
     &        token(1:1) .EQ. 'D' .OR. token(1:1) .EQ. 'B' .OR.
     &        token(1:1) .EQ. 'R' ) THEN
              nrep = nrep + 7
              IF( nrep .GT. maxrep ) THEN
                  PRINT *,' ***  ERROR  ***  Only ',maxrep/7,
     &               ' XN parameters may be given per list.'
                  ierror = ierror + 1
              ENDIF
              IF( token(1:1) .EQ. 'I' ) itemp = 1
              IF( token(1:1) .EQ. 'L' ) itemp = 2
              IF( token(1:1) .EQ. 'R' ) itemp = 3
c    constant not allowed on the left hand side of an equation!
              IF( token(1:1) .EQ. 'D' ) itemp = 5
              IF( token(1:1) .EQ. 'B' ) itemp = 6
              token(1:nchars) = token(2:nchars)
              nchars = nchars - 1
              CALL dcode( token, nchars, areal, istat )
              IF( istat .NE. 2 ) THEN
                  ierror = ierror + 1
                  GOTO 100
              ENDIF
              index = NINT(areal)
              ireplace(nrep+1) = itemp
              ireplace(nrep+2) = index
              IF( token(nchars:nchars) .EQ. '=' ) GOTO 250
  240         CALL getoke( token, nchars )
              IF( nchars .EQ. 0 ) THEN
                  CALL rdline
                  GOTO 240
              ENDIF
c              IF( token(1:nchars) .NE. '=' ) THEN
              IF( token(1:nchars) .NE. '=' ) GOTO 251
c                  PRINT *,' ***  ERROR  ***  Missing required =.'
c                  ierror = ierror + 1
c                  GOTO 100
c              ENDIF
  250         CALL getoke( token, nchars )
  251         IF( nchars .EQ. 0 ) THEN
                  CALL rdline
                  GOTO 250
              ENDIF
              CALL upcase( token, nchars )
              jtype = 0
              IF( token(1:1) .EQ. 'I' ) jtype = 1
              IF( token(1:1) .EQ. 'L' ) jtype = 2
              IF( token(1:1) .EQ. 'R' ) jtype = 3
c    a constant doesn't have a letter!  jtupe = 4
              IF( token(1:1) .EQ. 'D' ) jtype = 5
              IF( token(1:1) .EQ. 'B' ) jtype = 6
              IF( jtype .EQ. 0 ) THEN
c****             might be a constant
                  CALL dcode( token, nchars, constant, istat )
                  IF( istat .NE. 2 ) THEN
                      PRINT *,' ***  ERROR  ***  ',token,
     &             ' should have been IH, LH, RH, DH, BH or a constant.'
                      ierror = ierror + 1
                  ENDIF
                  ireplace(nrep+3) = 4
                  replace(nrep+4) = constant
c****             We have a problem Houston.  This was a constant.  It
c****             may or may not be the last part of the equation.
c****             The next thing must be an operation (+,-,*,/) or else
c****             it's the end of the equation.
                  CALL getoke( token, nchars )
                  CALL upcase( token, nchars )
                  IF( token(1:1) .NE. '+' .AND. token(1:1) .NE. '-'
     &                .AND. token(1:1) .NE. '*' 
     &                .AND. token(1:1) .NE. '**' 
     &                .AND. token(1:1) .NE. '/' ) GOTO 110
                  GOTO 261
              ENDIF
              token(1:nchars) = token(2:nchars)
              nchars = nchars - 1
              CALL dcode( token, nchars, areal, istat )
              IF( istat .NE. 2 ) THEN
                  ierror = ierror + 1
                  GOTO 100
              ENDIF
              index = NINT(areal)
              ireplace(nrep+3) = jtype
              ireplace(nrep+4) = index
  260         CALL getoke( token, nchars )
  261         IF( nchars .EQ. 0 ) THEN
                  CALL rdline
                  GOTO 260
              ENDIF
              CALL upcase( token, nchars )
              IF( token(1:1) .NE. '+' .AND. token(1:1) .NE. '-' .AND.
     &            token(1:1) .NE. '*' .AND. token(1:1) .NE. '/' .AND.
     &            token(1:1) .NE. '**' ) GOTO 110
              jtype = 0
              IF( token(1:nchars) .EQ. '+' ) jtype = 1
              IF( token(1:nchars) .EQ. '-' ) jtype = 2
              IF( token(1:nchars) .EQ. '*' ) jtype = 3
              IF( token(1:nchars) .EQ. '/' ) jtype = 4
              IF( token(1:nchars) .EQ. '**' ) jtype = 5
              ireplace(nrep+5) = jtype
  270         CALL getoke( token, nchars )
              IF( nchars .EQ. 0 ) THEN
                  CALL rdline
                  GOTO 270
              ENDIF
              CALL upcase( token, nchars )
              jtype = 0
              IF( token(1:1) .EQ. 'I' ) jtype = 1
              IF( token(1:1) .EQ. 'L' ) jtype = 2
              IF( token(1:1) .EQ. 'R' ) jtype = 3
c    a constant doesn't have a letter!  jtupe = 4
              IF( token(1:1) .EQ. 'D' ) jtype = 5
              IF( token(1:1) .EQ. 'B' ) jtype = 6
              IF( jtype .EQ. 0 ) THEN
c****             might be a constant
                  CALL dcode( token, nchars, constant, istat )
                  IF( istat .NE. 2 ) THEN
                      PRINT *,' ***  ERROR  ***  ',token,
     &             ' should have been IH, LH, RH, DH, BH or a constant.'
                      ierror = ierror + 1
                  ENDIF
                  ireplace(nrep+6) = 4
                  replace(nrep+7) = constant
                  GOTO 100
              ENDIF
              token(1:nchars) = token(2:nchars)
              nchars = nchars - 1
              CALL dcode( token, nchars, areal, istat )
              IF( istat .NE. 2 ) THEN
                  ierror = ierror + 1
                  GOTO 100
              ENDIF
              index = NINT(areal)
              ireplace(nrep+6) = jtype
              ireplace(nrep+7) = index
              GOTO 100
          ENDIF
          IF( nchars .EQ. 0 ) GOTO 100
          last_bhdr = 0
          IF( token(1:nchars) .NE. 'END') THEN
              IF( nbhdr .EQ. 0 ) THEN
                  PRINT *,' ***  ERROR  ***  No such parameter as ',
     *                  token(1:nchars)
                  ierror = ierror + 1
              ELSE
                  last_bhdr = 1
                  csave = token
                  isave = nchars
                  token = 'BHDR'
                  nchars = 4
              ENDIF
          ENDIF
          IF( last_bhdr .EQ. 1 ) GOTO 129
          GOTO 100
      ENDIF
c****
c****    DO some error checking
c****
      IF( MOD(nhdr,2) .NE. 0 ) THEN
          PRINT *,' ***  ERROR  ***  HDR must be in pairs.'
          ierror = ierror + 1
      ENDIF
      IF( MOD(nihdr,2) .NE. 0 ) THEN
          PRINT *,' ***  ERROR  ***  IHDR must be in pairs.'
          ierror = ierror + 1
      ENDIF
      IF( MOD(nlhdr,2) .NE. 0 ) THEN
          PRINT *,' ***  ERROR  ***  LHDR must be in pairs.'
          ierror = ierror + 1
      ENDIF
      IF( lno .LT. fno ) THEN
          PRINT *,' ***  ERROR  *** LNO must be larger than FNO.'
          ierror = ierror +1
      ENDIF
      IF( ltr .LT. ftr ) THEN
          PRINT *,' ***  ERROR  ***  LTR must be larger than ftr.'
          ierror = ierror +1
      ENDIF
      IF( nswap .GT. maxswap ) THEN
          PRINT *,' ***  ERROR  ***  Too many items to swap.  Max = 50.'
          ierror = ierror +1
      ENDIF
      IF( nlists(numhdr) .GT. 0 ) THEN
          IF( fno .LT. lastlno ) THEN
              PRINT *,' ***  ERROR  ***  FNO must increase.'
              ierror = ierror + 1
          ENDIF
      ENDIF
      IF( lno .EQ. 9999987 ) THEN
          lastlno = fno
      ELSE
          lastlno = lno
      ENDIF
      IF( nlists(numhdr) .GT. 1 .AND. fno .EQ. -1 ) THEN
          PRINT *,
     &        ' ***  ERROR  ***  FNO must be given when multiple lists.'
          ierror = ierror +1
      ENDIF
      DO i = 1, 120, 2
         IF( ihdr(i) .LT. 0 .OR. ihdr(i) .GT. 120 ) THEN
             PRINT *,' ***  ERROR  ***  Bad IHDR index of ',
     &              ihdr(i)
             ierror = ierror + 1
         ENDIF
         IF( lhdr(i) .LT. 0 .OR. lhdr(i) .GT. 60 ) THEN
             PRINT *,' ***  ERROR  ***  Bad LHDR index of ',
     &              lhdr(i)
             ierror = ierror + 1
         ENDIF
         IF( hdr(i) .LT. 0 .OR. hdr(i) .GT. 60 ) THEN
             PRINT *,' ***  ERROR  ***  Bad HDR index of ',
     &              hdr(i)
             ierror = ierror + 1
         ENDIF
       ENDDO
c****
c****  INTERP/SPAT preset is on except when equations are used.  And,
c****  SPAT should have been named INTERP and I have to leave it in
c****  so old scripts will work.  sh!@#^&*
       IF( ispat .LT. 0 ) THEN
           IF( interp .LT. 0 ) THEN
               interp = 1
               IF( nrep .GT. -7 ) interp = 0
           ENDIF
           ispat = interp
       ENDIF
c****
c****    Write the list to disc
c****
      lscr(1) = fno
      lscr(2) = lno
c      IF( lno .EQ. 9999987 ) THEN
c          lscr(2) = fno
c      ELSE
c          lscr(2) = lno
c      ENDIF
      lscr(3) = ftr
      lscr(4) = ltr
      lscr(5) = lprint
      lscr(6) = ispat
      lscr(7) = type
      lscr(8) = ltype
      lscr(9) = itype
      lscr(10) = clean
      lscr(11) = lunrev1
      lscr(12) = noinc
      lscr(13) = trinc
      DO 1000 i = 1, 120
         lscr(13+i) = ihdr(i)         ! these are indices
         lscr(13+120+i) = lhdr(i)
         lscr(13+120+120+i) = hdr(i)
         lscr(13+120+120+120+i) = dhdr(i)  
 1000 CONTINUE
      nwrds = 13 + 120 + 120 + 120 + 120 + 120
      nlists(numhdr) = nlists(numhdr) + 1
      CALL wrdisc( iunit(numhdr), scr, nwrds )
      CALL wrdisc( iunit(numhdr), ireplace, maxrep )
      CALL wrdisc( iunit(numhdr), iswap, maxswap )
      nwrds = nwrds + maxrep + maxswap
      IF( IAND(lprint,1) .NE. 0 ) THEN
          PRINT *,(lscr(i),i=1,13)
          PRINT *,' SHORT:'
          PRINT *,(lscr(i),i=14,133)
          PRINT *,' LONG:'
          PRINT *,(lscr(i),i=134,253)
          PRINT *,' FLOAT'
          PRINT *,(lscr(i),i=254,373)
          PRINT *,' DOUBLE'
          PRINT *,(lscr(i),i=374,493)
          PRINT *,' BYTE'
          PRINT *,(lscr(i),i=494,613)
          PRINT *,' ireplace: ',(ireplace(i),i=1,10)
          PRINT *,' SWAP: ',(iswap(i),i=1,10)
      ENDIF
c****
c****    finish up the parameter reading
c****
 2000 CONTINUE
      fno = -1
      lno = 9999987
      nihdr = 0
      nlhdr = 0
      nhdr = 0
      ndhdr = 0
      lunrev1 = 0
      DO 2010 i = 1, 370
 2010 lscr(i) = 0
      DO 2020 i = 1, 120
         hdr(i) = 0.
         lhdr(i) = 0
         ihdr(i) = 0
         dhdr(i) = 0
         chdr(i) = 0
 2020 CONTINUE
      pname = ' '
      nrep = -7
      DO i = 1, maxrep
         ireplace(i) = 0
      ENDDO
      DO i = 1, nswap
         iswap(i) = 0
      ENDDO
      nswap = 0
      CALL getoke( token, nchars )                                      ! get the next token
      IF( nchars .LE. 0 ) THEN
          IF( now .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL rdline                                                   ! get a new line of parameters
          ntokes = 0
          GOTO 2000
      ENDIF
      CALL upcase( token, nchars )
      ntokes = ntokes + 1
      IF( token(1:nchars) .NE. 'END' .OR. nchars .NE. 3 ) GOTO 110
c****
c**** Write the SEGY header back to disc, if it's been read!
c****
      IF( iunhdr .GT. 0 ) THEN
          DO i = 1, num
             IF( iorder(i) .EQ. 42 .OR. iorder(i) .EQ. 84 .OR.
     &           iorder(i) .EQ. 85 ) THEN
                 CALL podisc( iunhdr, 1, 0 )
                 DO j = 1, 40
                    token = cheadr(j)
                    CALL wrdisc( iunhdr, ivms, 20 )
                 ENDDO
             ENDIF
          ENDDO
      ENDIF
c
      RETURN
      END
