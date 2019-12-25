       SUBROUTINE segdded( ibuf, buf, lbuf, scr )
c
c
c  Copyright (C) University of California
c  ALL RIGHTS RESERVED.
c
c  Programmer notes:
c  I read the first shot in the edit phase for sevreal reasons:
c 1)  I think some other edits need things like the sample interval and data length.
c 2)  By doing the SEGY header stuff here, the final execution phase is not
c     burdened with the slop
c  This does cause a headache because either the edit passes some stuff to
c  the execution phase or the edit has to reformat the trace(s).  Therefore,
c  The edit doesn't reformat/demultiplex the data, it passes such info as
c  the number of scans per tape block on to the execution.
c
c   mod 1 Oct 02 - Add parameter LDEOLIST
c   mod 29 Oct 02 - If fcset = 99, then use all channel sets.
c                 - If lcset > ncsets, then lcset = ncsets
c  Mod 19 Mar 03 - Separate the ldeo external header into it's own lun
c  mod 20 Mar 03 - Make fcset 99 the preset.
c  mod 22 Apr 03 - Print the number of channels in each channel set when fcset=99
c  mod 5 Mar 04 - Add iformat 3 for Hydroscience
c               - Allow Hydrosciences format with a 32 byte file header
c  mod 22 Apr 04 - Allow iformat 4 for SIO-RT.
c                - Allow iformat 5 for Gemetrics file naming FFILEN.sgd
c  mod 4 May 04 - Bad index on byte swap on PC version.
c  mod 28 May 04 - Give error message if multiple parameter lists.
c  mod 27 June 04 - Add parameter DESCALE  (YES/NO, preset NO).
c  mod 27 Sept 04 - Set the sort flag to 1 in the binary header.
c  mod 25 Apr 06 - Bad preset for NSPFILE - was = 100
c  mod 2 Oct 06 - HUH? - Bad preset for NSPFILE - was = 100
c  mod 14 Aug 07 - g95 IAND requires arguments to be same type and kind.
c  mod 16 Apr 08 - Add FORMAT LDEO for using the LDEO external header
c                - Warn if FCSET not given and LCSET is given.
c  mod 21 Apr 08 - Append the LOG file if possible.
c  mod 17 Jul 08 - Add shot_inc (second parameter of parameter STACK)
c                - finish up right if errors
c  mod 18 Apr 11 - Change retrac so that <0 means cumulative trace count,
c                  =0 means use the segd trace number, >0 means use user given.
c  mod 6 Oct 11 - Change "bad" ftr and ltr values into warnings rather than errors.
c  mod 19 Dec 11 - Wait for the STACK file to exist rather than erroring.
c  mod 9 Mar 12 - Close on free the STACK file while waiting so we don't use too many units.
c  mod 13 Aug 12 - Check for LISTPATH file to exist.
c  mod 30 Jan 13 - Use rshift_sio rather than rshift so vacated bits are zeroed.
c  mod 31 Mar 14 - Change DESCALE preset to 1 - DESCALE ON
c  mod 12 Fev 15 - Error if the file doesn't exist.
c
      PARAMETER ( npars =  30 )                                          ! the number of user parameters
      PARAMETER ( maxwrd = 32767 )                                      ! the maximum number of 16 bit words that can be read
      DIMENSION buf(1000)
      INTEGER*2 ibuf(1000), rshift, lshift, i15, i255, rshift_sio
      DATA i15/15/, i255/255/
      INTEGER lbuf(1000), scr(1000)
      CHARACTER*80 cheadr(40)                                           ! the SEGY tape header
      EQUIVALENCE ( cheadr(1), ivms )
      CHARACTER*120 token, pathname
      CHARACTER*8 names(npars), name_last
      CHARACTER*1 types(npars)                                          ! the type of parameter
      REAL vals(npars)                                                  ! holds the REAL parameter values
      DIMENSION lvals(npars)                                            ! holds the INTEGER parameter values
      CHARACTER*4 months(12)
c
      COMMON /readt/ ilun, numhdr, numdat, iunhdr, ireel, intrcs, ifmt,
     *       nskip, sec, lrenum, isrcf, idtype
      COMMON /inputdev/inputdev
      CHARACTER*80 inputdev
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /porder/ num, iorder(40)
      COMMON /segdin/ junit, nlists, nwrds, luntr0, luntr0a, lunldeo,
     &       shot_inc
      COMMON /segddin/ cpath
      CHARACTER*120 cpath
      COMMON /outdev/outdevice
      CHARACTER*80 outdevice
      LOGICAL iexist
c
      INTEGER ffilen, ftr, decimf, filinc, trinc, fday, fgmt, renum,
     &        fcset, gmtinc, tr0, retrac, stack, odevice, descale,
     &        shot_inc
      EQUIVALENCE ( ipath, vals(1) ),
     2            ( lprint, lvals(2) ),
     3            ( ffilen, lvals(3) ),
     4            ( lfilen, lvals(4) ),
     5            ( ftr, lvals(5) ),
     6            ( ltr, lvals(6) ),
     7            ( secs, vals(7) ),
     8            ( decimf, lvals(8) ),
     9            ( filinc, lvals(9) ),
     *            ( trinc, lvals(10) ),
     1            ( fday, lvals(11) ),
     2            ( lday, lvals(12) ),
     3            ( fgmt, lvals(13) ),
     4            ( lgmt, lvals(14) ),
     5            ( ntrgat, lvals(15) ),
     6            ( stime, vals(16) ),
     7            ( renum, lvals(17) ),
     8            ( fcset, lvals(18) ),
     9            ( lcset, lvals(19) ),
     *            ( gmtinc, lvals(20) ),
     1            ( iformat, lvals(21) ),
     2            ( tr0, lvals(22) ),
     3            ( retrac, lvals(23) ),
     4            ( stack, lvals(24) ),
     5            ( listpath, lvals(25) ),
     6            ( logpath, lvals(26) ),
     7            ( nspfile, lvals(27) ),
     8            ( odevice, lvals(28) ),
     9            ( ldeolist, lvals(29) ),
     *            ( descale, lvals(30) )
      DATA names /'IPATH ', 'LPRINT', 'FFILEN', 'LFILEN', 'FTR   ',
     *            'LTR   ', 'SECS  ', 'DECIMF', 'FILINC', 'TRINC ',
     *            'FDAY  ', 'LDAY  ', 'FGMT  ', 'LGMT  ', 'NTRGAT',
     *            'STIME ', 'RENUM' , 'FCSET ', 'LCSET ', 'GMTINC',
     *            'FORMAT', 'TR0   ', 'RETRAC', 'STACK','LISTPATH',
     *            'LOGPATH','NSPFILE','ODEVICE','LDEOLIST','DESCALE' /
      DATA types /'A',5*'L','F',8*'L','F',4*'L',2*'A','L',3*'A','L','A',
     &            'A','A'/
      DATA months/'Jan.','Feb.','Mar.','Apr.','May ','June',
     *            'July','Aug.','Sep.','Oct.','Nov.','Dec.'/
      DATA pathname/' '/
c****
c****    Set the parameter presets and various variable presets
c****
      nlists = 0
      ifile = 0                                                         ! file or shot number
      iunit = 0
      lprint = 0
      ffilen = 99999
      lfilen = 999999999
      ftr = 99999
      ltr = 0
      secs = 0.
      decimf = 1
      filinc = 99999
      trinc = 1
      fday = 0
      lday = 0
      fgmt = 0
      lgmt = 0
      ntrgat = 0
      stime = 0.
      renum = 0
      fcset = 99
      lcset = 0
      gmtinc = 99999
      iformat = 1
      luntr0 = 0
      luntr0a = 0
      lunldeo = 0
      tr0 = 0
      retrac = 0
      stack = 0
      list = 0
      nspfile = 9999999
      lunotape = 0
      lunlog = 0
      odevice = 0
      ldeolist = 0
      descale = 1
      shot_inc = 0
      CALL getfil( 1, junit, token, istat )                             ! get a file for the SEGDIN parameters
      CALL getfil( 1, iunhdr, token, istat )                            ! get an SEGY header file
c         get rid of file in and IN
          itemp = 99
          OPEN(UNIT=ITEMP,FILE='IN',STATUS='UNKNOWN')
          CLOSE(UNIT=ITEMP,STATUS='DELETE')
          OPEN(UNIT=ITEMP,FILE='in',STATUS='UNKNOWN')
          CLOSE(UNIT=ITEMP,STATUS='DELETE')
c****
c****     get the user's parameters -  there must be something, at least an "end"
c****
c      CALL rdline                                                       ! read a line
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
             name_last = names(nparam)
  120        CALL getoke( token, nchars )                               ! get the value
             ntokes = ntokes + 1
             IF( nchars .EQ. 0 ) THEN
                 CALL rdline
                 ntokes = 0
                 GOTO 120
             ENDIF
             IF( types(nparam) .EQ. 'A' ) THEN
                 IF( names(nparam) .EQ. 'IPATH' ) pathname = token
                 IF( names(nparam) .NE. 'STACK' ) name_last = ' '
                 IF( names(nparam) .EQ. 'STACK' ) THEN
                     icount = 1
  125                CALL GETFIL( 4, stack, token, istat )
                     IF( istat .LT. 0 ) THEN
                         PRINT *,' Waiting for STACK file ',
     &                      token(1:nchars),' count=',icount
                         icount = icount + 1
                         CALL frefil(2, stack, istat)   ! close and release iunit
                         CALL SLEEP(5)
                         GOTO 125
                     ENDIF
                     CALL frefil( -2, stack, istat )
                     cpath = token
  130                OPEN( UNIT=stack, FILE=cpath,
     &                     FORM='FORMATTED', STATUS='OLD')
c****            Get the second file
                     CALL rline1(stack)
                     CALL getoke1( token, nchars )
                     CALL rline1(stack)
                     CALL getoke1( token, nchars )
                     CLOSE( UNIT=stack )
c****           the following assumes the format parameter was given
c****           prior to the stack parameter
                     IF( iformat .EQ. 2 .AND.
     &                   token(nchars-4:nchars) .NE. '.SEIS' ) THEN
                         PRINT *,' Waiting for a seisnet file.'
                         CALL sleep(30)
                         GOTO 130
                     ENDIF
                     CALL getfil( 4, iunit, token, istat )
                     IF( istat .NE. 0 ) THEN
                         PRINT *,
     &                   ' ***  WARNING  ***  Waiting for contents of',
     &                       token(1:50)
                         CALL frefil(2, iunit, istat)   ! close and release iunit
                         CALL sleep(30)
                         GOTO 130
                     ENDIF
                     GOTO 100
                 ENDIF
                 IF( names(nparam) .EQ. 'LISTPATH' ) THEN
                     INQUIRE( FILE=token, EXIST=iexist )
                     IF( .NOT. iexist ) THEN
                         PRINT *,' ***  ERROR  ***  File ',
     &                           token(1:nchars),' does not exist.'
                         ierror = ierror + 1
                         GOTO 100
                     ENDIF
c****                list will have the unit number of the list and iunit
c****                will have the unit number of the SEGD data file.
                     CALL GETFIL( 2, list, token, istat )
                     OPEN( UNIT=list, FILE=token,
     &                     FORM='FORMATTED', STATUS='OLD')
c****                GET the first segd file from the list
                     CALL rline1(list)
                     CALL getoke1( token, nchars )
                     pathname = token
                     GOTO 100
                 ENDIF
                 IF( names(nparam) .EQ. 'LOGPATH' ) THEN
                     CALL GETFIL( 2, lunlog, token, istat )
c                     OPEN( UNIT=lunlog, FILE=token, POSITION='APPEND',
                     OPEN( UNIT=lunlog, FILE=token,
     &                     FORM='FORMATTED', STATUS='UNKNOWN')
                     GOTO 100
                 ENDIF
c****            The first call to magtap will assign the tape
                 IF( names(nparam) .EQ. 'ODEVICE' ) THEN
                     outdevice = token(1:nchars)
                     DO i = 1, nchars-1
                        IF( token(i:i+1) .EQ. 'bn' ) GOTO 100
                     ENDDO
                     PRINT *,
     &    ' ***  WARNING  ***  DEVICE should be BSD and No Rewind (bn).'
                     iwarn = iwarn + 1
                     GOTO 100
                 ENDIF
                 IF( names(nparam) .EQ. 'LDEOLIST' ) THEN
                     iformat = 2                                         ! make sure it's SEISNET
                     cpath = token
                     CALL GETFIL( 2, ldeolist, token, istat )
                     IF( istat .NE. 0 ) THEN
                         PRINT *,' *** ERROR ***  Could not open file ',
     &                           cpath
                         ierror = ierror + 1
                         GOTO 100
                     ENDIF
  150                INQUIRE( FILE=token, EXIST=iexist )
                     IF( iexist ) GOTO 160
                     PRINT *,' Waiting for LDEOLIST.'
                     INQUIRE( FILE='in', EXIST=iexist )
                     IF( iexist ) THEN
                         itemp = 99
                         OPEN(UNIT=ITEMP,FILE='in')
                         READ(itemp,'(I4)',END=151,ERR=151)i
                         IF( i .LT. 0 ) THEN
                             PRINT *,' User stopped LDEOLIST.'
                             STOP
                         ENDIF
                     ENDIF
  151                INQUIRE( FILE='IN', EXIST=iexist )
                     IF( iexist ) THEN
                         itemp = 99
                         OPEN(UNIT=ITEMP,FILE='IN')
                         READ(itemp,'(I4)',END=152,ERR=152)i
                         IF( i .LT. 0 ) THEN
                             PRINT *,' User stopped LDEOLIST.'
                             STOP
                         ENDIF
                     ENDIF
  152                CALL SLEEP(10)
                     GOTO 150
  160                OPEN( UNIT=ldeolist, FILE=token,
     &                     FORM='FORMATTED', STATUS='OLD')
                     CALL rline1(ldeolist)
                     CALL getoke1( token, nchars )
                     CALL GETFIL( 4, iunit, token, istat )
                     IF( istat .LT. 0 ) THEN
                         PRINT *,' ***  ERROR  ***  Check LDEOLIST.'
                         PRINT *,' No such file as: ',token
                         ierror = ierror + 1
                     ENDIF
                     GOTO 100
                 ENDIF
                 CALL upcase( token, nchars )
                 IF( names(nparam) .EQ. 'FORMAT' ) THEN
                     IF( token(1:nchars) .EQ. 'SEG-D' ) iformat = 1
                     IF( token(1:nchars) .EQ. 'SEISNET' ) iformat = 2
                     IF( token(1:nchars) .EQ. 'HTI' ) iformat = 3
                     IF( token(1:nchars) .EQ. 'SIO-RT' ) iformat = 4
                     IF( token(1:nchars) .EQ. 'GEOMETRICS' ) iformat = 5
                     IF( token(1:nchars) .EQ. 'LDEO' ) iformat =6
                 ENDIF
                 IF( names(nparam) .EQ. 'TR0' ) THEN
                     IF( token(1:nchars) .EQ. 'YES' ) tr0 = 1
                     IF( token(1:nchars) .EQ. 'NO' ) tr0 = 0
                     IF( token(1:nchars) .EQ. 'ON' ) tr0 = 1
                     IF( token(1:nchars) .EQ. 'OFF' ) tr0 = 0
                     IF( token(1:nchars) .EQ. '1' ) tr0 = 1
                     IF( token(1:nchars) .EQ. '0' ) tr0 = 0
                 ENDIF
                 IF( names(nparam) .EQ. 'DESCALE' ) THEN
                     IF( token(1:nchars) .EQ. 'YES' ) descale = 1
                     IF( token(1:nchars) .EQ. 'NO' ) descale = 0
                     IF( token(1:nchars) .EQ. 'ON' ) descale = 1
                     IF( token(1:nchars) .EQ. 'OFF' ) descale = 0
                     IF( token(1:nchars) .EQ. '1' ) descale = 1
                     IF( token(1:nchars) .EQ. '0' ) descale = 0
                 ENDIF
                 GOTO 100
             ENDIF
             CALL dcode( token, nchars, areal, istat )                  ! convert the alpha number to an internal machine number
             IF( istat .NE. 2 ) ierror = ierror + 1                     ! was the an error decoding it?
             IF( types(nparam) .EQ. 'L' ) THEN
                 lvals(nparam) = areal                                  ! convert the real to INTEGER*4
             ELSE
                 vals(nparam) = areal                                   ! move the real to the parameter
             ENDIF
             GOTO 100
         ENDIF
  200 CONTINUE
      CALL upcase( token, nchars )
      IF( token(1:nchars) .NE. 'END') THEN
          IF( name_last(1:5) .EQ. 'STACK' ) THEN
              IF( token(1:nchars) .EQ. 'ODD' ) shot_inc = 1
              IF( token(1:nchars) .EQ. 'EVEN' ) shot_inc = 2
              IF( shot_inc .EQ. 0 ) THEN
        PRINT *,' ***  ERROR  ***  STACK INCREMENT must be ODD or EVEN',
     *            token(1:nchars)
                  ierror = ierror + 1
              ENDIF
          ELSE
              PRINT *,' ***  ERROR  ***  No such parameter as ',
     *          token(1:nchars)
              ierror = ierror + 1
          ENDIF
          GOTO 100
      ENDIF
c****
c****    Do the parameter validity checks
c****
      IF( fday .LT. 0 .OR. fday .GT. 366 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal FDAY value of ',fday
          ierror = ierror + 1
      ENDIF
      IF( lday .LT. 0 .OR. lday .GT. 366 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal LDAY value of ',lday
          ierror = ierror + 1
      ENDIF
      IF( fgmt .LT. 0 .OR. fgmt .GT. 2400 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal FGMT value of ',fgmt
          ierror = ierror + 1
      ENDIF
      IF( lgmt .LT. 0 .OR. lgmt .GT. 2400 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal LGMT value of ',lgmt
          ierror = ierror + 1
      ENDIF
      IF( ftr .LT. 0 .OR. ftr .GT. 99999 ) THEN
          PRINT *,' ***  WARNING  ***  FTR value of ',ftr,' may be bad.'
          iwarn = iwarn + 1
      ENDIF
      IF( ltr .LT. 0 .OR. ltr .GT. 10000 ) THEN
          PRINT *,' ***  WARNING  ***  LTR value of ',ltr,' may be bad.'
          iwarn = iwarn + 1
      ENDIF
      IF( secs .LT. 0 .OR. secs .GT. 99 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal SECS value of ',secs
          ierror = ierror + 1
      ENDIF
      IF( decimf .LT. 1 .OR. decimf .GT. 20 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal DECIMF value of ',decimf
          ierror = ierror + 1
      ENDIF
      IF( ntrgat .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  NTRGAT must be positive.'
          ierror = ierror
      ENDIF
      IF( stime .LT. 0. ) THEN
          PRINT *,' ***  ERROR  ***  STIME must be positive.'
          ierror = ierror + 1
      ENDIF
      IF( stime .GT. 20. ) THEN
          PRINT *,' ***  WARNING  ***  STIME ',stime,' is unusually ',
     *      'large.'
          iwarn = iwarn + 1
      ENDIF
c****
c****    Open the first shot, build the pathname if necessary.
c****
      IF( iformat .EQ. 5 ) THEN
          IF( pathname .EQ. ' ' ) THEN
              PRINT *,' ***  ERROR  ***  IPATH must be given.'
              ierror = ierror + 1
              GOTO 2000
          ENDIF
          cpath = pathname
          CALL bldgname( cpath, ffilen, pathname )
      ENDIF
      IF( stack + ldeolist .EQ. 0 ) THEN
          INQUIRE( FILE=pathname, EXIST=iexist )
          IF( iexist ) THEN
              CALL getfil( 4, iunit, pathname, istat )
              IF( istat .NE. 0 ) THEN
                  PRINT *,' ***  ERROR  ***  Could not open file ',
     &                 pathname
                  ierror = ierror + 1
                  GOTO 2000
              ENDIF
          ELSE
              PRINT *,' ***  ERROR  ***   Can not open file ',
     &                 pathname
                  ierror = ierror + 1
                  GOTO 2000
          ENDIF
      ENDIF

      IF( ierror .NE. 0 ) GOTO 2000
c
c****
c****   Now do some work!
c****   Read a tape record, which must have a multiple of 32 bytes in it,
c****  because it is the "general header".  Extract PACKED BCD variables.
c****
      IF( irun .EQ. 0 ) GOTO 2000
c**** Seisnet has a header before the SEG-D General header and it's in
c****  little endian (pc byte order).
      IF( iformat .EQ. 2 ) THEN
          CALL rddisc( iunit, lbuf, 3, istat )
          IF( icompt .NE. 2 .AND. icompt .NE. 4 ) CALL swap32(lbuf(3),1)
          nbytes = lbuf(3)
          CALL podiscb( iunit, 1, nbytes )
      ENDIF
c**** read the first general header
  300 CALL rddiscb( iunit, ibuf(1), 32, istat )
      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap16( ibuf, 16 )
      indx = 17
      ifile = rshift_sio(ibuf(1),12) * 1000 +
     *        IAND( rshift(ibuf(1),8), i15) * 100 +
     *        IAND( rshift(ibuf(1),4), i15) * 10 +
     *        IAND(ibuf(1),i15)
      ifmt = IAND( rshift(ibuf(2),12), i15) * 1000 +                      ! the iformat code
     *       IAND( rshift(ibuf(2),8), i15) * 100 +
     *       IAND( rshift(ibuf(2),4), i15) * 10 +
     *       IAND(ibuf(2),i15)
      IF( ifmt .EQ. 3336 ) THEN
          IF( iformat .NE. 3 ) THEN
              PRINT *,' ****  WARNING  ****  Is this in HTI iformat?'
              iwarn = iwarn + 1
          ELSE
c****         Skip this one if it's HydroScience's SS iformat
              GOTO 300
          ENDIF
      ENDIF
      IF( ifmt .NE. 8015 .AND. ifmt .NE. 8048 .AND. ifmt .NE. 8058 .AND.
     *    ifmt .NE. 8036 ) THEN
          PRINT *,' ***  ERROR  ***  SIOSEIS does not know about SEG-D',
     *       ' format ',ifmt
          ierror = ierror + 1
      ENDIF
  550 CONTINUE
      iyear = IAND( rshift(ibuf(6),12), i15) * 10 +
     *        IAND( rshift(ibuf(6),8), i15)
      IF( iyear .LT. 80 ) THEN
          iyear = iyear + 2000
      ELSE
          iyear = iyear + 1900
      ENDIF
      nblks = IAND( rshift(ibuf(6),4), i15)
c**** get any general headers in addition to the first 2 which are garunteed
      IF( nblks .GT. 0 ) THEN
          nbytes = nblks * 32
          CALL rddiscb( iunit, ibuf(indx), nbytes, istat )
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     &        CALL swap16( ibuf(indx), nbytes/2 )
          indx = indx + nbytes / 2
      ENDIF
      iday = IAND( ibuf(6),i15) * 100 +
     *       IAND( rshift(ibuf(7),12), i15) * 10 +
     *       IAND( rshift(ibuf(7),8), i15)
      ihour = IAND( rshift(ibuf(7),4), i15) * 10 +
     *        IAND(ibuf(7),i15)
      imin = IAND( rshift(ibuf(8),12), i15) * 10 +
     *       IAND( rshift(ibuf(8), 8), i15)
      isec = IAND( rshift(ibuf(8),4), i15) * 10 +
     *       IAND(ibuf(8), i15)
      manuf = IAND( rshift(ibuf(9),12), i15) * 10 +
     *        IAND( rshift(ibuf(9),8), i15)
      serial = IAND( rshift(ibuf(9),4), i15) * 1000 +
     *         IAND(ibuf(9), i15) * 100 +
     *         IAND( rshift(ibuf(10),12), i15) * 10 +
     *         IAND( rshift(ibuf(10),8), i15 )
      CALL julcal(month, iiday, iyear, iday )        ! convert Julian day to calendar day
      itemp = IAND( rshift(ibuf(12),8), i255 )       ! the sample interval in base 1/16 mils
      si = FLOAT(itemp) / 16. / 1000.                ! the REAL sample interval
      micros = FLOAT(itemp) / 16. * 1000.            ! the sample interval in microseconds
      length = IAND(ibuf(13),i15) * 10 +
     *         IAND( rshift(ibuf(14),12), i15)
      temp = IAND( rshift(ibuf(14),8), i15)
      rlen = ( FLOAT(length) + temp/10. ) * 1.024
      IF( rlen .LT. secs .OR. secs .EQ. 0 ) THEN
          temp = rlen
      ELSE
          temp = secs
      ENDIF
      nsamps = temp / si + 1
      ncsets = IAND(rshift(ibuf(15),12),i15) * 10 +
     *         IAND( rshift(ibuf(15),8), i15)
      IF( ncsets .GT. 0 ) THEN
          nbytes = ncsets * 32
          CALL rddiscb( iunit, ibuf(indx), nbytes, istat )
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     &        CALL swap16( ibuf(indx), nbytes/2 )
          indx = indx + nbytes / 2
      ENDIF
      nskew = IAND( rshift(ibuf(15),4), i15) * 10 +
     *        IAND(ibuf(15),i15)
      IF( nskew .GT. 0 ) THEN
          nbytes = nskew * 32
          CALL rddiscb( iunit, ibuf(indx), nbytes, istat )
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     &        CALL swap16( ibuf(indx), nbytes/2 )
          indx = indx + nbytes / 2
      ENDIF
      nextend = IAND(rshift(ibuf(16),12),i15) * 10 +
     *          IAND( rshift(ibuf(16),8), i15)
      IF( nextend .EQ. 165 ) nextend = lshift(IAND(ibuf(19),i255),8) +
     &             IAND( rshift(ibuf(20),8),i255)
      nexternal = IAND( rshift(ibuf(16),4), i15) * 10 +
     *            IAND(ibuf(16),i15)
      IF( nexternal .EQ. 165 ) nexternal=lshift(IAND(ibuf(20),i255),8) +
     &             IAND( rshift(ibuf(21),8),i255)
      ntotal = 1 + nblks + ncsets + nskew + nextend + nexternal
      IF( IAND(lprint,2) .NE. 0 )
     &    PRINT *,' total general header blocks ',ntotal
c**** Get the record length from GH#2 if it's FFF.
c**** This one is in mils
      IF( length .EQ. 165 ) THEN
          length = ibuf(24) * 255 + IAND(rshift(ibuf(25),8),i255)
          nsamps = length / (si * 1000)
      ENDIF
c****
c****   Get the Extended headers
c****
      IF( nextend .GT. 0 ) THEN
          nbytes = nextend * 32
          CALL rddiscb( iunit, ibuf(indx), nbytes, istat )
          indx = indx + nextend * 16
      ENDIF
c****
c****    Get the External header
c****
      IF( nexternal .GT. 0 ) THEN
          nbytes = nexternal * 32
          CALL rddiscb( iunit, ibuf(indx), nbytes, istat )
          indx = indx + nexternal * 16
      ENDIF
c****
c**** get the number of input traces because things like gather might
c**** need them.  This doesn't quite make sense to me.  I'm off two
c**** scans already, so use a magic number.
c****
      index = (1+nblks) * 16
      idelay = ibuf(index+3) * 2
      delay = FLOAT(idelay)/1000.
      IF( stime .GT. delay ) THEN                                       ! assume that the first trace will not be used
          delay = stime
          nsamps = nsamps - (stime-delay)/si
      ENDIF
      numdat = nsamps
      IF( ncsets .GT. 1 .AND. fcset .EQ. 99 )THEN
          indx = index
          DO i = 1, ncsets
             n = IAND( rshift(ibuf(indx+5),12), i15) * 1000 +
     *           IAND( rshift(ibuf(indx+5),8), i15) * 100 +
     *           IAND( rshift(ibuf(indx+5),4), i15) * 10 +
     *           IAND(ibuf(indx+5),i15)
             indx = indx + 16
         PRINT *,' ***  WARNING *** Channel set ',i,' has ',n,' traces.'
             iwarn = iwarn + 1
          ENDDO
      ENDIF
      IF( fcset .EQ. 0 ) fcset = ncsets
      IF( lcset .EQ. 0 ) lcset = ncsets
      IF( fcset .EQ. 99 ) THEN
          fcset = 1
          lcset = ncsets
      ENDIF
      IF( lcset .GT. ncsets ) THEN
          lcset = ncsets
          PRINT *,' ***  WARNING  ***  lcset being reset to ',ncsets
          iwarn = iwarn + 1
      ENDIF
      IF( fcset .EQ. 99 .AND. lcset .NE. 0 ) THEN
          PRINT *,' ***  WARNING  ***  FCSET should be given.'
          iwarn = iwarn + 1
      ENDIF
      intrcs = 0
      DO i = fcset, lcset
         index = index + (i-1) * 16
         n = IAND( rshift(ibuf(index+5),12), i15) * 1000 +
     *         IAND( rshift(ibuf(index+5),8), i15) * 100 +
     *         IAND( rshift(ibuf(index+5),4), i15) * 10 +
     *         IAND(ibuf(index+5),i15)
         intrcs = intrcs + n
      ENDDO
c****
c****  Always save the extended header in luntr0 as a circular file
c****  If we're to save all trace 0 in a file, open a file
c****
      CALL getfil( 1, luntr0, token, istat )
      IF( iformat .EQ. 6 ) CALL getfil( 1, lunldeo, token, istat )
      IF( tr0 .NE. 0 ) THEN
c****   This is the wierdest parameter.  OUTPUT is the one that
c****   uses it, not segdin.
          WRITE( token, 900 ) iday, ihour, imin
  900     FORMAT('tr0.',I3.3,'-',I2.2,I2.2)
          CALL getfil( 3, luntr0a, token, istat )
      ENDIF
c****
c****  Write the SEGDEX parameters to a disc file and get all the other
c****  parameter list taken care of before building headers
c****
      lbuf(1) = iunit
      lbuf(2) = ffilen
      lbuf(3) = lfilen
      lbuf(4) = filinc
      lbuf(5) = ftr
      lbuf(6) = ltr
      lbuf(7) = trinc
      buf(8) = secs
      lbuf(9) = decimf
      lbuf(10) = lprint
      IF( fday .EQ. 0 .AND. fgmt .NE. 0 ) fday = iday
      lbuf(11) = fday
      IF( lday .EQ. 0 ) lday = fday                                     ! preset lday to fday
      lbuf(12) = lday
      lbuf(13) = fgmt
      IF( lgmt .EQ. 0 ) lgmt = 2400
      lbuf(14) = lgmt
      lbuf(15) = ntrgat
      buf(16) = stime
      lbuf(17) = renum
      lbuf(18) = fcset
      lbuf(19) = lcset
      lbuf(20) = gmtinc
      lbuf(21) = iformat
      lbuf(22) = retrac
      lbuf(23) = stack
      lbuf(24) = list
      lbuf(25) = lunlog
      lbuf(26) = nspfile
      lbuf(27) = lunotape
      lbuf(29) = ldeolist
      lbuf(30) = descale
      nwrds = npars
      CALL wrdisc( junit, buf, nwrds )
      nlists = nlists + 1
      IF( IAND(lprint,1) .NE. 0 ) THEN
          PRINT *,(lbuf(i),i = 1, 7), buf(8)
          PRINT *,(lbuf(i),i = 9, nwrds)
      ENDIF
c****
c****    finish up the parameter reading
c****
 2000 CONTINUE
      CALL getoke( token, nchars )                                      ! get the next token
      CALL upcase( token, nchars )
      ntokes = ntokes + 1
      IF( nchars .LE. 0 ) THEN
          IF( now .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL rdline                                                   ! get a new line of parameters
          ntokes = 0
          GOTO 2000
      ENDIF
      IF( token(1:nchars) .NE. 'END' .OR. nchars .NE. 3 ) THEN
           PRINT *,' ***  ERROR  ***  Only 1 list allowed in SEGDDIN.'
           PRINT *,
     &         ' Use parameter LISTPATH to enter multiple SEGD files.'
           CALL getoke( token, nchars )
           GOTO 2000
       ENDIF
c****
c****  Do some error/warning checks
c****
      IF( ncsets .NE. 1 .AND. ierror .EQ. 0 ) THEN
          IF( fcset + lcset .EQ. 0 ) THEN
              PRINT *,' ***  WARNING  ***  Multiple SEG-D channel sets.'
              PRINT *,'                    Use fcset and lcset.'
              iwarn = iwarn + 1
          ENDIF
c          IF( retrac .EQ. 0 .AND. fcset + lcset .EQ. 0 ) THEN
c              PRINT *,' ***  WARNING  ***  RETRAC should be used with mu
c     &ltiple channel sets.'
c              iwarn = iwarn + 1
c          ENDIF
      ENDIF
c****
c****    Build the SEGY EBCDIC tape header
c****
      DO 2100 i = 1, 40
         WRITE( cheadr(i), 2090 ) i
 2090    FORMAT('c ',I2)
 2100 CONTINUE
      IF( ncruis + nreel .NE. 0 ) THEN
          WRITE( cheadr(2), 2110 ) nreel, ncruis, months(month), iyear
 2110     FORMAT('c  2  Tape ',I3,' Cruise ',I3,2x,A4,1x,I4)
      ENDIF
      CALL ascebc( cheadr, 3200, cheadr )
      CALL wrdisc( iunhdr, ivms, 800 )
c****
c****    Build the SEGY binary header
c****
      DO i = 1, 200
         ibuf(i) = 0
      ENDDO
      ibuf(7) = intrcs
      ibuf(11) = (nsamps + decimf - 1) / decimf
      ibuf(13) = ifmt
      ibuf(15) = 1
      CALL wrdisc( iunhdr, ibuf, 100 )
c****
c****    Build 1 SEGY trace header
c****
      DO i = 1, 60
         lbuf(i) = 0
      ENDDO
      lbuf(3) = ffilen
      lbuf(4) = ftr
      ibuf(15) = 1
      ibuf(55) = idelay
      ibuf(58) = (nsamps + decimf - 1) / decimf
      ibuf(59) = micros * decimf
      ibuf(79) = iyear
      ibuf(80) = iday
      ibuf(81) = ihour
      ibuf(82) = imin
      ibuf(83) = isec
      ibuf(84) = 1
      buf(46) = delay
      buf(49) = si * decimf

      RETURN
      END
