       SUBROUTINE segded( ibuf, buf, lbuf, scr )
c                           PROCESS SEGDIN
c                           _______ ______
c
c  Document date:
c
c       PROCESS SEGDIN reads seismic tape formatted in the SEG-D format
c  or the SEG-D, Revision 1 format (see Geophysics, April 1994). 
c  Three demultiplexed data formats are available: 
c  1)  20 bit binary format.
c  2)  IBM floating point (SEG-D hexidecimal).
c  3)  IEEE floating point (SEG-D, Revision 1.).
c       Reel changes and job termination may be done in the normal tape
c  manner, that is; the new tape unit number should be inserted in a 
c  file named "IN" when the new tape is loaded and ready.  A negative
c  tape unit number indicates that there are no more tapes and the job
c  should be terminated.
c
c  Example:
c  segdin
c      iunit 5 end
c  end
c
c  PARAMETER DICTIONARY
c  --------- ----------
c
c  PARAMETERS FOR READING SHOTS BY FILE NUMBER (SHOT NUMBER)
c-----------------------------------------------------------
c
c  FFILEN - The first file number (shot) to read from tape.  File numbers
c           less than FFILEN will be omitted.  FFILEN 99999 indicates
c           that all shot files should be read, in the order on tape,
c           until LFILEN is reached or end-of-tape is detected.
c           Preset = 99999
c
c  LFILEN - The last file number (shot) to read from tape.  File numbers
c           greater than LFILEN will be omitted.
c           Preset = 999999999
c
c  FILINC - The increment between FFILEN and LFILEN.  FILINC 99999
c           indicates that all shots between FFILEN and LFILEN will
c           be used regardless of order.  Only valid when FFILEN is
c           given.
c           Preset = 99999
c
c
c  PARAMETERS FOR READING SHOTS BY GMT
c-------------------------------------
c
c  FDAY   - The Julian day of the first shot to be read from tape.
c           preset = the day of the first shot on tape    e.g. fday 365
c
c  LDAY   - The Julian day of the last shot to be processed.
c           preset = fday    e.g. lday 366
c
c  FGMT   - The GMT of the first data to extract from the tape.  Shots
c           prior to FGMT will not be processed.
c           preset = the GMT of the first shot on tape.
c
c  LGMT   - The GMT of the last shot to extract from tape.
c           preset = the last shot on the FIRST tape.
c
c  GMTINC - The increment between FFILEN and LFILEN.  FILINC 99999
c           indicates that all shots between FFILEN and LFILEN will
c           be used regardless of order.
c           Preset = 99999
c
c
c  PARAMETERS USED BY BOTH GMT AND FILE NUMBERS
c----------------------------------------------
c
c  IUNIT  - The input tape unit number. On most Unix systems this is the
c           number used in the device name /dev/nrst*.  The SEG-D format
c           requires each shot to be terminated by a single file mark,
c           so various tape utilities (e.g. tutil or mt) may be used.
c           preset = 0    e.g.  iunit 1   means /dev/nrst1
c
c  LOADER - The sleep time, in seconds, to sleep after each tape is 
c           automatically loaded on the IBM 3480 cartridge auto-loader.
c           SEGDIN issues a rewind/offline command when two consecutive
c           file marks are sensed.  It then sleeps while the loader is
c           changing tapes.  A 90 second sleep works on the Fujitsu 
c           M2485.  The normal tape change scheme (via file in) is used
c           after the last tape in the loader, enabling another stack of
c           tapes to be inserted and SEGDIN notified via file in. (-1 in
c           file in terminates the SIOSEIS run).
c           Preset = 0       e.g. loader 50
c
c  FTR    - The first trace within each file to process.  Trace numbers
c           less than FFILEN will be omitted.
c           Preset = 1     e.g.   ftr 97
c
c  LTR    - The last trace within each file to process.  Traces with 
c           numbers larger than LTR will be omitted.
c           Preset = The largest data trace number in the channel scan.
c
c  TRINC  - The increment between traces.  The trace skip increment.
c           preset = 1      e.g.  trinc 2  every other trace is skipped
c
c  SECS   - The maximum number of seconds of data to process.
c           Preset = length in the header.     e.g.   secs 6
c
c  STIME  - The time of the first data to output.  The delay of the 
c           trace after reformatting.  The data times will be from STIME
c           to STIME + SECS.  The first portion of the data trace will
c           be discarded whenever STIME is greater than the delay of the
c           recorded data.
c           Preset - The recorded delay.   e.g. stime 4.0
c
c  DECIMF - Sample rate decimation factor.  
c           Preset = 1,     e.g.   decimf 2   decimates by a factor of 2
c
c  NTRGAT - The number of traces per gather.  SIOSEIS requires RPs to be
c           terminated with a -1 in word 51 of the SEGY trace header.  
c           Because this is unique to SIOSEIS, gathers from other 
c           computers may be converted by setting NTRGAT to the proper
c           value.  SEGDIN will set every NTRGAT trace to be a
c           terminator.  e.g. ftr 91 ltr 96 ntrgat 6 will read only
c           traces 91 - 96 and will set every trace 96 (the sixth trace
c           to be read) to be a gather terminator.
c           Preset = 0,    e.g.  ntrgat 12
c
c  RENUM  - Renumbers the shots (file numbers) so that the first shot
c           read will be RENUM and successive shots will be incremented
c           by 1.
c           Preset = 0,    e.g.   renum 1
c
c  REWIND - Rewinds the input tape PRIOR to reading the first shot file.
c         = 1, (TRUE), rewind.
c         = 0, (FALSE), NO rewind.
c           Preset = 1,      e.g.  rewind 0    # don't rewind
c
c  FCSET  - First channel set.  The SEG-D format allows all traces with
c           similar characteristics to be grouped together.  If traces
c           have different trace length, all traces with the same length
c           are grouped together.  Likewise, auxillary traces belong
c           to a different set from seismic traces.
c           Preset = total number of channel sets.
c
c  LCSET  - Last channel set.
c           Preset = total number of channel sets.
c
c  NFSKIP - The number of files to skip before reading the first file
c           (in SEG-D, each shot is terminated by a filemark).
c           Preset = 0.    e.g.   nfskip 1        skips 1 file
c
c  FORMAT - The type of SEG-D format of the data tape.
c         =UTIG, The UTIG "EOFless" (no file marks) format.
c           Preset SEGD
c
c  OFFLINE - When set non-zero, the tape is set offline during every
c           tape change.  This is quite useful on 3480 drives without
c           a stack loader since it ejects the tape from the drive,
c           signalling that a new tape should be mounted.
c           Preset = 0
c           
c        
c
c
c  Copyright (C) Seismic Reflection Processors, San Diego, Ca.
c  ALL RIGHTS RESERVED.
c
c  Mod 15 May 1994 - Add parameter REWIND.
c                  - Increase maxwrd from 20,000 to 32767
c  Mod 10 June 1994 - Add parameter DEVICE.
c  Mod 5 Oct. 1994 - Add parameter LOADER.
c  Mod 21 Dec 94 - Change lday preset to fday rather than first on tape.
c  Mod 8 June 1995 - Add FCSET and LCSET.
c                  - Set fday if fgmt is given and fday is not.
c  Mod 1 Nov 95 - Add gmtinc, nfskip, format
c  Mod 5 Jan 96 - Make gmtinc, nfskip, format work! (filinc was there twice)
c  Mod 8 Apr 96 - Allow iunit up to 100 (from 8).
c  Mod 29 Aug 96 - Add parameter offline.
c  Mod 4 Sep 96 - Add parameter tr0 (see segdex for description)
c               - add newfile to communicate with process output when
c                 an EOT was found so it can also write a new file.
c  Mod 5 Dec 96 - nfskip didn't work if iunit was not 0 (call to magtap
c                 used ilun rather than iunit)
c  Mod 26 Jun 97 - Make newfile a parameter and make preset = 0!
c                - Make REWIND a YES/NO (in addition to old 1/0) switch
c                - Make TR0 a YES/NO (in addition to old 1/0) switch
c  Mod 9 Sep 97  - Make OFFLINE a YES/NO (in addition to old 1/0) switch
c  Mod 9 Dec 97 - Put nsamps into SEG-Y binary header for other packages!
c               - Make nsamps correct when decimating.
c  mod 20 Nov 98 - Change ftr preset to 99999
c  mod 23 Feb 99 - Change DEVICE to be passed to astape and remove all
c                  reference to the F77 tape interface.
c  mod 4 Sep 99 - Call astape if DEVICE is given (magtap doesn't do it!)
c  mod 28 Mar 01 - Add RETRAC to renumber the traces within each shot;
c                  especially usefull when multiple channel sets,
c                  because each channel set starts with trace 1
c                  and SEGY doesn't like duplicate trace numbers.
c  mod 29 Apr 01 - Y2K bug going from 2 digit SEGD to 4 digit SEGY
c  mod 3 Jul 01 - Preset retrac to 1 when more than 1 channel set
c  mod 20 Jul 02 - Add a warning when multiple channel sets.
c  mod 31 Jul 02 - Add warning if DEVICE is not 'bn'
c  mod 19 Mar 03 - Make fcset 99 mean all channel sets.
c                - Change fcset preset to 99
c  mod 22 Apr 03 - Print the number of channels in each channel set when fcset=99
c  mod 8 May 03 - Sgi doesn't use the bn convention, that's Sun's.
c  mod 27 June 04 - Add parameter DESCALE.
c  mod 14 Aug 07 - g95 IAND requires arguments to be same type and kind.
c  mod 19 Feb 09 - Allow format 8036 (24 bit integer)
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
c  e.g. The "UTIG" data has 16 auxillary channels, each being a separate
c           channel set, and 96 seismic traces in the 17th channel set.
c
      PARAMETER ( npars = 30 )                                          ! the number of user parameters
      PARAMETER ( maxwrd = 32767 )                                      ! the maximum number of 16 bit words that can be read
      DIMENSION buf(1000)
      INTEGER*2 ibuf(1000), rshift, i15, i255
      DATA i15/15/, i255/255/
      INTEGER lbuf(1000), scr(1000)
      DIMENSION k(12)
      CHARACTER*80 cheadr(40)                                           ! the SEGY tape header
      EQUIVALENCE ( cheadr(1), ivms )
      CHARACTER*80 token
      CHARACTER*7 names(npars)
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
      COMMON /SEGY/ header(60)
      COMMON /binhdr/ ibinhdr(200)
      INTEGER*2 ibinhdr
      COMMON /segdin/ junit, nlists, nwrds, luntr0, luntr0a, lunldeo
c
      INTEGER ffilen, ftr, decimf, filinc, trinc, fday, fgmt, renum,
     &        rewind, loader, fcset, gmtinc, nfskip, format, offline,
     &        tr0, retrac, descale
      EQUIVALENCE ( iunit, lvals(1) ),
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
     8            ( rewind, lvals(18) ),
     9            ( device,lvals(19) ),
     *            ( loader,lvals(20) )
      EQUIVALENCE ( fcset, lvals(21) ),
     2            ( lcset, lvals(22) ),
     3            ( gmtinc, lvals(23) ),
     4            ( nfskip, lvals(24) ),
     5            ( format, lvals(25) ),
     6            ( offline, lvals(26) ),
     7            ( tr0, lvals(27) ),
     8            ( newfile, lvals(28) ),
     9            ( retrac, lvals(29) ),
     *            ( descale, lvals(30) )
      DATA names /'IUNIT ', 'LPRINT', 'FFILEN', 'LFILEN', 'FTR   ',
     *            'LTR   ', 'SECS  ', 'DECIMF', 'FILINC', 'TRINC ',
     *            'FDAY  ', 'LDAY  ', 'FGMT  ', 'LGMT  ', 'NTRGAT',
     *            'STIME ', 'RENUM' , 'REWIND', 'DEVICE', 'LOADER',
     *            'FCSET ', 'LCSET ', 'GMTINC', 'NFSKIP', 'FORMAT',
     *            'OFFLINE', 'TR0  ', 'NEWFILE','RETRAC', 'DESCALE' /
      DATA types /6*'L','F',8*'L','F','L',2*'A',5*'L',4*'A','L','A' /
      DATA months/'Jan.','Feb.','Mar.','Apr.','May ','June',
     *            'July','Aug.','Sep.','Oct.','Nov.','Dec.'/
c**** 
c****    Set the parameter presets and various variable presets
c****
      nlists = 0
      ifile = 0                                                         ! file or shot number
      iunit = 1
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
      rewind = 1
      inputdev = ' '
      loader = 0
      fcset = 99
      lcset = 0
      gmtinc = 99999
      nfskip = 0
      format = 0
      offline = 0
      luntr0 = 0
      luntr0a = 0
      lunldeo = 0
      tr0 = 0
      newfile = 0
      retrac = 0
      iread = 0
      descale = 0
      DO 10 i = 1, num
         IF( iorder(i) .EQ. 41 ) iread = 1                              ! see if segdin is in procs list
   10 CONTINUE
      CALL getfil( 1, junit, token, istat )                             ! get a file for the SEGDIN parameters
      IF( iread .EQ. 1 ) CALL getfil( 1, iunhdr, token, istat )         ! get an SEGY header file
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
  120        CALL getoke( token, nchars )                               ! get the value
             ntokes = ntokes + 1
             IF( nchars .EQ. 0 ) THEN
                 CALL rdline
                 ntokes = 0
                 GOTO 120
             ENDIF
             IF( types(nparam) .EQ. 'A' ) THEN
                 IF( names(nparam) .EQ. 'DEVICE') THEN
                     inputdev = token(1:nchars)
c****   sgi needs    nrns  (no-rewind, no-swap)
              IF( token(nchars-3:nchars) .EQ. 'nrns' ) GOTO 100
c****  sun needs   bn
                     DO i = 1, nchars-1
                        IF( token(i:i+1) .EQ. 'bn' ) GOTO 100
                     ENDDO
                     PRINT *,
     &    ' ***  WARNING  ***  DEVICE should be BSD and No Rewind (bn).'
                     iwarn = iwarn + 1
                     GOTO 100
                 ENDIF
                 CALL upcase( token, nchars )
                 IF( names(nparam) .EQ. 'FORMAT' ) THEN
                     IF( token(1:nchars) .EQ. 'UTIG' ) format = 1
                 ENDIF
                 IF( names(nparam) .EQ. 'REWIND' ) THEN
                     IF( token(1:nchars) .EQ. 'YES' ) rewind = 1
                     IF( token(1:nchars) .EQ. 'NO' ) rewind = 0
                     IF( token(1:nchars) .EQ. 'ON' ) rewind = 1
                     IF( token(1:nchars) .EQ. 'OFF' ) rewind = 0
                     IF( token(1:nchars) .EQ. '1' ) rewind = 1
                     IF( token(1:nchars) .EQ. '0' ) rewind = 0
                 ENDIF
                 IF( names(nparam) .EQ. 'TR0' ) THEN
                     IF( token(1:nchars) .EQ. 'YES' ) tr0 = 1
                     IF( token(1:nchars) .EQ. 'NO' ) tr0 = 0
                     IF( token(1:nchars) .EQ. 'ON' ) tr0 = 1
                     IF( token(1:nchars) .EQ. 'OFF' ) tr0 = 0
                     IF( token(1:nchars) .EQ. '1' ) tr0 = 1
                     IF( token(1:nchars) .EQ. '0' ) tr0 = 0
                 ENDIF
                 IF( names(nparam) .EQ. 'NEWFILE' ) THEN
                     IF( token(1:nchars) .EQ. 'YES' ) newfile = 1
                     IF( token(1:nchars) .EQ. 'NO' ) newfile = 0
                     IF( token(1:nchars) .EQ. 'ON' ) newfile = 1
                     IF( token(1:nchars) .EQ. 'OFF' ) newfile = 0
                     IF( token(1:nchars) .EQ. '1' ) newfile = 1
                     IF( token(1:nchars) .EQ. '0' ) newfile = 0
                 ENDIF
                 IF( names(nparam) .EQ. 'OFFLINE' ) THEN
                     IF( token(1:nchars) .EQ. 'YES' ) offline = 1
                     IF( token(1:nchars) .EQ. 'NO' ) offline = 0
                     IF( token(1:nchars) .EQ. 'ON' ) offline = 1
                     IF( token(1:nchars) .EQ. 'OFF' ) offline = 0
                     IF( token(1:nchars) .EQ. '1' ) offline = 1
                     IF( token(1:nchars) .EQ. '0' ) offline = 0
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
          PRINT *,' ***  ERROR  ***  No such parameter as ',
     *      token(1:nchars)
          ierror = ierror + 1
      ENDIF
c****
c****    Do the parameter validity checks
c****
      IF( iunit .LT. 0 .OR. iunit .GT. 99 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal IUNIT value of ',iunit
          ierror = ierror + 1
      ENDIF
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
          PRINT *,' ***  ERROR  ***  Illegal FTR value of ',ftr
          ierror = ierror + 1
      ENDIF
      IF( ltr .LT. 0 .OR. ltr .GT. 10000 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal LTR value of ',ltr
          ierror = ierror + 1
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

      IF( ierror .NE. 0 ) RETURN
c
c****
c****   Now do some work!
c****   Read a tape record, which must have a multiple of 32 bytes in it,
c****  because it is the "general header".  Extract PACKED BCD variables.
c****
      IF( iread .EQ. 0 .OR. irun .EQ. 0 ) GOTO 2000                     ! segdin in procs and an exexute job
      IF( inputdev .NE. ' ' ) CALL astape( iunit, inputdev, 0 )
      IF( rewind .EQ. 1 ) CALL magtap(iunit,ibuf,maxwrd, 30, istat )
      IF( nfskip .NE. 0 ) THEN
          DO 400 i = 1, nfskip
             CALL magtap( iunit, idum, idum, 22, istat )                ! skip file
  400    CONTINUE
      ENDIF
c**** read the general header, a multiple of 32 bytes
      CALL magtap( iunit, ibuf, maxwrd, 21, istat )                     ! read
      IF( istat .EQ. -1 ) THEN
          IF( ifile .EQ. 0 ) THEN                                       ! is this the first file?
              PRINT *,' ***  ERROR  ***  The tape is not SEG-D, it ',
     *        ' starts with a file mark!'
              STOP
          ENDIF
      ENDIF
      IF( istat/16*16 .NE. istat ) THEN                                 ! are there a multiple of 32 bytes?
          PRINT *,' ***  ERROR  ***  The SEG-D "general header" has ',
     *      istat*2,' bytes in it.'
          ierror = ierror + 1
      ENDIF
      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap16( ibuf, 1 )     ! swap byte on the file number
      ifile = rshift(ibuf(1),12) * 1000 +
     *        IAND( rshift(ibuf(1),8), i15) * 100 +
     *        IAND( rshift(ibuf(1),4), i15) * 10 +
     *        IAND(ibuf(1),i15)
      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     *        CALL swap16( ibuf(2), istat-1 )                           ! swap the rest if we want this file!
      ifmt = IAND( rshift(ibuf(2),12), i15) * 1000 +                      ! the format code
     *       IAND( rshift(ibuf(2),8), i15) * 100 +
     *       IAND( rshift(ibuf(2),4), i15) * 10 +
     *       IAND(ibuf(2),i15)
      IF( ifmt .NE. 8015 .AND. ifmt .NE. 8048 .AND. ifmt .NE. 8058 .AND.
     &    ifmt .NE. 8036 ) THEN
          PRINT *,' ***  ERROR  ***  SIOSEIS does not know about SEG-D',
     *       ' format ',ifmt
          ierror = ierror + 1
      ENDIF
      j = 1
      DO 550 i = 3, 5                        ! get 12 Konstants
         k(j) = IAND( rshift(ibuf(i),12), i15)
         j = j + 1
         k(j) = IAND( rshift(ibuf(i),8), i15)
         j = j + 1
         k(j) = IAND( rshift(ibuf(i),4), i15)
         j = j + 1
         k(j) = IAND( ibuf(i), i15)
         j = j + 1
  550 CONTINUE
      ncruis = k(1)*100 + k(2)*10 + k(3)       ! this is unique to GULF
      nreel = k(4)*100 + k(5)*10 + k(6)
      iyear = IAND( rshift(ibuf(6),12), i15) * 10 +
     *        IAND( rshift(ibuf(6),8), i15)
      IF( iyear .LT. 80 ) THEN
          iyear = iyear + 2000
      ELSE
          iyear = iyear + 1900
      ENDIF
      naddblks = IAND( rshift(ibuf(6),4), i15)
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
      CALL julcal(month, iiday, iyear, iday )    ! convert Julian day to calendar day
      itemp = IAND( rshift(ibuf(12),8), i255 )    ! the sample interval in base 1/16 mils
      si = FLOAT(itemp) / 16. / 1000.            ! the REAL sample interval
      micros = FLOAT(itemp) / 16. * 1000.        ! the sample interval in microseconds
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
      nskew = IAND( rshift(ibuf(15),4), i15) * 10 +
     *        IAND(ibuf(15),i15)
      nextend = IAND(rshift(ibuf(16),12),i15) * 10 +
     *          IAND( rshift(ibuf(16),8), i15)
      nexternal = IAND( rshift(ibuf(16),4), i15) * 10 +
     *            IAND(ibuf(16),i15)
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' ncsets=',ncsets,' nskew=',nskew,' nextend=',nextend,
     &      ' nexternal=',nexternal,' naddblks=',naddblks
      ENDIF
c**** point to the first channel set descriptor
      index = (1+naddblks)*16
      idelay = ibuf(index+2) * 2
      delay = FLOAT(idelay)/1000. 
      IF( stime .GT. delay ) THEN                                       ! assume that the first trace will not be used
          delay = stime
          nsamps = nsamps - (stime-delay)/si
      ENDIF
      numdat = nsamps
      intrcs = IAND( rshift(ibuf(index+5),12), i15) * 1000 +
     *         IAND( rshift(ibuf(index+5),8), i15) * 100 +
     *         IAND( rshift(ibuf(index+5),4), i15) * 10 +
     *         IAND(ibuf(index+5),i15)
      igain = IAND(ibuf(index+6),i15)     ! type of gain, 3=fixed, 8=binary AGR
      iafilt = IAND( rshift(ibuf(index+7),12), i15) * 1000 +             ! alias filter frequency
     *         IAND( rshift(ibuf(index+7),8), i15) * 100 +
     *         IAND( rshift(ibuf(index+7),4), i15) * 10 +
     *         IAND(ibuf(index+7),i15)  
      islope = IAND( rshift(ibuf(index+8),12), i15) * 1000 +             ! alias filter slope
     *         IAND( rshift(ibuf(index+8),8), i15) * 100 +
     *         IAND( rshift(ibuf(index+8),4), i15) * 10 +
     *         IAND(ibuf(index+8),i15)  
      lc = IAND( rshift(ibuf(index+9),12), i15) * 1000 +                 ! low cut filter frequency
     *     IAND( rshift(ibuf(index+9),8), i15) * 100 +
     *     IAND( rshift(ibuf(index+9),4), i15) * 10 +
     *     IAND(ibuf(index+9),i15)  
      ls = IAND( rshift(ibuf(index+10),12), i15) * 1000 +                ! low cut filter slope
     *     IAND( rshift(ibuf(index+10),8), i15) * 100 +
     *     IAND( rshift(ibuf(index+10),4), i15) * 10 +
     *     IAND(ibuf(index+10),i15)  
      notch = IAND( rshift(ibuf(index+11),12), i15) * 100 +              ! notch filter ferquency
     *        IAND( rshift(ibuf(index+11),8), i15) * 10 +
     *        IAND( rshift(ibuf(index+11),4), i15)
      rnotch = FLOAT(notch) + temp/10.
c****
c****  Always save the extended header in luntr0 as a circular file
c****  If we're to save all trace 0 in a file, open a file
c****  If it's a Syntron, then it the Ewing, so open lunldeo
c****
      CALL getfil( 1, luntr0, token, istat )
      IF( manuf .EQ. 34 ) CALL getfil( 1, lunldeo, token, istat )
      IF( tr0 .NE. 0 ) THEN
c****   This is the weirdest parameter.  OUTPUT is the one that
c****   uses it, not segdin.
          WRITE( token, 900 ) iday, ihour, imin
  900     FORMAT('tr0.',I3.3,'-',I2.2,I2.2)
          CALL getfil( 3, luntr0a, token, istat )
      ENDIF
c****
c****  Do some error/warning checks
c****
      IF( ncsets .GT. 1 .AND. fcset .EQ. 99 ) THEN
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
      IF( fcset .LT. 1 ) THEN
          PRINT *,' ***  ERROR  ***  Bad FCSET of ',fcset
          ierror = ierror + 1
      ENDIF
      IF( lcset .LT. fcset .OR. lcset .GT. ncsets ) THEN
          PRINT *,' ***  ERROR  ***  Bad LCSET of ',lcset
          ierror = ierror + 1
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
      lbuf(20) = loader
      lbuf(21) = fcset
      lbuf(22) = lcset
      lbuf(23) = gmtinc
      lbuf(24) = nfskip
      lbuf(25) = format
      lbuf(26) = offline
      lbuf(28) = newfile
c**** If multiple channel sets, make sure retrac is used
      IF( fcset .NE. lcset .AND. retrac .EQ. 0 ) retrac = 1
      lbuf(29) = retrac
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
      IF( token(1:nchars) .NE. 'END' .OR. nchars .NE. 3 ) GOTO 100
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
         ibinhdr(i) = 0
      ENDDO
      ibinhdr(7) = intrcs
      ibinhdr(11) = (nsamps + decimf - 1) / decimf
      ibinhdr(13) = ifmt
      ibinhdr(15) = 1
      CALL wrdisc( iunhdr, ibinhdr, 100 )
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
