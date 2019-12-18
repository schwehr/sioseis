      SUBROUTINE segdex( buf, lbuf, ibuf, scr, lscr, iscr, istop )
c**** SEGDEX is the execution phase of the SIOSEIS SEGDIN process.
c  modified June 1988 by pch to make ffilen and lfilen work
c  modified June 1989 by pch because nsamps was 0 when stime > delay
c  modified June 1989 by pch to use the delay and number of samples from
c         the shot header and secs was given!
c  mod May 1994 by pch for the expanaded SEGD according to Apr. 1994
c         and for the 8048 format (IBM floating point) used by Alliant
c     GEOPHYSICS, page 668
c  mod May 94 - Increase max record length to 64KB
c  mod 10 June 94 - add DEVICE (f77 tape I/O)
c  mod 16 June 94 - Change logic on fgmt/lgmt because of bad gmt on tape
c  mod 29 Sept 94 - ffilen/lfilen 0 didn't work.
c  mod 4 Oct. 94 - for LDGO/Digicon.  channel set 1 is aux, 2 is seismic
c  mod 5 Oct. 94 - Add the auto-loader feature.
c  mod 6 Oct. 94 - Add the LDEO nav.
c  mod 21 Dec. 94 by gmk - changed exit handling for lday problem
c                        - changed setting of delay
c  mod 8 June95 - Allow multiple channel sets
c               - Add FCSET and LCSET.
c  mod 23 Oct. 95 - Change LDEO termination logic to not pass trace 0 on
c  mod 27 Oct 95 - Fujitsu and Exabyte stack loaders return incorrect status for EOT
c                - get around UTIG/Ewing stacker problem loading new tapes
c                - comment out the above.
c  mod 1 Nov. 95 - Add EOF less format for inhouse UTIG modified SEG-D
c                - Add filinc.
c  mod 6 Jan. 96 - eofless didn't work.
c                - fgmt needed to be reset to 0 after first trace found,
c                  so next day (0000z) works.
c                - filinc caused ffilen to be wrong.
c  mod 18 Jan 96 - remove dec 94 delay kludge because channel set logic works now.
c  mod 24 Aug 96 - make lfilen work if ltr is not given.
c  mod 29 Aug 96 - Add parameter offline to set EVERY tape offline when
c                  done - when offline is set!
c  mod 4 Sept 96 - Save the Digicon/LDGO trace 0 on disk in luntr0.
c  mod 7 Sept 96 - Skip files with bad LDGO shot numbers
c  mod 13 Sept 96 - Put Digicon trace0 section 13 (depths) in common
c  mod 15 Sept 96 - Allow multiple parameter lists!
c  mod 18 Sept 96 - Don't need trace 0 disk file anymore.  Also put the
c                  entire trace 0 in circular disk file luntr0 so that
c                  others (e.g. geomex) can see it.
c  mod 25 Sep 96 - Check each trace to see if it's an unexpected SEG-D
c                  general header (96 bytes).  (DSS says "SQRT error 84"
c                  where SQRT means square tape (3480 is square))
c  mod 26 Sep 96 - Get rid of the bad traces dues to the LDGO cable
c                  "rebuild" 
c                - Detect the LDGO error which causes the LDGO header
c                  to be repeated (not updated) for numerous shots.  The
c                  file number increments, but the shot number and time
c                  do not.
c  mod 29 Sep 96 - put Hydrosweep depth in word 16
c                - convert water depth to time and put in word 50
c  mod 25 Feb 97 - Changed args in ldgo_tr0 to include mils.
c  mod 9 Mar 97  - Use "pure" SEG-Y for lat/long.
c  mod 25 Jun 97 - Change common/writet/ nsamps name to nsampo so we
c                  don't clobber it!
c  mod 16 Jun 97 - Add parameter NEWFIL
c  mod 10 Sep 97 - OPEN needs a valid unit number when "status=unknown"
c  mod 14 Oct 97 - Don't skip shot with bad LDGO info, just reset the
c                  shot number to be 1 bigger than the last.
c  mod 3 Dec 97 - Change MAX0 to AMAX1 when args are real - ALPHA is pickie
c  Mod 9 Dec 97 - Make nsamps correct when decimating.
c  mods 20 Nov. 98 for Ewing Syntron system.  The record length in the
c     general header is wrong, as is the start/end time in the channel
c     set header.
c    a) The additional general header block count is the number of 32
c       byte blocks in addition to general header #1.  I guess Rev. 0
c       had 1 general header block and Rev. 1 has 2, because the doc
c       for Rev 1 says the additional blocks counter must be > 0. 
c       e.g. when the additional block count is 2, there are 3 blocks!
c    b) Syntron is using the extended trace header.
c  Mod 18 Feb. 99 - The LDEO "trace 0" starts in byte 1883 after
c    the general header and the additional general headers and the
c    extended header and the external header.  It should have been in
c    the external header.  It looks like they've created their own
c    header.  Syntron is manufacturer 34.  At this stage the LDEO
c    general header block is 9696 bytes long.
c  mod 23 Feb 99 - Change DEVICE to be passed to astape and remove all
c                  reference to the F77 tape interface.
c  mod Mar 99 - Bugger.  General header 1 has the extended header block
c               in BCD and general header 2 has it in binary. F!@#$%^&
c  mod March 1999 - Muck about to get the Digicourse stuff from the new
c                   LDEO Syntron system.  geomex needs it.
c  mod Sept 2000 - The LDEO Hydrosweep water depth is bad (0?) occassionally
c                  so save it and use the last one when it's zero.
c mod Sept 2000  - ffilen/lfilen without ftr/ltr didn't work.
c                - reverse lat/long in SEGY header.  Should be long/lat
c                - make lat/long REAL*8
c                - Syntron uses the extended header file number when
c                  the file number exceeds 16665
c mod 29 Apr 01 - Y2K bug!!!!!   SEGD is 2 digits and SEGY is 4
c mod 2 Jul 01 - Add LDEO/Spectra/Syntrak navigation block.
c              - lfilen didn't work when multiple channel sets.
c mod 3 Jul 02 - ntrgat needs to set the rp number and rp trace number
c mod 9 Jul 09 - COMMON WRITET had changed!!!!!
c mod 15 Jul 02 - Add ntotal to see how many 32 byte blocks should be
c                 in the general header.
c mod 20 Jul 02 - Add an FYI message for multiple channel sets.
c mod 29 Sep 02 - Don't save/use bad water depths.
c mod 4 Oct 02 - Get the line name from the LDEO header (don't know
c                what to do with it though!)
c mod 19 Mar 03 - Separate SEG-D general header from LDEO header.
c mod 6 May 04 - Detect the expanded file number correctly.
c              - Use the nav ($GPGGA) from the external header if it's there.
c mod 27 June 04 - Add parameter DESCALE and honor the MP factor
c mod 14 Sep 04 - Extended file number was wrong.
c mod 19 Sep 04 - The MP factor is sign AND magnitude!
c mod 13 Jun 05 - Set newfile = 0 on every call.
c mod 14 Aug 07 - g95 IAND requires arguments to be same type and kind.
c               - g95 can't declare type and set value on same statement
c mod 3 June 08 - LDEO lat/long is off by a factor of 6.
c mod 5 Feb 09 - Allow format 8036 - 24 bit integer
c mod 6 Feb 09 - Increase maxwrd from 32KW to 65KW (128KB is max segd allows)
c
c****
c****             TRACE 0
c****     The Digicon DMS2000 system continued their earlier trace 0
c****  philosophy for passing non seismic information.  e.g.  The
c****  streamer depth and compass and tail buoy information is contained
c****  in trace 0.  In SEG-D, "trace 0" is really trace 1 of channel set
c****  1 (the seismic data are in channel set 2).
c****     SEGDIN tries to preserve this convention.  Trace 0 is stored
c****  in a "circular file" (luntr0), thus segdex rewinds it before
c****  writing to it.  luntr0a is a file with ALL the trace 0s.
c****     GEOM reads trace 0 to get depth and compasses.
c****     OUTPUT writes it with trace id 28
c****
      PARAMETER ( maxwrd = 65535 )    ! the maximum number of 16 bit words that can be read
c     remember that scr, iscr, lscr are equivalenced
      DIMENSION buf(1000), scr(1000)
      DIMENSION delay(99), idelay(99)
      INTEGER*2 ibuf(1000), iscr(1000), rshift
      INTEGER lbuf(1000), lscr(1000)
      COMMON /apmem/iap(32766)
      INTEGER*2 iap, i15, i255, i127, i128
      DATA i15/15/, i255/255/, i127/127/, i128/128/
      REAL*8 ship_lat, ship_long, tail_lat, tail_long
      CHARACTER*3 dgps_id
      CHARACTER*200 cbufin
      COMMON /sioln1/cbufin
      COMMON /sioln2/ jchar, ncbuf
      COMMON /sioap/ iasgnd, irelse, in, iout,nextad,lapsiz,ifree,iuseap
      COMMON /segdin/ junit, nlists, nwrds, luntr0, luntr0a, lunldeo
      COMMON /readt/ iunit, numhdr, numdat, ihunit, ireeln, intrcs
      COMMON /WRITET/ounit,NSAMPS,OREEL,POSAFT,OFMT,NTRCS,LFOR,ONUMTR,
     &       nfskipo, rewindo, newfile, itrace0, ounit2
      INTEGER*4 OFMT,OUNIT,POSAFT,ONUMTR,OREEL, rewindo, ounit2
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /inputdev/inputdev
      CHARACTER*80 inputdev
      CHARACTER*800 ldeo_ascii
      INTEGER*2 ldeo_dumb(400)
      EQUIVALENCE (ldeo_ascii,ldeo_dumb(1))
      CHARACTER*10 line_name
      INTEGER ffilen, filinc, ftr, trinc, decimf, fgmt, fday, renum
      INTEGER format, naddblocks, manuf, serial, fftr, fcset, blocked,
     &        gmtinc, nfskip, ifmt, offline, badtrace, retrac, descale
      REAL descalar(10)
      LOGICAL first, getlist
      SAVE
      DATA first/.TRUE./, mtrgat/0/, ldgo_shotno/0/, nchanges/0/
      DATA wdepth /0./, owdepth/-1./, lastshotno/0/, blocked/0/
      DATA ship_lat/0./, ship_long/0./, rev/0./
      DATA badtrace/0/, lastshot/0/, lastfile/0/ neofs/0/
c****
c****  get the parameters from disc on the first entry
c****
c****
      newfile = 0
      IF( first ) THEN
          first = .FALSE.
          CALL podisc( junit, 1, 0 )                                    ! rewind the parameter file
          mlists = 0
          getlist = .TRUE.
c         get rid of file in and IN
          itemp = 99
          OPEN(UNIT=ITEMP,FILE='IN',STATUS='UNKNOWN')
          CLOSE(UNIT=ITEMP,STATUS='DELETE')
          OPEN(UNIT=ITEMP,FILE='in',STATUS='UNKNOWN')
          CLOSE(UNIT=ITEMP,STATUS='DELETE')
      ENDIF
      IF( getlist ) THEN
          mlists = mlists + 1
          getlist = .FALSE.
          CALL rddisc( junit, lscr, nwrds, istat )
          iunit = lscr(1)
          ffilen = lscr(2)
          lfilen = lscr(3)
          filinc = lscr(4)
          ftr = lscr(5)
          ltr = lscr(6)
          trinc = lscr(7)
          secs = scr(8)
          decimf = lscr(9)
          lprint = lscr(10)
          fday = lscr(11)
          lday = lscr(12)
          fgmt = lscr(13)
          lgmt = lscr(14)
          ntrgat = lscr(15)
          stime = scr(16)
          renum = lscr(17)
          loader = lscr(20)
          fcset = lscr(21)
          lcset = lscr(22)
          gmtinc = lscr(23)
          nfskip = lscr(24)
          ifmt = lscr(25)
	     offline = lscr(26)
          newfil = lscr(28)
          newfile = 0
          retrac = lscr(29)
          descale = lscr(30)
c****
c****  ibuf, the SEGY header, was created by the edit (segded.f).
          igmt = ibuf(81)*100 + ibuf(82)                                ! the GMT of the shot from the general header
          fftr = ftr                                                    ! the next trace wanted
          idelay(1) = ibuf(55)
          micros = ibuf(59) / decimf
          iyear = ibuf(79)
          iday = ibuf(80)
          ihour = ibuf(81)
          imin = ibuf(82)
          isec = ibuf(83)
          si = buf(49) / decimf
          IF( IAND(lprint,2) .NE. 0 ) THEN
              PRINT *,iunit,ffilen,lfilen,filinc,ftr,ltr,trinc,secs,
     *                decimf
              PRINT *,fday,lday,fgmt,lgmt,ntrgat,stime,renum,igmt,
     *             idelay(1),nsamps,micros,iyear,iday,ihour,imin,isec,si
              PRINT *, fcset, lcset, gmtinc, nfskip, ifmt, offline,
     &             newfil
          ENDIF
          IF( mlists .EQ. 1 ) THEN
              CALL magtap( iunit, scr, 0, 25, istat )                   ! skip back a record
              GOTO 120
          ENDIF
      ENDIF
c****
c****   Rather than take file marks and general headers as they come,
c****  skip to them after the last trace (intrcs is the number of
c****  traces per shot).
c**** When fftr = ftr, then look for the general header
      IF( fftr .NE. ftr .OR. ftr .EQ. 99999 ) GOTO 200
c**** 
c****    get a shot/general header ( right after a file mark )
c****
  100 CONTINUE
      IF( ifmt .EQ. 0 ) THEN                                            ! standard SEG-D is ifmt 0
          CALL magtap( iunit, scr, 0, 22, istat )                       ! skip a file
      ELSE
c****     In the EOFless format; 2 EOFs = EOT, 1 EOF = ignore it
  115     CONTINUE
          CALL magtap( iunit, scr, maxwrd, 21, istat )
          IF( istat .EQ. -6 ) THEN
              PRINT *,' ***  WARNING  ***   Tape read PARITY.'
              GOTO 115
          ENDIF
          IF( istat .LE. 0 ) THEN
              neofs = neofs + 1
              IF( neofs .EQ. 1 ) GOTO 115
              GOTO 125
          ENDIF
          neofs = 0
          format = IAND( rshift(iscr(2),12), i15) * 1000 +
     *         IAND( rshift(iscr(2),8), i15) * 100 +
     *         IAND( rshift(iscr(2),4), i15) * 10 +
     *         IAND(iscr(2),i15)
          IF( format .LT. 8015 .OR. format .GT. 8058 ) GOTO 115
          GOTO 125
      ENDIF
  120 CONTINUE                                                          ! read the general header
      CALL magtap( iunit, scr, maxwrd, 21, istat )
      nbytes = istat * 2
c     Some devices seem to return parity (-6) on the second file mark!
  125 IF( istat .LE. 0 ) THEN                                           ! EOT?
          IF( newfil .NE. 0 ) newfile = 1
          CALL magtap( iunit, scr, 0, 30, istat )
          IF( loader .EQ. 0 ) THEN                                      ! not using stacker, so change tape
              IF( offline .NE. 0 ) THEN
                  CALL offlmt( iunit )  
c                 offlmt put the drive offline, subsequent usage will
c                 bomb out until a new tape is loaded, so signal tpchng
c                 to NOT check status or do anything to the drive.
c     The Ewing 3480 needs to clear it's status, which untape does!
                  CALL untape( iunit )
c                  CALL astape( iunit, scr, 0 )
                  iunit = -99
              ENDIF
              CALL tpchng( iunit, iunit, 0 )                            ! get a new tape unit number
              IF( iunit .LT. 0 ) THEN                                   ! did the operator stop the job?
                  istop = -1
                  RETURN
              ENDIF
              GOTO 120
          ELSE                                                          ! yes, we are using stacker
              IF( nchanges .NE. 0 ) THEN                                ! if haven't read a record since last eot
	             CALL tpchng( iunit, iunit, 0 )                     ! change the stacker
	             IF( iunit .LT. 0 ) THEN                            ! did operator stop the job?
                      istop = -1
                      RETURN
                  ENDIF
                  CALL magtap( iunit, scr, 0, 30, istat )
                  GOTO 120
              ELSE                                                      ! this is first eot since a good record
                  nchanges = nchanges + 1                               ! set eot flag 
                  CALL offlmt( iunit )                                  ! eject tape
                  CALL sleep( loader )                                  ! sleep while loader loads next tape
c     move the tape and then rewind because of load problems at UTIG/Ewing.
                  CALL magtap( iunit, scr, 0, 23, istat )
                  CALL magtap( iunit, scr, 0, 30, istat )
                  GOTO 120
              ENDIF
          ENDIF
      ENDIF
      neofs = 0
      nchanges = 0                                                      ! reset eot flag
      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     *        CALL swap16( iscr(2), istat )
      ifilen = IAND( rshift(iscr(1),12), i15) * 1000 +                    ! file number
     *         IAND( rshift(iscr(1),8), i15) * 100 +
     *         IAND( rshift(iscr(1),4), i15) * 10 +
     *         IAND(iscr(1),i15)  
      format = IAND( rshift(iscr(2),12), i15) * 1000 +
     *         IAND( rshift(iscr(2),8), i15) * 100 +
     *         IAND( rshift(iscr(2),4), i15) * 10 +
     *         IAND(iscr(2),i15)
      iyear = IAND( rshift(iscr(6),12), i15) * 10 +
     *        IAND( rshift(iscr(6),8), i15)
      IF( iyear .LT. 80 ) THEN
          iyear = iyear + 2000
      ELSE
          iyear = iyear + 1900
      ENDIF
      naddblocks = IAND( rshift(iscr(6),4), i15)
      iday = IAND( iscr(6),i15) * 100 +
     *       IAND( rshift(iscr(7),12), i15) * 10 +
     *       IAND( rshift(iscr(7),8), i15)
      ihour = IAND( rshift(iscr(7),4), i15) * 10 +
     *        IAND(iscr(7),i15)
      imin = IAND( rshift(iscr(8),12), i15) * 10 +
     *       IAND( rshift(iscr(8), 8), i15)
      igmt = ihour*100 + imin
      IF( IAND(lprint,2) .NE. 0 ) PRINT *,' file',ifilen,
     *    ' day',iday,' hour',ihour,' min',imin
      IF( (fgmt .NE. 0 .OR. lgmt .NE. 2400) .AND. lday .EQ. iday
     *    .AND. igmt .GT. lgmt ) THEN                                   ! If the user gave LGMT and this shot is bigger, STOP
          istop = -1
          RETURN
      ENDIF
      IF( iday .GT. lday .AND. fday. LT. lday ) THEN                    ! fday set equal to lday if not using GMT
          istop = -1                                                    ! thus will not exit if reading by shot and
          RETURN                                                        ! day change occurs. GMK
      ENDIF
      isec = IAND( rshift(iscr(8),4), i15) * 10 +
     *       IAND(iscr(8), i15) 
      manuf = IAND( rshift(iscr(9),12), i15) * 10 +
     *        IAND( rshift(iscr(9),8), i15)
c      serial = IAND( rshift(iscr(9),4), i15) * 1000 +
c     *         IAND(iscr(9), i15) * 100 +
c     *         IAND( rshift(iscr(10),12), i15) * 10 +
c     *         IAND( rshift(iscr(10),8), i15 )
      blocked = IAND(iscr(10), i255)
      itemp = IAND( rshift(iscr(12),8), i255 )                            ! the sample interval in base 1/16 mils
      si = FLOAT(itemp) / 16. / 1000.                                   ! the REAL sample interval
      micros = FLOAT(itemp) / 16. * 1000.                               ! the sample interval in microseconds
      length = IAND(iscr(13),i15) * 10 +
     *         IAND( rshift(iscr(14),12), i15)
      temp = IAND( rshift(iscr(14),8), i15)
      rlen = (FLOAT(length) + temp/10. ) * 1.024
      ncsets = IAND(rshift(iscr(15),12),i15) * 10 +
     *         IAND( rshift(iscr(15),8), i15)
      IF( fcset .EQ. 0 ) fcset = ncsets
      IF( lcset .EQ. 0 ) lcset = ncsets
      nskew = IAND( rshift(iscr(15),4), i15) * 10 +
     *        IAND(iscr(15),i15)
      nextend = IAND(rshift(iscr(16),12),i15) * 10 +
     *          IAND( rshift(iscr(16),8), i15)
      IF( nextend .EQ. 165 ) nextend = lshift(IAND(iscr(19),i255),8) +
     &             IAND( rshift(iscr(20),8),i255)
      nexternal = IAND( rshift(iscr(16),4), i15) * 10 +
     *            IAND(iscr(16),i15)
      IF( nexternal .EQ. 165 ) nexternal=lshift(IAND(iscr(20),i255),8) +
     &             IAND( rshift(iscr(21),8),i255)
      ntotal = 2 + naddblocks + nskew + nextend + nexternal
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' ncsets=',ncsets,' nskew=',nskew,' nextend=',nextend,
     &      ' nexternal=',nexternal,' naddblocks=',naddblocks
          PRINT *,' total general header blocks ',ntotal 
      ENDIF
c**** Syntron uses the extended header file number only when the file
c**** number is > 9999.  Others use it all the time if the extended
c**** header is used.
c     this could be (iscr(1) .EQ. -1)
      IF( naddblocks .GT. 0 ) THEN
          IF( ifilen .EQ. 16665 ) ifilen = iscr(17) * 256 +
     &         IAND(rshift(iscr(18),8),i255)
c          rev = IAND(rshift(iscr(22),8),i255)+FLOAT(IAND(iscr(22),i255))/10.
          rev = IAND(rshift(iscr(22),8),i255)+(IAND(iscr(22),i255))/10.
          ntrailer = iscr(23) * 32
      ENDIF
      ishotno = ifilen
c****
c****  decode the scan type header - We're in deep dodo if there is more
c****  than one scan type (channel set) for the seismic data.  Assume that
c****  the last scan type is seismic and the first are for auxillary channels.
c****
      intrcs = 0
      DO 150 i = fcset, lcset
         index = (i+naddblocks)*16
         idelay(i) = iscr(index+2) * 2                                  ! a binary number - in 2 mil increments
c****    Bytes 7 & 8 are the MP factor, which changed in REV. 1 by 
c****    adding byte 7 as a "precision extension".  Byte 8 is the
c****    high order byte and 7 is the low order byte.  It used to be
c****    a count of .25 increments.  Now it's .000976525 (.25 / 256.).
c****    It's a sign and a magnitude.
c****    A byte swap does this for us!
         IF( iscr(index+4) .NE. 0 ) THEN
             isign = IAND(iscr(index+4),i128)
             iscr(index+4) = IAND(iscr(index+4),i127)
             CALL swap16(iscr(index+4),1)
c             temp = FLOAT(iscr(index+4))
             temp = (iscr(index+4))
             IF( isign .NE. 0 ) temp = -temp
             descalar(i) = 2. ** (temp * .25 / 256. )
             IF( IAND(lprint,8) .NE. 0 ) PRINT *,' Channel set ',i,
     &          ' has a descalar of ',descalar(i)   
         ENDIF
         n = IAND( rshift(iscr(index+5),12), i15) * 1000 +
     *       IAND( rshift(iscr(index+5),8), i15) * 100 +
     *       IAND( rshift(iscr(index+5),4), i15) * 10 +
     *       IAND(iscr(index+5),i15)
c         IF( fcset .EQ. lcset .AND. ncsets .NE. 1 ) 
c     &       PRINT *,'  Channel set ',i,' has ',n,' traces.'
         intrcs = intrcs + n
         delay(i) = FLOAT(idelay(i))/1000.
  150 CONTINUE
      IF( ltr .GT. intrcs .OR. ltr .EQ. 0 ) ltr = intrcs
      ltr1 = ltr
      IF( iday .lt. fday ) GOTO 100
      IF( iday .eq. fday .and. igmt .LT. fgmt ) GOTO 100
      IF( ftr .NE. 99999 ) fftr = ftr
      IF( nextend .NE. 0 ) THEN
          index = (naddblocks+ncsets+nskew)*16
c****     Look out. Syntron only put the file num in extended when file num > 9999
          IF( rlen .GT. 100. ) THEN                                     ! extended record length
              itemp = IAND( rshift(iscr(9),8), i255 )
c              rlen = FLOAT(iscr(8)) * 256. + FLOAT(itemp)
              rlen = (iscr(8)) * 256. + (itemp)
              rlen = rlen / 1000.                                       ! convert segd mils to secs
          ENDIF
      ENDIF
c**** Get the file number from the extended header if the extended header
c**** exists, unless it's Syntron, who only uses the file number from the 
c**** extended header when the file number is > 9999
      IF( naddblocks .NE. 0 ) THEN
          IF( manuf .NE. 34 .OR. ifilen .EQ. 16665 )
     &        ifilen = IAND( lrshift(lscr(9),8), 16777215 )              ! bytes 1-3
      ENDIF
c**** Always write the entire General Header to luntr0
c**** the only thing not in the SEGY trace header is the gun info.
      CALL podiscb( luntr0, 0, 0 )
      nbytes = ntotal * 32
      CALL wrdisc( luntr0, nbytes, 1 )
      CALL wrdiscb( luntr0, scr, nbytes )
c**** Do the external header
      IF( nexternal .GT. 0 ) THEN
          index = (1+naddblocks+ncsets+nskew+nextend) * 32 / 2
          DO i = 1, 400
             ldeo_dumb(i) = iscr(index+i)
          ENDDO
          IF( ldeo_ascii(1:6) .EQ. '$GPGGA' ) THEN
              nbytes = nexternal * 32
c****         gpgga uses getokec which uses cbufin
              cbufin = ldeo_ascii(1:nbytes)
              jchar = 1
              ncbuf = nbytes
c****         The time of the fix is not the shot time.
              CALL gpgga( ihourg, iming, isecg, ship_lat, ship_long,
     &             iquality, nsats, hdop, ldgps_sec, dgps_id )
              IF( IAND(lprint,8) .NE. 0 ) THEN
                  PRINT *,cbufin(1:nbytes)
                  PRINT *, ihourg, iming, isecg, ship_lat, ship_long,
     &             iquality, nsats, hdop, dgps_sec, dgps_id
              ENDIF
          ENDIF
c****     The LDEO/Syntrak/Spectra block starts with $1
          IF( ldeo_ascii(1:2) .EQ. '$1' ) THEN
              IF( ldeo_ascii(19:19) .EQ. '.' ) THEN
                  READ( ldeo_ascii,'(2x,4x,4x,2x,3I2,1x,I6,I4,2I2,3x,
     &                  I6,16x,2F11.6,F6.1,2F11.6,2F5.1,F4.1)' )
     &            ldgo_hr, ldgo_min, ldgo_sec, ldgo_mil,
     &            ldgo_yr, month, iday, ldgo_shotno,
     &            ship_lat, ship_long, wdepth, tail_lat, tail_long,
     &            gyro, cmg, speed
              ELSE
                  ldgo_mil = 0
                  READ( ldeo_ascii,'(2x,4x,4x,2x,3I2,I4,2I2,3x,
     &                  I6,16x,2F11.6,F6.1,2F11.6,2F5.1,F4.1)' )
     &            ldgo_hr, ldgo_min, ldgo_sec,
     &            ldgo_yr, month, iday, ldgo_shotno,
     &            ship_lat, ship_long, wdepth, tail_lat, tail_long,
     &            gyro, cmg, speed
              ENDIF
              ldgo_mil = FLOAT(ldgo_mil) / 1000.
              CALL caljul( month, iday, ldgo_yr, ldgo_day )
          ELSE
              CALL ldgo_tr0( ldeo_ascii, ldgo_shotno, ldgo_yr,
     &             ldgo_day, ldgo_hr, ldgo_min, ldgo_sec, ldgo_mil,
     &             ship_lat, ship_long, wdepth,
     &             tail_lat, tail_long, tail_dist, tail_bear )
              IF( IAND(lprint,256) .NE. 0 ) THEN
                  PRINT *,' ',ldeo_ascii(52:77),' ',ldeo_ascii(8:28),
     &              ' ',ldeo_ascii(158:162),
     &              ' ',ldeo_ascii(1:6),' ',ldeo_ascii(229:314)
              ENDIF
          ENDIF
          line_name = ldeo_ascii(140:149)
          IF( IAND(lprint,4) .NE. 0 ) THEN
              PRINT *,' Shot number: ', ldgo_shotno,
     &            ' byte index=',index*2
              PRINT *,' Joe date: ',ldgo_yr, ldgo_day,
     &                 ldgo_hr, ldgo_min, ldgo_sec, ldgo_mil
              PRINT *,' ship lat: ',ship_lat, ldeo_ascii(50:61)
              PRINT *,' ship long: ',ship_long, ldeo_ascii(63:75)
              PRINT *,' water depth: ',wdepth
              PRINT *,' tail pos: ',tail_lat, tail_long
              PRINT *,' tail dist and bearing: ',tail_dist, tail_bear
          ENDIF
          IF( ldgo_yr + ldgo_day .LE. 0 ) PRINT *,
     &             ' ***  WARNING  ***  Bad LDGO header on file ',ifilen
c****     luntr0 is circular.  geom and output need the current tr0
c****     make it look like the old Digicon stuff where section 11 was the
c****     LDEO ASCII header with 214 bytes and section 13 with the Digicourse
c****     ASCII.  Each section has a 4 byte header with I*2 word 1 the
c****     section number and word 2 the number of bytes, including the 4 byte
c****     header, in the section. e.g. LDEO has 214 bytes. 4 header and 210 info
	  iscr(13) = 1
	  iscr(14) = 214
          CALL podiscb( lunldeo, 0, 0 )
          CALL wrdiscb( lunldeo, scr, 28 )
          CALL wrdiscb( lunldeo, ldeo_dumb, 210 )
          iscr(1) = 13
          iscr(2) = 0                                               ! the length of section 13
	  ldeo_ascii(1:484) = ldeo_ascii(212:695)
c         READ( ldeo_ascii(1:4), '(I4)' ) iscr(2)
c****  ah f!%^&,  READ fails/bombs sioseis if the Digicourse stuff isn't there
          CALL dcode( ldeo_ascii, 4, areal, istat )
          IF( istat .EQ. 2 ) THEN
              iscr(2) = NINT(areal) + 4
          ELSE
              PRINT *,' LDEO "nav" block does not have Digicourse.'
          ENDIF
          CALL wrdiscb( lunldeo, scr, 4 )
          CALL wrdiscb( lunldeo, ldeo_ascii(5:5), 2028 )
      ENDIF
c****
c****   Find the proper demultiplexed trace - Do the DEMUX header
c****
  200 CONTINUE
      fgmt = 0                                                          ! take all from now on
      CALL magtap( iunit, scr, maxwrd, 21, istat )
c**** When the Ewing DSS gets a "SQTP error 84" which truncates the
c**** shot and starts the next shot without a file mark
      IF( istat .EQ. 48 ) THEN                                          ! 96 bytes is the SEG-D General Header
	  PRINT *,' ***  WARNING  ***  Unexpected SEG-D General Header.'
	  GOTO 125
      ENDIF
      IF( istat .LT. 0 ) THEN
          IF( istat .EQ. -1 ) GOTO 120                                  ! hit the end of shot!
          PRINT *,' ***  ERROR  ***  Tape read error of ',istat
          GOTO 200
      ENDIF
      nbytes = istat * 2
      IF( ifmt .EQ. 8036 ) nbytes = nsamps * 3
      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap16( scr, 4 )      ! swap bytes in the trace header
c**** The file number in the trace header might be bogus because it's only 4 BCD
c**** long and the real file number might be in the extended general header
c      ifilen = IAND( rshift(iscr(1),12), i15) * 1000 +                    ! file number
c     *         IAND( rshift(iscr(1),8), i15) * 100 +
c     *         IAND( rshift(iscr(1),4), i15) * 10 +
c     *         IAND(iscr(1),i15)  
c      IF( ifilen .EQ. 65535 ) ifilen = IAND( lscr(5), 16777215 )         ! bytes 18, 19, 20
      icsn = IAND( rshift(iscr(2),4), i15) * 10 + IAND(iscr(2),i15)         ! channel set number
c****  Remember this is packed BCD, not HEX. FF in BCD is 165
      IF( icsn .EQ. 165 ) THEN                       
          itemp = IAND( rshift(iscr(9),8), i255)
          icsn = IAND( iscr(8),i255 ) * 256 + itemp
      ENDIF
      IF( rlen .GT. secs .AND. secs .GT. 0. ) THEN
          temp = secs
          IF( stime .GT. delay(icsn) ) temp = secs +(stime-delay(icsn)) ! convert out to stime+secs
      ELSE
          IF( rlen .NE. 0. ) THEN
              temp = rlen
          ELSE
              itemp = 32                                                ! bits per word
              IF( format .EQ. 8015 ) itemp = 20
              temp = (istat-10) * 16 / itemp * si
          ENDIF
          IF( secs .GT. 0. ) temp = secs
      ENDIF
      nsamps = NINT(temp / si)
c****
c**** The LDGO/Digicon system put nav and other non seismic stuff in
c**** the first channel set.  LDEO puts their stuff in "section 11" of
c**** the Digicon trace.  LDEO uses ASCII.  First, find section 11.
c**** When writing the trace0 disk file, use a constant because trace0
c**** is really a variable length record - and incorrect sometimes.
c****
      IF( icsn .EQ. 1 .AND. manuf .EQ. 21 ) THEN
          IF( luntr0 .NE. 0 ) THEN
c****     luntr0 is circular.  geom and output need the current tr0
              CALL podiscb( luntr0, 0, 0 )
              CALL wrdiscb( luntr0, iscr, 3940 )
          ENDIF
          index = 13
          DO 300 j = 1, 11
	     IF( IAND(lprint,8) .NE. 0 ) THEN
	         DO i = 1, 105
                    ldeo_dumb(i) = iscr(index+1+i)
                 ENDDO
             ENDIF
             IF( iscr(index) .EQ. 11 .OR. iscr(index) .EQ. 13 ) THEN
	            DO i = 1, 105
                    ldeo_dumb(i) = iscr(index+1+i)
                 ENDDO
             ENDIF
             IF( iscr(index) .EQ. 11 ) THEN
	            CALL ldgo_tr0( ldeo_ascii, ldgo_shotno, ldgo_yr,
     &               ldgo_day, ldgo_hr, ldgo_min, ldgo_sec, ldgo_mil,
     &               ship_lat, ship_long, wdepth,
     &               tail_lat, tail_long, tail_dist, tail_bear )
	            IF( IAND(lprint,4) .NE. 0 ) THEN
	                PRINT *,' Shot number: ', ldgo_shotno
	                PRINT *,' Joe date: ',ldgo_yr, ldgo_day,
     &                 ldgo_hr, ldgo_min, ldgo_sec, ldgo_mil
	                PRINT *,' ship pos: ',ship_lat, ship_long
	                PRINT *,' tail pos: ',tail_lat, tail_long
	                PRINT *,' tail dist and bearing: ',tail_dist,
     &                   tail_bear
                 ENDIF
                 IF( ldgo_yr + ldgo_day .LE. 0 ) PRINT *,
     &             ' ***  WARNING  ***  Bad LDGO header on file ',ifilen
             ENDIF
             index = index + iscr(index+1) / 2
  300     CONTINUE
c****     Watch out for file numbers incrementing and shot numbers
c****     not incrementing!  If that happens, increment the shot
c****     number.  The time will be bad, but I don't want to
c****     "correct" that here because I don't know the rep rate!
          IF( ldgo_shotno .LE. lastshot .AND. ifilen .NE. lastfile )THEN
              PRINT *,' ***  WARNING  ***  Bad LDGO shot number of ',
     &             ldgo_shotno,' Using ', lastshot + 1
              ldgo_shotno = lastshot + 1
          ENDIF
          lastshot = ldgo_shotno
          lastfile = ifilen
          GOTO 200
      ENDIF
c****
c****   End of LDGO
c****
      itrcno = IAND( rshift(iscr(3),12), i15) * 1000 +   ! trace number
     *         IAND( rshift(iscr(3),8), i15) * 100 +
     *         IAND( rshift(iscr(3),4), i15) * 10 +
     *         IAND(iscr(3),i15)  
      IF( IAND(lprint,2) .NE. 0 ) 
     *     PRINT *,' ifilen=',ifilen,' trace=',itrcno,' fftr=',fftr,
     *       ' ltr=',ltr,' cn=',icsn,' igmt=',igmt,' ffilen=',ffilen
c**** The LDGO cable "rebuilds" unexpectedly and writes some crazy
c**** trace numbers.  It looks like traces 2-12 get clobbered.  Trace
c**** 11 gets numbered 155 and is a bitch to get rid of, so try to
c**** detect that here and spare the processor some grief later.
      IF( manuf .EQ. 21 .AND. itrcno .GT. 1000 ) THEN
	  badtrace = 1
          PRINT *,' ***  WARNING  ***  Dropping bad trace numbered ',
     &        itrcno
          GOTO 200
      ENDIF
      IF( badtrace .EQ. 1 .AND. itrcno .EQ. 155 ) THEN
          PRINT *,' ***  WARNING  ***  Dropping bad trace numbered 155.'
          GOTO 200
      ENDIF
      badtrace = 0
      IF( ffilen .NE. 99999 .AND. ifilen .LT. ffilen ) GOTO 100         ! find the first shot
c     check for fcset/lcset AFTER LDGO/Digicon "trace 0"
      IF( icsn .LT. fcset .OR. icsn .GT. lcset ) GOTO 200
c***********************************************************
c****  This statement allows all shots between fno and lno
      IF( filinc .EQ. 99999 ) ffilen = 99999                             ! got ffilen, so change it to accept all files
c***********************************************************
c****  This statement insists that the trace numbers increase by 1
c      IF( itrcno .NE. fftr .AND. ltr .NE. 0 ) GOTO 200                   ! get the right trace
c***********************************************************
c     If ftr is not given, then take all traces in all channel sets.
      IF( ftr .NE. 99999 ) THEN
c****     This statement allows all traces numbered between ftr and ltr
          IF( itrcno .LT. fftr .OR. (itrcno .GT. ltr .AND. ltr .NE. 0 ))
     &        GOTO 200
      ENDIF
      itword = iscr(4)          ! time word
      itemp = IAND( rshift(iscr(5),8), i255)
      tword = itword + FLOAT(itemp)/256.
      ithext = IAND(iscr(5),i255)
      itemp = IAND( rshift(iscr(6),8), i255 )   ! sample skew 
      skew = FLOAT(itemp) / 256.
      itimeb = iscr(7)                          ! time break
      itemp = IAND( rshift(iscr(8),8), i255)
      tbreak = itimeb + FLOAT(itemp)/256.
c****
c****    GOT A TRACE TO KEEP!   unpack the data and create an SEGY trace header
c****
c trace header is 20 bytes, extended trace header is 32 bytes per block
      IF( ithext .EQ. 0 ) THEN
          index = 6
      ELSE
          index = ithext * 8 + 6
      ENDIF
c****  Use the numbers of samples read from tape rather than the header.
c****  In Syntron's case, the header is WRONG!
      IF( secs .EQ. 0 ) nsamps = (nbytes - (index-1)*4) / 4
      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) 
     &    CALL swap32( scr(index), nsamps ) 
      IF( format .EQ. 8015 ) 
     &    CALL segd20( scr(index),buf(numhdr+1),nsamps)
      IF( ifmt .EQ. 8036 ) THEN
          CALL i24i32( scr(index), buf(numhdr+1), nsamps )
          DO i = 1, nsamps
             buf(numhdr+i) = FLOAT( lbuf(numhdr+i) )
          ENDDO
      ENDIF
      IF( format .EQ. 8048 ) 
     &    CALL ibm2fp( scr(index),nsamps,buf(numhdr+1))
      IF( format .EQ. 8058 ) THEN                                       ! IEEE f.p.
          index = index - 1
          DO 350 i = 1, nsamps
  350     buf(numhdr+i) = scr(index+i)
      ENDIF
      IF( stime .GT. 0. .AND. stime .GT. delay(icsn) ) THEN             ! should we get rid of the front of the data trace?
          n = NINT( (stime - delay(icsn)) / si )
          nsamps = nsamps - n
          DO 400 i = 1, nsamps
             buf(numhdr+i) = buf(numhdr+n+i)                            ! move the data so stime is the first data sample
  400     CONTINUE
c          delay(icsn) = stime
c          idelay(icsn) = stime * 1000.
      ENDIF
      IF( decimf .GT. 1 ) THEN
          j = 1
          DO 450 i = 1, nsamps, decimf
             buf(numhdr+j) = buf(numhdr+i)
             j = j + 1
  450     CONTINUE
      ENDIF 
c**** Descale by the MP factor if the user asks for it.  icsn is the
c**** current trace's channel set number (I think).
      IF( descale .EQ. 1 .AND. descalar(icsn) .NE. 0 ) THEN
          temp = descalar(icsn)
          DO i = 1, numdat
             buf(numhdr+i) = buf(numhdr+i) * temp
          ENDDO
      ENDIF
      DO 500 i = 1, 60 
  500 lbuf(i) = 0
      lbuf(3) = ishotno
      IF( renum .GT. 0 ) lbuf(3) = renum
      IF( retrac .EQ. 0 ) THEN
          lbuf(4) = itrcno
      ELSE
c***      renumber the traces from RETRAC if it's a new shot
          IF( ishotno .NE. lastshotno ) THEN
              otrcno = retrac
              lastshotno = ishotno
          ENDIF
          lbuf(4) = otrcno
          otrcno = otrcno + 1
      ENDIF
      lbuf(5) = ifilen
      ibuf(15) = 1
      buf(46) = AMAX1(delay(icsn),stime)
      ibuf(55) = NINT(buf(46) * 1000.)
      ibuf(58) = (nsamps + decimf - 1) / decimf
      numdat =  ibuf(58)
      ibuf(59) = micros * decimf
      ibuf(79) = iyear
      ibuf(80) = iday
      ibuf(81) = ihour
      ibuf(82) = imin
      ibuf(83) = isec
      ibuf(84) = 1
      buf(49) = si * decimf
      IF( ntrgat .GT. 0 ) THEN
          mtrgat = mtrgat + 1
          IF( mtrgat .EQ. ntrgat ) THEN
              lbuf(51) = -1
              mtrgat = 0
          ENDIF
          lbuf(6) = lbuf(3)
      ENDIF
      IF( ldgo_shotno .NE. 0 ) THEN
          lbuf(3) = ldgo_shotno
          IF( renum .GT. 0 ) lbuf(3) = renum
          IF( wdepth .EQ. 0. .AND. owdepth .NE. -1. ) THEN
          PRINT *,' ***  WARNING  ***  Bad water depth, using previous.'
              wdepth = owdepth
          ENDIF
c****     Don't save/use bad water depths
          IF( wdepth .GT. 6 ) THEN
              owdepth = wdepth
          ELSE
              wdepth = 0.
          ENDIF
          lbuf(16) = wdepth
          ibuf(79) = ldgo_yr
          ibuf(80) = ldgo_day
          ibuf(81) = ldgo_hr
          ibuf(82) = ldgo_min
          ibuf(83) = ldgo_sec
c          ibuf(84) = NINT( (ibuf(83) - sec_ldgo ) * 1000.)
          ibuf(84) = ldgo_mil
c****  Ah sh.  Put the ship position in the group position because
c****  process geom uses 19 & 20 for the x/y coordinate and geom is
c****  needed to get the steamer depth.   Remember that x is longitude
c****  and y is latitude.
c          ibuf(36) = -10
c****    FUCKUP   factor of 6 too big!!!
c          lbuf(19) = NINT(ship_long*360.*60.*10.)
c          lbuf(20) = NINT(ship_lat*360.*60.*10.)
c          lbuf(21) = NINT(ship_long*360.*60.*10.)
c          lbuf(22) = NINT(ship_lat*360.*60.*10.)
          ibuf(36) = -100
c****     arcsec = 60sec/min * 60min/deg = 3600. sec/deg
          lbuf(19) = NINT(ship_long*3600.*100.)
          lbuf(20) = NINT(ship_lat*3600.*100.)
          lbuf(21) = NINT(ship_long*3600.*100.)
          lbuf(22) = NINT(ship_lat*3600.*100.)
          ibuf(45) = 2
          buf(50) = wdepth / 750.
          buf(54) = wdepth
      ENDIF
      IF( IAND(lprint,4) .NE. 0 ) THEN
          PRINT *,' SEGY header:'
          PRINT *,lbuf(3),lbuf(4),ibuf(15),ibuf(55),ibuf(58),ibuf(59)
          PRINT *,lbuf(19),lbuf(20)
          PRINT *,(ibuf(i),i=79,84),buf(46),buf(49),lbuf(51)
      ENDIF
c****
c****    finished a trace, now set up ftr for the next shot
c****
      IF( ftr .NE. 99999 ) THEN
          fftr = fftr + trinc
c     ltr is set to intrcs if it wasn't given
c****   Check itrcno too since fftr is just a count, whereas itrcno 
c****   comes from the data!
          IF( fftr .GT. ltr .OR. itrcno .GE. ltr ) THEN                     ! past the last trace requested?
              fftr = ftr
              IF( ffilen .NE. 99999 .AND. filinc .NE. 99999 ) 
     &            ffilen = ffilen + filinc
              IF( ifilen .GE. lfilen ) THEN
                  IF( mlists .EQ. nlists ) THEN
                      istop = 1
                  ELSE
                      getlist = .TRUE.
                  ENDIF
              ENDIF
          ENDIF
      ENDIF
c**** make lfilen work if ltr is not given    -   pch 24 aug 96
c      IF( ltr .EQ. 0 .AND. ifilen .GT. lfilen ) THEN
c     That didn't work because ltr is set to intrcs if it wasn't given, so
c       make it intrcs - pch sept 2000
c**** ftr is preset to 99999. ltr is preset to 0, but segdex changes it to intrcs
c****  Ah shoot, when doing multiple channel sets, itrcno is the
c****  trace number in the channel set, but ltr is counting the
c****  total number of traces in the file.  Let's assume that
c****  when that happens, retrac was used and the segy trace number is
c****  the count of the output traces (assumes ftr was not given).
      IF( ftr .EQ. 99999 .AND. ltr .EQ. intrcs .AND. 
     &    lbuf(4) .EQ. ltr .AND. ifilen .EQ. lfilen ) THEN
          IF( mlists .EQ. nlists ) THEN
              istop = 1
          ELSE
              getlist = .TRUE.
          ENDIF
      ENDIF
c**** renum won't work if multiple channel sets. (intrcs is set to the sum
c**** of all the traces whereas itrcno is the trace number within each channel set.
      IF( renum .GT. 0 .AND. itrcno .GE. intrcs ) renum = renum + 1
      in = 0                                                            ! signal that the data is not in the ap
      RETURN
      END
