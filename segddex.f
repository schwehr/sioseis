      SUBROUTINEsegddex( buf, lbuf, ibuf, scr, lscr, iscr, istop )
c SEG-D disk input - similar to segdex
c
c (C) The Regents of the University of California
c Written by Paul Henkart, July 2002
c
c mod ?? Aug 02 - Increase lat/long field size in log file.
c mod 19 Sep 02 - Allow the files in a stack not have / as the first
c                 character to allow symbolic links.
c mod 21 Sep 02 - Write a message in the log file when newfile is set.
c mod 29 Sep 02 - Don't save/use bad water depths.
c mod 4 Oct 02 - Get the line name from the LDEO header (don't know
c                what to do with it though!)
c mod 8 Oct 02 - ftr/ltr and ipath didn't terminate correctly
c mod 19 Mar 03 - Always write the general header to luntr0 and
c                 write the ldeo external header to lunldeo if present.
c mod 20 Mar 03 - Move ns_done count to be after stack wait.
c mod 21 Mar 03 - Do the log file differently if ldeo_shotno = 0
c mod 8 Jul 03 - Seisnet failed because the seisnet header was a different
c                size than expected.  Find the first trace differently!
c mod 6 Mar 04 - Allow format 8036 - 24 bit integers
c mod 29 Apr 04 - Use the record length from GH#2 if rlen = FFF in GH#1.
c               - Decode the number of General Header blocks
c               - Partially set up the SIO-RT nav and depth files
c mod 4 May 04 - nsamps calculated from GH#2 on PC-Linux need explicit
c                NINT(FLOAT(length)/si*1000.)
c mod 5 May 04 - Get the nav from the extended header if its starts 
c                with $GPGGA
c mod 6 June 04 - Honor SEG-D Rev. 1 Expanded file number
c               - Start looking at the MP Factor - descaling factor.
c mod 27 June 04 - Apply the MP descaling factor.
c mod 14 Sep 04 - Extended file number was wrong.
c mod 19 Sep 04 - The MP factor is sign AND magnitude!
c mod 27 Sep 04 - Make up a shot number if the LDEO shot number = 0.
c mod 13 Jun 05 - Set newfile = 0 on every call/trace.
c               - lprint 32 means print the External header in ASCII.
c               - remove the sio realtime stuff since it won't happen!
c               - Add a call to Lee Ellett's External header.
c mod 28 Jul 05 - Add lprint of the K constants.
c mod 31 Jan 06 - Put the SIO lat/long in the SEG-Y header regardless of
c                 it's time of fix.  i.e. ignore time of fix.
c mod 12 Oct 06 - Set ship_lat and ship_long to 0 on every shot before
c                 doing the external header in case it's not there.
c               - Get rid of Healy05 and Healy06 calls.
c mod 16 Oct 06 - Generalize $GPGGA to be NMEA strings in general.
c               - Use $DBT for NMEA water depth.
c               - Get rid of Leeshdr.
c mod 10 Jan 06 - stack didn't honor extended shot numbers (> 9999)
c mod 18 July 07 - Allow $SDDBT in addition to $DBT
c mod 14 Aug 07 - g95 IAND requires arguments to be same type and kind.
c               - g95 can't declare type and set value on same statement
c mod 17 Apr 08 - The LDEO external header was broken.
c mod 21 Apr 08 - Change iformat to 6 (LDEO) if LDEO external header ($1)
c               - Add nav to non-ldeo log file.
c mod 19 May 08 - Write the water depth to real word 54 (why was it commented out?)
c mod 20 May 08 - The extended file number was screwed up
c               - FLUSH the log so it can been seen immediately. NOT ON LANGSETH
c mod 7 July 08 - Reset ftr on each channel set
c mod 17 Jul 08 - Add shot_inc (odd/even) to STACK
c mod 24 Jul 08 - Use lrshift to shift a long integer (filen in additional block)
c mod 28 Jul 08 - Allow shooting the line backwards with decreasing shot numbers
c               - but catch a ldgo_shot of 0
c mod 24 May 10 - Use lbuf(16) rather than buf(54) for water depth
c mod 22 Jul 11 - Mess with ***  WARNING  *** Water depth has not changed in 50 shots.
c               - FLUSH (fortran function) doesn't work on cygwin
c mod 18 Nov 11 - Redo reading NMEA strings from the external header
c mod 21 Nov 11 - lrshift (and c) don't zero the vacated bits on right shifts
c  mod 19 Dec 11 - Honor file "in" when STACK is used.
c mod 13 Feb 15 - Very large external header caused buffer overflow.
c               - print warning if format HTI is given and data are segd rev 1
c mod 6 Jun 15 - Honor extended number of channel sets in general header #2
c mod Feb 16 - really the change is in gpgga for bad Geo Eel gps strings
c mod 29 Mar 16 - Big modify to gpgga & added arguments mils & istat
c               - put the mils in SEGY (84) if mils is available (it's 9999 if not)
c
      PARAMETER ( maxndeps = 50 )
c     remember that scr, iscr, lscr are equivalenced
      DIMENSION buf(1000), scr(1000)
      DIMENSION delay(99), idelay(99), H2O_depths(maxndeps),descalar(99)
      INTEGER*2 ibuf(1000), iscr(1000), rshift, lshift
c   we need to define the length of constants for IAND to work right
      INTEGER*2 i15, i255, i127, i128
      DATA i15/15/, i255/255/, i127/127/, i128/128/
      DATA i16777215/16777215/   ! FFFFFF or lower 24 bits on
      INTEGER lbuf(1000), lscr(1000)
      REAL*8 ship_lat, ship_long, tail_lat, tail_long
      COMMON /sioap/ iasgnd, irelse, in, iout,nextad,lapsiz,ifree,iuseap
c**** segdin and segddin are mutually exclusive, so segddin uses segdin's common
      COMMON /segdin/ junit, nlists, nwrds, luntr0, luntr0a, lunldeo,
     &                shot_inc
      COMMON /segddin/ cpath
      CHARACTER*80 cpath
      COMMON /readt/ iunit, numhdr, numdat, ihunit, ireeln, intrcs
c**** writet is needed so we can signal output to write a new segy file
      COMMON /WRITET/ounit,NSAMPSO,OREEL,POSAFT,OFMT,NTRCS,LFOR,ONUMTR,
     &       nfskipo, rewindo, newfile, itrace0, ounit2
      INTEGER*4 OFMT,OUNIT,POSAFT,ONUMTR,OREEL, rewindo
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      CHARACTER*200 cbufin
      COMMON /sioln1/cbufin
      COMMON /sioln2/ jchar, ncbuf
      COMMON /sioln4/ ICHAR4, NCHARS4, iprint4, lunpo4          ! used by rline1
      COMMON /inputdev/inputdev
      CHARACTER*80 inputdev, token, top1, top2, filename
      CHARACTER*800 ldeo_ascii
      INTEGER*2 ldeo_dumb(400)
      EQUIVALENCE (ldeo_ascii,ldeo_dumb(1))
      CHARACTER*10 line_name
      CHARACTER*3 dgps_id
      INTEGER*2 idumb(2)
      EQUIVALENCE (idumb(1),ldumb)
      INTEGER ffilen, filinc, ftr, trinc, decimf, fgmt, fday, renum
      INTEGER naddblocks, manuf, serial, fftr, fcset, shot_inc,
     &        gmtinc, stack, badtrace, retrac, descale, trcount
      LOGICAL first, getlist, iexist, ldeoopen
      DATA lastshot/0/, lastfilen/0/, ldeoopen/.TRUE./, badtrace/0/
      SAVE
      DATA first/.TRUE./, mtrgat/0/, ldgo_shotno/0/, nchanges/0/
      DATA wdepth /0./, owdepth/-1./, getnewshot/0/, istop_stack/0/
      DATA ns_done/-1/                                                  ! the segdded read the first segd general header
      DATA lunin/0/, idifference/0/, lastcsn/0/
      DATA ship_lat/0./, ship_long/0./, rev/0./
      DATA milsg/9999/
c****
c****  format = 1, 
c****  format = 2, Seisnet/LDEOLIST
c****  format = 3, Hydroscience with extra 32 byte header
c****  format = 4, SIO-RT ---  This never came to fruition - where
c****              there was an external file with all the metadata.
c****  format = 5, Geometrics - ASCII (nonNMEA) info in the external header.
c****  format = 6, LDEO external header - same as lunldeo non-zero.
c****  format = 7, ION 32 bit byte swap on all reads
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
          DO i = 1, maxndeps
             H2O_depths(i) = -1.
          ENDDO
          nh2o = 0
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
          fcset = lscr(18)
          lcset = lscr(19)
          gmtinc = lscr(20)
          iformat = lscr(21)
          retrac = lscr(22)
          stack = lscr(23)
          list = lscr(24)
          lunlog = lscr(25)
          nspfile = lscr(26)
          lunotape = lscr(27)
          ldeolist = lscr(29)
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
     *             idelay(1),nsamps,micros,iyear,iday,ihour,imin
              PRINT *, isec, si, fcset, lcset, gmtinc, ifmt
              PRINT *, iformat,retrac,stack,list,lunlog,nspfile,
     &                 lunotape,ldeolist,descale
          ENDIF
          itrcno = 0
          IF( stack + ldeolist .NE. 0 .AND. lunin .EQ. 0 ) THEN
              CALL getfil( 2, lunin, token, istat )                     ! get a lun for the IN file
              OPEN(UNIT=lunin,FILE='in',STATUS='OLD',IOSTAT=istat)
              CLOSE(UNIT=lunin,STATUS='DELETE')
              OPEN(UNIT=lunin,FILE='IN',STATUS='OLD',IOSTAT=istat)
              CLOSE(UNIT=lunin,STATUS='DELETE')
          ENDIF
      ENDIF
      IF( getnewshot .GT. 0 ) GOTO 1000
      IF( itrcno .GT. 0 ) GOTO 200
      CALL podisc( iunit, 1, 0 )
c**** 
c****    get a general headers
c****
  100 CONTINUE
c**** Seisnet has a header before the SEG-D General header and it's in
c****  little endian (pc byte order).
c   word 2 = number of channels
c        3 = number of bytes in the Seisnet header
c        4 = number of bytes in the General header
c        5 = 3+4 = address of 
      IF( iformat .EQ. 2 ) THEN
          CALL rddisc( iunit, lscr, 3, istat )
          IF( icompt .NE. 2 .AND. icompt .NE. 4 ) CALL swap32(lscr(3),1)
          nbytes = lscr(3)
          CALL podiscb( iunit, 1, nbytes )
      ENDIF          
  130 CALL rddiscb( iunit, iscr(1), 32, istat )
      IF( istat .NE. 32 ) GOTO 980
      indx = 17
      IF( IAND(lprint,16) .NE. 0 ) THEN
          CALL tohex( iscr(1), 32, token )
          PRINT *,' General Header #1, ',token(1:64)
      ENDIF
      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     *        CALL swap16( iscr(1), 32 )                                ! swap bytes on DEC computers
      ifilen = IAND( rshift(iscr(1),12), i15) * 1000 +                    ! file number
     *         IAND( rshift(iscr(1),8), i15) * 100 +
     *         IAND( rshift(iscr(1),4), i15) * 10 +
     *         IAND(iscr(1),i15)  
      ns_done = ns_done + 1
      newfile = 0
      ifmt = IAND( rshift(iscr(2),12), i15) * 1000 +
     *       IAND( rshift(iscr(2),8), i15) * 100 +
     *       IAND( rshift(iscr(2),4), i15) * 10 +
     *       IAND( iscr(2),i15 )
c**** HydroScience has an extra block with a different trace length
c      IF( ifmt .EQ. 3336 .AND. iformat .EQ. 3 ) THEN
      IF( ifmt .EQ. 8036 .AND. iformat .EQ. 3 ) THEN
          IF( icompt .NE. 2 .AND. icompt .NE. 4 ) THEN
              CALL swap32( lscr(1), 8 )
          ELSE
              CALL swap16( iscr(1), 32 )                                
          ENDIF
          itrsize = lscr(4)
          GOTO 130
      ENDIF
      IF( IAND(lprint,16) .NE. 0 ) THEN
          CALL tohex( iscr(indx+2), 6, token )
          PRINT *,' General Constants: ',token(1:6)
      ENDIF
      iyear = IAND( rshift(iscr(6),12), i15) * 10 +
     *        IAND( rshift(iscr(6),8), i15)
      IF( iyear .LT. 80 ) THEN
          iyear = iyear + 2000
      ELSE
          iyear = iyear + 1900
      ENDIF
      naddblocks = IAND( rshift(iscr(6),4), i15)
      IF( naddblocks .GT. 0 ) THEN
          nbytes = naddblocks * 32
          CALL rddiscb( iunit, iscr(indx), nbytes, istat )
          IF( istat .NE. nbytes ) GOTO 980
c****     seisnet doesn't swap the general header
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     &        CALL swap16( iscr(indx), nbytes/2 )
          IF( IAND(lprint,16) .NE. 0 ) THEN
c****         watch out - token is character*80
              CALL tohex( iscr(indx), 40, token )
              PRINT *,' additional General Headers: ',token(1:80)
          ENDIF
          rev = IAND(rshift(iscr(22),8),i255) + 
     &          REAL(IAND(iscr(22),i255))/10.
          ntrailer = iscr(23) * 32
c****     16665 = 15000 + 1500 + 150 + 15 = FFFF
          IF( ifilen .EQ. 16665 ) THEN
c              CALL tohex(iscr(indx),4,token)
c              print *,' token=',token(1:8)
              IF( icompt .NE. 2 .AND. icompt .NE. 4 ) THEN
                  idumb(1) = iscr(indx)
                  idumb(2) = iscr(indx+1)
              ELSE
c****             it was 16 bit swapped earlier, so swap the short word order
                  idumb(1) = iscr(indx+1)
                  idumb(2) = iscr(indx)
              ENDIF
              ldumb = lrshift(ldumb,8)
              ldumb = IAND(ldumb,i16777215)
              ifilen = ldumb
          ENDIF
          indx = indx + nbytes / 2
      ENDIF
      IF( stack .GT. 0 .AND. ifilen .EQ. lastfilen ) THEN
          CALL sleep(3)
          GOTO 1000
      ENDIF
      IF( ns_done .EQ. nspfile ) THEN
          newfile = 1
          ns_done = 0
      ENDIF
      ishotno = ifilen
      iday = IAND( iscr(6),i15) * 100 +
     *       IAND( rshift(iscr(7),12), i15) * 10 +
     *       IAND( rshift(iscr(7),8), i15)
      ihour = IAND( rshift(iscr(7),4), i15) * 10 +
     *        IAND(iscr(7),i15)
      imin = IAND( rshift(iscr(8),12), i15) * 10 +
     *       IAND( rshift(iscr(8), 8), i15)
      igmt = ihour*100 + imin
      IF( IAND(lprint,2) .NE. 0 ) PRINT *,' file',ifilen,
     *    ' day',iday,' hour',ihour,' min',imin,' mils',milsg
      IF( (fgmt .NE. 0 .OR. lgmt .NE. 2400) .AND. lday .EQ. iday
     *    .AND. igmt .GT. lgmt ) THEN                                   ! If the user gave LGMT and this shot is bigger, STOP
          PRINT *,' User stop with GMT parameters.'
          istop = -1
          RETURN
      ENDIF
      IF( iday .GT. lday .AND. fday. LT. lday ) THEN                    ! fday set equal to lday if not using GMT
          PRINT *,' User stop with DAY parameters.'
          istop = -1                                                    ! thus will not exit if reading by shot and
          RETURN                                                        ! day change occurs. GMK
      ENDIF
      isec = IAND( rshift(iscr(8),4), i15) * 10 +
     *       IAND(iscr(8), i15) 
      manuf = IAND( rshift(iscr(9),12), i15) * 10 +
     *        IAND( rshift(iscr(9),8), i15)
      serial = IAND( rshift(iscr(9),4), i15) * 1000 +
     *         IAND(iscr(9), i15) * 100 +
     *         IAND( rshift(iscr(10),12), i15) * 10 +
     *         IAND( rshift(iscr(10),8), i15 )
      itemp = IAND( rshift(iscr(12),8), i255 )                            ! the sample interval in base 1/16 mils
      si = FLOAT(itemp) / 16. / 1000.                                   ! the REAL sample interval
      micros = FLOAT(itemp) / 16. * 1000.                               ! the sample interval in microseconds
      length = IAND(iscr(13),i15) * 10 +
     *         IAND( rshift(iscr(14),12), i15)
      temp = IAND( rshift(iscr(14),8), i15)
      rlen = (FLOAT(length) + temp/10. ) * 1.024
      nsamps = rlen / si
      IF( iformat .EQ. 3 ) nsamps = (itrsize-20)/3
      ncsets = IAND(rshift(iscr(15),12),i15) * 10 +
     *         IAND( rshift(iscr(15),8), i15)
      IF( ncsets .EQ. 165 ) THEN
          ncsets = IAND( iscr(18),i15) * 10 +
     &             IAND( rshift(iscr(19),12), i15)
          IF( icompt .EQ. 2 .or. icompt .EQ. 4 ) THEN
c****  between fortran and endiannes this sucks.  We did a 16 bit byte swap before
              idumb(1) = iscr(17)
              idumb(2) = iscr(18)
              CALL swap16( idumb, 2 )
              CALL swap32( ldumb, 1 )
              ncsets = IAND( ldumb, i15 ) * 10
              idumb(1) = iscr(19)
              idumb(2) = iscr(20)
              CALL swap16( idumb, 2 )
              ncsets = ncsets + idumb(1)
          ENDIF
      ENDIF
      IF( ncsets .GT. 0 ) THEN
          nbytes = 32
          DO i = 1, ncsets
             CALL rddiscb( iunit, iscr(indx), nbytes, istat )
             IF( istat .NE. nbytes ) GOTO 980
             IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     &           CALL swap16( iscr(indx), nbytes/2 )
             IF( IAND(lprint,16) .NE. 0 ) THEN
                 CALL tohex( iscr(indx), 40, token )
                 PRINT *,' Channel Set header:',token(1:80)
             ENDIF
             indx = indx + nbytes / 2
          ENDDO
      ENDIF
      nskew = IAND( rshift(iscr(15),4), i15) * 10 +
     *        IAND(iscr(15),i15)
      IF( nskew .GT. 0 ) THEN
          nbytes = nskew * 32
          CALL rddiscb( iunit, iscr(indx), nbytes, istat )
          IF( istat .NE. nbytes ) GOTO 980
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     &        CALL swap16( iscr(indx), nbytes/2 )
          IF( IAND(lprint,16) .NE. 0 ) THEN
		    CALL tohex( iscr(indx), 40, token )
		    PRINT *,' at nskew, ',token(1:80)
          ENDIF
          indx = indx + nbytes / 2
      ENDIF
      nextend = IAND(rshift(iscr(16),12),i15) * 10 +
     *          IAND( rshift(iscr(16),8), i15)
      IF( nextend .EQ. 165 ) nextend = lshift(IAND(iscr(19),i255),8) +
     &             IAND( rshift(iscr(20),8),i255)
      IF( nextend .GT. 0 ) THEN
          nbytes = nextend * 32
          CALL rddiscb( iunit, iscr(indx), nbytes, istat )
          IF( istat .NE. nbytes ) GOTO 980
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     &        CALL swap16( iscr(indx), nbytes/2 )
          IF( IAND(lprint,16) .NE. 0 ) THEN
              PRINT *,' Extended header length = ',nbytes,' bytes.'
              CALL tohex( iscr(indx), 40, token )
              PRINT *,' Extended header (partial):'
              PRINT *,token(1:MIN0(nbytes*2,80))
          ENDIF
          indx = indx + nbytes / 2
      ENDIF
      ship_lat = 0.
      ship_long = 0.
      nexternal = IAND( rshift(iscr(16),4), i15) * 10 +
     *            IAND(iscr(16),i15)
      IF( nexternal .EQ. 165 ) nexternal=lshift(IAND(iscr(20),i255),8) +
     &             IAND( rshift(iscr(21),8),i255)
      IF( nexternal .GT. 0 ) THEN
          nbytes = nexternal * 32
c****   Ah shit.  Hydroscience has a huge (32k) external header which is too big
c          CALL rddiscb( iunit, iscr(indx), nbytes, istat )
c          IF( istat .NE. nbytes ) GOTO 980
          CALL adrdisc( iunit, laddress )
          ntodo = MIN0(nbytes,4000)    ! 4000 is arbitrary
          CALL rddiscb( iunit, iscr(indx), ntodo, istat )
          IF( IAND(lprint,16) .NE. 0 ) THEN
              PRINT *,' External header length = ',nbytes,' bytes.'
              CALL tohex( iscr(indx), 40, token )
              PRINT *,' External header (partial): ',token(1:80)
          ENDIF
          IF( IAND(lprint,32) .NE. 0 ) THEN
              CALL podiscb( iunit, 2, -ntodo)
              CALL rddiscb( iunit, token, 80, istat )
              CALL podiscb( iunit, 2, ntodo-80 )
              PRINT *,token(1:80)
          ENDIF
c****     watch out for ldeo_ascii size
          DO i = 1, MIN0(ntodo/2,400)
             ldeo_dumb(i) = iscr(indx+i-1)
          ENDDO
          IF( IAND(lprint,16) .NE. 0 ) THEN
              PRINT *,' 1:80=',ldeo_ascii(1:80)
              PRINT *,' 81:160=',ldeo_ascii(81:160)
              PRINT *,' 161:240=',ldeo_ascii(161:240)
              PRINT *,' 241:320=',ldeo_ascii(241:320)
              PRINT *,' 321:400=',ldeo_ascii(321:400)
          ENDIF
          IF( ldeo_ascii(1:2) .EQ. '$1' .AND. iformat .NE. 6 ) THEN
              PRINT *,' ***  WARNING  ***  LDEO external header detected
     & - changing to segddin FORMAT LDEO.'
              iwarn = iwarn + 1
              iformat = 6
          ENDIF
c****
c****  Search the External header for NMEA strings
c****  Put the ASCII string in the common buffer so getoke works
c****
          ncbuf = 82  ! NMEA string maximum
          CALL upcase( ldeo_ascii, ntodo )
          DO jjchar = 1, nbytes
             IF( ldeo_ascii(jjchar:jjchar) .EQ. '$' .AND.
     $           ldeo_ascii(jjchar+3:jjchar+5) .EQ. 'GGA' .AND.
     &           jjchar+40 .LT. nbytes ) THEN
                 jchar = 1
                 cbufin(1:82) = ldeo_ascii(jjchar:jjchar+81)
                 IF( IAND(lprint,16) .NE. 0 ) PRINT *,cbufin(1:81)
                 CALL gpggaa( ihourg, iming, isecg, milsg,
     &                       ship_lat, ship_long, istat )
             ENDIF
          ENDDO
          DO jjchar = 1, ntodo
             IF( ldeo_ascii(jjchar:jjchar) .EQ. '$' .AND.
     &           ( ldeo_ascii(jjchar+1:jjchar+3) .EQ. 'DBT' .OR.
     &           ldeo_ascii(jjchar+3:jjchar+5) .EQ. 'DBT' )) THEN
                 jchar = 1
                 cbufin(1:82) = ldeo_ascii(jjchar:jjchar+81)
                 IF( IAND(lprint,16) .NE. 0 ) PRINT *,cbufin(1:81)
                 CALL dbt( wdepth )
             ENDIF
          ENDDO
          IF( IAND(lprint,8) .NE. 0 .AND. iformat .NE. 6 ) THEN
             PRINT *,' ihourg, iming, isecg, milsg, ship_lat, ship_long'
              PRINT *, ihourg, iming, isecg, milsg, ship_lat, ship_long
          ENDIF
          IF( ldeo_ascii(3:3) .EQ. '-' .AND.ldeo_ascii(6:6).EQ.'-') THEN
c****         It's Lee's 2005 external header
              cbufin = ldeo_ascii(1:ntodo)
              jchar = 1
              ncbuf = ntodo
c****         itemp, itemp1, temp are the time of shot 
c****         ihourg, iming, secg are the $GPGGA (time of the fix)
              CALL leeshdr( itemp, itemp1, temp, 
     &            ihourg, iming, secg, ship_lat, ship_long, 
     &            wdepth, rmaggie)
              IF( IAND(lprint,8) .NE. 0 ) THEN
                  PRINT *,  ' SIO nav times: ',
     &                  itemp, itemp1, temp, ihourg, iming, secg
                  PRINT *,' SIO nav: ',ship_lat, ship_long
                  PRINT *,' water depth=', wdepth, ' maggie=',rmaggie
              ENDIF
c****         The time of the fix may not be the time of the shot.
c****         What to do?  Let's use it anyway.  Is the nearest second
c****         good enough?  5 seconds?  10 secs?
          ENDIF
          indx = indx + ntodo / 2
          CALL podiscb( iunit, 1, laddress+nbytes )  ! get past the external header
      ENDIF
      ntotal = 1 + naddblocks + ncsets + nskew + nextend + nexternal
c**** Get the record length from GH#2 if it's FFF.
c**** This one is in mils
      IF( length .EQ. 165 ) THEN
          length = iscr(24) * 256 + IAND(rshift(iscr(25),8),i255)
          nsamps = NINT(FLOAT(length) / (si * 1000.))
      ENDIF
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' ncsets=',ncsets,' nskew=',nskew,' nextend=',nextend,
     &      ' nexternal=',nexternal,' naddblks=',naddblks
          PRINT *,' total general header blocks ',ntotal 
          PRINT *,' SEG-D rev ',rev
          PRINT *,' ntrailer=',ntrailer
      ENDIF
      IF( iformat .EQ. 3 .AND. rev .NE. 0 ) THEN
          PRINT *,' ***  WARNING  ***  Remove parameter format HTI'
      ENDIF
      intrcs = 0
      DO 150 i = fcset, lcset
         index = (naddblocks+i)*16
c****  Bytes 3 & 4 are the channel set start time
         idelay(i) = iscr(index+2) * 2                                  ! a binary number - in 2 mil increments
c****  Bytes 5 & 6 are the channel set end time
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
             temp = REAL(iscr(index+4))
             IF( isign .NE. 0 ) temp = -temp
             descalar(i) = 2. ** (temp * .25 / 256. )
             IF( IAND(lprint,8) .NE. 0 ) PRINT *,' Channel set ',i,
     &          ' has a descalar of ',descalar(i),' MP=',temp
         ENDIF
         n = IAND( rshift(iscr(index+5),12), i15) * 1000 +
     *         IAND( rshift(iscr(index+5),8), i15) * 100 +
     *         IAND( rshift(iscr(index+5),4), i15) * 10 +
     *         IAND(iscr(index+5),i15)
         intrcs = intrcs + n
         delay(i) = REAL(idelay(i))/1000.
  150 CONTINUE
      IF( ltr .GT. intrcs .OR. ltr .EQ. 0 ) ltr = intrcs
      ltr1 = ltr
      IF( iday .lt. fday ) GOTO 1000
      IF( iday .eq. fday .and. igmt .LT. fgmt ) GOTO 1000
      IF( ftr .NE. 99999 ) fftr = ftr
      IF( rlen .GT. 100. ) THEN                                         ! extended record length
          itemp = IAND( rshift(iscr(25),8), i255 )
          rlen = REAL(iscr(24)) * 256. + REAL(itemp)
          rlen = rlen / 1000.                                           ! convert segd mils to secs
      ENDIF
c**** Always write ALL of the SEG-D headers to luntr0
c**** the only thing not in the SEGY trace header is the gun info.
      CALL podiscb( luntr0, 0, 0 )
      nbytes = ntotal * 32
      CALL wrdisc( luntr0, nbytes, 1 )
      CALL wrdiscb( luntr0, scr, nbytes )
c****  LDEO's Syntron system has a "LDEO block" in the external header
      IF( iformat .EQ. 2 .OR. iformat .EQ. 6 ) THEN
c****     The LDEO/Syntrak/Spectra block start with $1
          IF( ldeo_ascii(1:2) .EQ. '$1' ) THEN
              IF( ldeo_ascii(19:19) .EQ. '.' ) THEN
c****   2A=$1, I4=length of header, 4A=program revision, I2=line status,
                  READ( ldeo_ascii,'(2x,I4,4x,2x,3I2,1x,I6,I4,2I2,3x,
     &                  I6,A16,2F11.6,F6.1,2F11.6,2F5.1,F4.1)' )
     &            len_hdr,ldgo_hr, ldgo_min, ldgo_sec, ldgo_mil,
     &            ldgo_yr, month, iday, ldgo_shotno, line_name,
     &            ship_lat, ship_long, wdepth, tail_lat, tail_long,
     &            gyro, cmg, speed
              ELSE
                  ldgo_mil = 0
                  READ( ldeo_ascii,'(2x,I4,4x,2x,3I2,I4,2I2,3x,
     &                  I6,16x,2F11.6,F6.1,2F11.6,2F5.1,F4.1)' )
     &            len_hdr,ldgo_hr, ldgo_min, ldgo_sec,
     &            ldgo_yr, month, iday, ldgo_shotno,
     &            ship_lat, ship_long, wdepth, tail_lat, tail_long,
     &            gyro, cmg, speed
              ENDIF
c              CALL tohex(ldgo_shotno,4,token)
c              print *,' token=',token(1:8)
              ldgo_mil = FLOAT(ldgo_mil) / 1000.
              CALL caljul( month, iday, ldgo_yr, ldgo_day )
              IF( lunldeo .NE. 0 ) THEN
                  CALL podiscb( lunldeo, 0, 0 )
                  CALL wrdiscb( lunldeo, ldeo_ascii, len_hdr )
              ENDIF
          ELSE
c****         The old Digicon Trace 0
              CALL ldgo_tr0( ldeo_ascii, ldgo_shotno, ldgo_yr,
     &             ldgo_day, ldgo_hr, ldgo_min, ldgo_sec, ldgo_mil,
     &             ship_lat, ship_long, wdepth,
     &             tail_lat, tail_long, tail_dist, tail_bear )
              IF( IAND(lprint,256) .NE. 0 ) THEN
                  PRINT *,' ',ldeo_ascii(52:77),' ',ldeo_ascii(8:28),
     &              ' ',ldeo_ascii(158:162),
     &              ' ',ldeo_ascii(1:6),' ',ldeo_ascii(229:314)
                  line_name = ldeo_ascii(140:149)
              ENDIF
c****     make it look like the old Digicon trace 0, which starts with 
c****     24 bytes of the SEG-D record header and followed by a series of
c****     block or sections.   INT*2 word 1 of the section is the section
c****     number and word 2 is the number of bytes in the section (including
c****     the 4 byte section header).
c****     Section 11 was an LDEO ASCII header with 214 bytes 
c****     Section 13 with the Digicourse ASCII birds
c          IF( lunldeo .NE. 0 ) THEN
c              CALL podiscb( lunldeo, 0, 0 )
c              DO i = 1, 6
c                 scr(i) = 0
c              ENDDO
c              CALL wrdiscb( lunldeo, iscr, 24 )
c              iscr(1) = 11
c              iscr(2) = 214
c              CALL wrdiscb( lunldeo, iscr, 4 )
c              CALL wrdiscb( lunldeo, ldeo_dumb, 210 )
c          ENDIF
          ENDIF
c****     Check for consecutive file numbers
          IF( ifilen .NE. lastfilen + 1 .AND. lastfilen .NE. 0 ) THEN
              PRINT 151
              IF( lunlog .NE. 0 ) WRITE (lunlog, 151)
  151   FORMAT(' ***  WARNING  ***   File numbers indicate lost files.')
          ENDIF
c****     Spectra can shoot the line backwards and the shot number decreases, so stop this check
c          IF( ldgo_shotno .NE. lastshot + 1 .AND. lastshot .NE. 0 ) THEN
c              PRINT 152
c              IF( lunlog .NE. 0 ) WRITE (lunlog, 152)
c  152   FORMAT(' ***  WARNING  ***   Shot numbers indicate lost shots.')
c          ENDIF
c          IF( ldgo_shotno .NE. ifilen + idifference ) THEN
c              idifference = ldgo_shotno - ifilen
c              IF( lastshot.NE.0) THEN
c                  PRINT 153
c                  IF( lunlog .NE. 0 ) WRITE (lunlog, 153)
c  153   FORMAT(' ***  WARNING  ***   Shot and file numbers diverge.')
c              ENDIF
c          ENDIF
c****     The LDEO header gets dropped sometimes (on ew0210 it was always
c****     at the start of line), and Joyce wants a non zero number.
c****     Make it consistent with SEGDIN
          IF( ldgo_shotno .LE. lastshot .AND. ldgo_shotno .EQ. 0 .AND.
     &        lastfilen .NE. ifilen ) THEN
              PRINT *,' ***  WARNING  ***  Bad LDGO shot number of ',
     &             ldgo_shotno,' Using ', lastshot + 1
              ldgo_shotno = lastshot + 1
          ENDIF
c****     Check that the multibeam is working and passing depths
          DO i = 1, maxndeps 
             IF( wdepth .NE. H2O_depths(i) ) GOTO 160
          ENDDO
          IF( wdepth .EQ. -1 ) THEN
          PRINT *,' ***  WARNING  ***  Water depth not in SEG-D header.'
              PRINT *,' Is the multi-beam working?'
              PRINT *,' Is the multi-beam passing the water depth?'
              GOTO 160
          ELSE
              PRINT *,
     &' ***  WARNING  *** Water depth has not changed (check multibeam)'
          ENDIF
  160     DO i = 1, maxndeps - 1
             H2O_depths(i) = H2O_depths(i+1)
          ENDDO
          H2O_depths(maxndeps) = wdepth
c**** 
          IF( IAND(lprint,4) .NE. 0 ) THEN
              PRINT *,' Shot number: ', ldgo_shotno,
     &            ' byte index=',index*2
              PRINT *,' Joe date: ',ldgo_yr, ldgo_day,
     &                 ldgo_hr, ldgo_min, ldgo_sec, ldgo_mil
              PRINT *,' ship lat: ',ship_lat, ' ship long: ',ship_long
              PRINT *,' water depth: ',wdepth, 
     &               ' tail pos: ',tail_lat, tail_long
              PRINT *,' tail dist and bearing: ',tail_dist, tail_bear
          ENDIF
          IF( ldgo_yr + ldgo_day .LE. 0 ) PRINT *,
     &             ' ***  WARNING  ***  Bad LDGO header on file ',ifilen
c****     luntr0 is circular.  geom and output need the current tr0
          ldeo_ascii(1:484) = ldeo_ascii(212:695)
c         READ( ldeo_ascii(1:4), '(I4)' ) iscr(2)
c****  ah f!%^&,  READ fails/bombs sioseis if the Digicourse stuff isn't there
          CALL dcode( ldeo_ascii, 4, areal, istat )
          IF( istat .EQ. 2 ) THEN
              iscr(2) = NINT(areal) + 4
          ELSE
              PRINT *,' LDEO "nav" block does not have Digicourse.'
          ENDIF
          IF( lunldeo .NE. 0 ) THEN
              CALL wrdiscb( lunldeo, scr, 4 )
              CALL wrdiscb( lunldeo, ldeo_ascii(5:5), 2028 )
          ENDIF
      ENDIF
c**** seisnet seems to have an extra 136 blocks (4352 bytes) in the gen header
c****   On 2 channel sets there's 139 blocks (4448 bytes)
c      IF( iformat .EQ. 2 ) THEN
c****     4352 is also the value of index - the 16 bit word index or
c     index = (1+naddblocks+ncsets+nskew+nextend) * 32 / 2
c          IF( ncsets .EQ. 1 ) CALL podiscb( iunit, 2, 4352 )
c          IF( ncsets .EQ. 2 ) CALL podiscb( iunit, 2, 4448 )
c**** Seisnet has a bunch of stuff now - like a list of disk
c**** addresses and trace data length and other stuff.  So,
c**** skip to trace 1, BUT add 512 for some reason - it's like
c**** every write they do has an extra 512 bytes.  curious.
      IF( iformat .EQ. 2 ) THEN
          CALL rddiscb( iunit, scr, 4, istat )
          CALL swap32( lscr, 1 )
          CALL podiscb( iunit, 0, lscr(1) + 512 )
      ENDIF
      getnewshot = 0
c****
c****   Find the proper demultiplexed trace - Do the DEMUX header
c****
  200 CONTINUE
      IF( getnewshot .EQ. 1 ) GOTO 100
      CALL rddiscb( iunit, scr, 20, istat )
      IF( IAND(lprint,16) .NE. 0 ) THEN
          CALL tohex( scr, 20, token )
          PRINT *,' SEG-D trace header =',token(1:40)
      ENDIF
      IF( istat .NE. 20 ) GOTO 1000
      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap16( iscr, 10 )
      nmore = IAND(iscr(5),i255) * 32    !  number of extra trace header extension blocks
      IF( nmore .NE. 0 .AND. IAND(lprint,16) .NE. 0 ) 
     &    PRINT *,' Number of trace header extensions =',nmore
      IF( nmore .GT. 0 ) CALL rddiscb( iunit, scr(11), nmore, istat )
      nbytes = nsamps * 4
      IF( ifmt .EQ. 8036 ) nbytes = nsamps * 3
      CALL rddiscb( iunit, buf(numhdr+1), nbytes, istat )   !   This is the binary trace!
      IF( istat .NE. nbytes ) GOTO 990
      IF( iformat .EQ. 2 ) CALL podiscb(iunit, 2, 512 )
c**** The file number in the trace header might be bogus because it's only 4 BCD
c**** long and the real file number might be in the extended general header
      icsn = IAND( rshift(iscr(2),4), i15) * 10 + IAND(iscr(2),i15)         ! channel set number
c****  Remember this is packed BCD, not HEX. FF in BCD is 165 (150 + 15)
      IF( icsn .EQ. 165 ) THEN                       
          itemp = IAND( rshift(iscr(9),8), i255)
          icsn = IAND( iscr(8),i255 ) * 256 + itemp
      ENDIF
      IF( icsn .NE. lastcsn .AND. ftr .NE. 99999 ) fftr = ftr
      lastcsn = icsn
      itrcno = IAND( rshift(iscr(3),12), i15) * 1000 +                    ! trace number
     *         IAND( rshift(iscr(3),8), i15) * 100 +
     *         IAND( rshift(iscr(3),4), i15) * 10 +
     *         IAND(iscr(3),i15)  
      IF( icsn .EQ. 1 .AND. itrcno .EQ. 1 ) trcount = 0
      trcount = trcount + 1
      IF( IAND(lprint,2) .NE. 0 ) 
     *     PRINT *,' ifilen=',ifilen,' trace=',itrcno,' fftr=',fftr,
     *       ' ltr=',ltr,' cn=',icsn,' igmt=',igmt,' ffilen=',ffilen,
     *       ' trcount=',trcount
cccccc      IF( itrcno .GE. ltr ) istop = 1
      IF( ffilen .NE. 99999 .AND. ifilen .LT. ffilen ) GOTO 1000        ! find the first shot
      IF( icsn .GT. lcset ) GOTO 1000                                   ! get a new shot
      IF( icsn .LT. fcset ) GOTO 200                                    ! get another trace
      IF( filinc .EQ. 99999 ) ffilen = 99999                            ! got ffilen, so change it to accept all files
cccccc      IF( itrcno .NE. fftr .AND. ltr .NE. 0 ) GOTO 200                   ! get the right trace
c**** If ftr is not given, then take all traces in all channel sets.
      IF( ftr .NE. 99999 ) THEN
c****     This statement allows all traces numbered between ftr and ltr
          IF( itrcno .LT. fftr .OR. (itrcno .GT. ltr .AND. ltr .NE. 0 ))
     &        GOTO 200
      ENDIF
      itword = iscr(4)                                                  ! time word
      itemp = IAND( rshift(iscr(5),8), i255)
      tword = itword + FLOAT(itemp)/256.
      ithext = IAND(iscr(5),i255)
      itemp = IAND( rshift(iscr(6),8), i255 )                             ! sample skew 
      skew = FLOAT(itemp) / 256.
      itimeb = iscr(7)                                                  ! time break
      itemp = IAND( rshift(iscr(8),8), i255)
      tbreak = itimeb + FLOAT(itemp)/256.
c****
c****    GOT A TRACE TO KEEP!   unpack the data and create an SEGY trace header
c**** seisnet data is swapped
      IF( (icompt .EQ. 2 .OR. icompt .EQ. 4) .AND. iformat .NE. 2 ) 
     &    CALL swap32( buf(numhdr+1), nsamps ) 
      IF( icompt .NE. 2 .AND. icompt .NE. 4 .AND. iformat .EQ. 2 )
     &    CALL swap32( buf(numhdr+1), nsamps ) 
c  8015 = 20 bit SEGD floating point (4 bit hex exponent)
c  8022 = 8 bit integer
c  8036 = 24 bit integer (new in rev 2)
c  8038 = 32 bit integer
c  8048 = 32 bit IBM FP (not in rev 2)
c  8058 = 32 bit IEEE FP
      IF( ifmt .EQ. 8015 ) THEN
          PRINT *,' ****   SEG-D iformat 8015 failure'
          STOP
c****     the problem is how many bytes are in a trace
c          CALL segd20
      ENDIF
      IF( ifmt .EQ. 8036 ) THEN
          CALL i24i32( buf(numhdr+1), lscr, nsamps )
          DO i = 1, nsamps
             buf(numhdr+i) = FLOAT( lscr(i) )
          ENDDO
      ENDIF
      IF( ifmt .EQ. 8038 ) THEN
          DO i = 1, nsamps
             buf(numhdr+i) = FLOAT(lbuf(numhdr+i))
          ENDDO
      ENDIF
      IF( ifmt .EQ. 8048 ) 
     &    CALL ibm2fp( buf(numhdr+1),nsamps,buf(numhdr+1))
      numdat = nsamps
      IF( stime .GT. 0. .AND. stime .GT. delay(icsn) ) THEN             ! should we get rid of the front of the data trace?
          n = NINT( (stime - delay(icsn)) / si )
          numdat = numdat - n
          DO 400 i = 1, numdat
             buf(numhdr+i) = buf(numhdr+n+i)                            ! move the data so stime is the first data sample
  400     CONTINUE
c          delay(icsn) = stime
c          idelay(icsn) = stime * 1000.
      ENDIF
      IF( decimf .GT. 1 ) THEN
          j = 1
          DO 450 i = 1, numdat, decimf
             buf(numhdr+j) = buf(numhdr+i)
             j = j + 1
  450     CONTINUE
          numdat = numdat / decimf 
c****     Don't change si for decimf because it's set only once per shot
      ENDIF 
      IF( secs .NE. 0 ) THEN
c****     numdat has been reduced by stime and decimf
          itemp = NINT(secs/(si*decimf))
          numdat = MIN0(numdat,itemp)
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
      IF( ldgo_shotno .NE. 0 ) lbuf(3) = ldgo_shotno
      IF( stack .NE. 0 .AND. shot_inc .NE. 0 ) THEN
          itemp = MOD(lbuf(3),2)
c****     1 means odd, 2 means even
          IF( (shot_inc .EQ. 1 .AND. itemp .EQ. 0) .OR.
     &        (shot_inc .EQ. 2 .AND. itemp .EQ. 1) ) THEN
              getnewshot = 1
              lastfilen = ifilen
              lastshot = lbuf(3)
c****         don't do the log or renum or retrac - we're not using this shot
              RETURN
          ENDIF
      ENDIF
      IF( renum .GT. 0 ) lbuf(3) = renum
      IF( retrac .LT. 0 ) lbuf(4) = trcount
      IF( retrac .EQ. 0 ) lbuf(4) = itrcno
      IF( retrac .GT. 0 ) THEN
c****     renumber the traces from RETRAC if it's a new shot or if
c****     the trace count is the number of input traces per shot
c****     (needed in case the shot numbers don't increment
          IF( lbuf(3) .NE. lastshot )
     &        otrcno = retrac
          lbuf(4) = otrcno
          otrcno = otrcno + 1
      ENDIF
      lbuf(5) = ifilen
      lastfilen = lbuf(5)
      ibuf(15) = 1
      buf(46) = AMAX1(delay(icsn),stime)
      ibuf(55) = NINT(buf(46) * 1000.)
      ibuf(58) = numdat
      ibuf(59) = micros * decimf
      ibuf(79) = iyear
      ibuf(80) = iday
      ibuf(81) = ihour
      ibuf(82) = imin
      ibuf(83) = isec
      ibuf(84) = 1
      IF( milsg .NE. 9999 ) ibuf(84) = milsg
      buf(49) = si * decimf
c****  Put the ship position in the group position because
c****  process geom uses 19 & 20 for the x/y coordinate and geom is
c****  needed to get the steamer depth.   Remember that x is longitude
c****  and y is latitude.
      IF( ship_lat + ship_long .NE. 0 ) THEN
          ibuf(36) = -100
c****     arcsec = 60sec/min * 60min/deg = 3600. sec/deg
          lbuf(19) = NINT(ship_long*3600.*100.)
          lbuf(20) = NINT(ship_lat*3600.*100.)
          lbuf(21) = NINT(ship_long*3600.*100.)
          lbuf(22) = NINT(ship_lat*3600.*100.)
          ibuf(45) = 2
      ENDIF
      lbuf(16) = wdepth
c      buf(50) = wdepth / 750.
c      buf(54) = wdepth
      IF( ntrgat .GT. 0 ) THEN
          mtrgat = mtrgat + 1
          IF( mtrgat .EQ. ntrgat ) THEN
              lbuf(51) = -1
              mtrgat = 0
          ENDIF
          lbuf(6) = lbuf(3)
      ENDIF
      IF( ldgo_shotno .NE. 0 ) THEN
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
          ibuf(79) = ldgo_yr
          ibuf(80) = ldgo_day
          ibuf(81) = ldgo_hr
          ibuf(82) = ldgo_min
          ibuf(83) = ldgo_sec
c          ibuf(84) = NINT( (ibuf(83) - sec_ldgo ) * 1000.)
          ibuf(84) = ldgo_mil
      ENDIF
      IF( lunlog .NE. 0 .AND. lbuf(4) .EQ. 1 ) THEN
          lat = ship_lat
          long = ship_long
          WRITE(lunlog,170) lbuf(5),lbuf(3),ibuf(80),ibuf(81),ibuf(82),
     &          ibuf(83), ibuf(84),
     &          lat, (ABS(ship_lat)-IABS(lat)) * 60.,
     &          long, (ABS(ship_long)-IABS(long)) * 60., wdepth
  170           FORMAT (' file ',I6,' shot ',I6,' day ',I3,1X,2I2,'z ',
     &      I3,'.',I3,' lat/long ',I3,1x,F7.4,1X,I4,1X,F7.4,' WD ',F7.1)
           FLUSH( lunlog )
      ENDIF
      lastshot = lbuf(3)
      IF( IAND(lprint,4) .NE. 0 ) THEN
          PRINT *,' SEGY header:'
          PRINT *,lbuf(3),lbuf(4),ibuf(15),ibuf(55),ibuf(58),ibuf(59)
          PRINT *,lbuf(16),ibuf(36),lbuf(19),lbuf(20)
          PRINT *,(ibuf(i),i=79,84),buf(46),buf(49),lbuf(51)
      ENDIF
      IF( renum .GT. 0 ) lbuf(3) = renum
c**** renum won't work if multiple channel sets. (intrcs is set to the sum
c**** of all the traces whereas itrcno is the trace number within each channel set.
      IF( renum .GT. 0 .AND. itrcno .GE. intrcs ) renum = renum + 1
      in = 0                                                            ! signal that the data is not in the ap
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
c****  Ah shoot, when doing multiple channel sets, itrcno is the
c****  trace number in the channel set, but ltr is counting the
c****  total number of traces in the file.  Let's assume that
c****  when that happens, retrac was used and the segy trace number is
c****  the count of the output traces (assumes ftr was not given).
c	print *,' ftr=',ftr,' ltr=',ltr,' intrcs=',intrcs,' lb=',lbuf(4)
c	print *,' ifilen=',ifilen,' lfilen=',lfilen,' list=',list
c	print *,' istop=',istop,' stack=',stack
      IF( ftr .EQ. 99999 .AND. ltr .EQ. intrcs .AND. 
     &    lbuf(4) .EQ. ltr ) THEN
          IF( ifilen .EQ. lfilen ) THEN
              istop = 1
              RETURN
          ENDIF
          IF( iformat .EQ. 2 .AND. list+stack+ldeolist .EQ. 0 ) THEN    ! reading the file directly
c****         read a bunch because seisnet has headers and trailers
              CALL rddiscb( iunit, scr, 5000, istat )
              CALL podiscb( iunit, 2, -5000 )
              IF( istat .NE. 100 ) THEN
                  IF( mlists .EQ. nlists ) THEN
                      istop = 1
                  ELSE
                      getlist = .TRUE.
                  ENDIF
              ENDIF
              RETURN
          ENDIF
          getnewshot = 1
      ENDIF
c****  make it work for now - sleepie
      IF( ftr .NE. 99999 .AND. ltr .EQ. intrcs .AND.
     &    lbuf(4) .EQ. ltr ) THEN
          IF( list+stack .NE. 0 ) getnewshot = 1
      ENDIF
      RETURN
c****
c****    Come here when the shot is truncated.
c****    Don't abort on truncated shots, move on to the next shot.
c****
  980 CONTINUE
      PRINT *,' ***  WARNING  ***   Bad SEG-D general header.'
      PRINT *,'       Skipping to next shot.'
      GOTO 1000
  990 CONTINUE
c**** seisnet has a trailer, so a short trace is really just the trailer.
      IF( iformat .EQ. 2 ) GOTO 1000
      PRINT *,
     &' ***  WARNING  ***   Bad (short) trace - skipping to next shot .'
      PRINT *,'   Check file ',ifilen,' shot ',lbuf(3),' trace ',lbuf(4)
      PRINT *,'   Wanted ',nbytes,' read ',istat,' bytes.'
      GOTO 1000
c****
c****    No trace ready, get another shot
c****
 1000 CONTINUE
      IF( list .GT. 0 ) THEN
          CALL frefil( 2, iunit )
          CALL rline1(list)
          CALL getoke1( token, nchars )
          IF( nchars .EQ. 0 .AND. list .NE. 0 ) THEN
              istop = -1
              RETURN
          ENDIF
          IF( nchars .EQ. 0 ) THEN
              istop = 1
              RETURN
          ENDIF
          CALL getfil( 4, iunit, token, istat )
          IF( istat .NE. 0 ) THEN
              PRINT *,' ***  ERROR  ***  Could not open file ', token
              istop = -1
              RETURN
          ENDIF
          GOTO 100
      ENDIF
c****  Some compilers (Fedora and intel) don't allow GOTO into the middle
c****  of an if/then block, so set a flag and make 1010 a block of it's own.
      iflag = 0
c**** There all sorts of timing issues with the "current" shot, so
c**** let's always use the second from current
c**** I'm tempted to assume the inode/address of the file holding the
c**** pathname of the latest shot remains the same, it might not, so
c**** close the file after reading it.
      IF( stack+ldeolist .GT. 0 ) THEN
c****     Ethan wants stdout flushed
          CALL fdsync( 1 )
          CALL frefil( 2, iunit )
          iflag = 1
      ENDIF
      IF( iflag .EQ. 0 ) GOTO 1040
 1010     CONTINUE
c****     Use the SIOSEIS negative number in file IN/in convention of tapes.
          OPEN(UNIT=lunin,FILE='in',STATUS='UNKNOWN',IOSTAT=istat)
          READ(lunin,'(I4)',END=1020,ERR=1020)i
          CLOSE(UNIT=lunin,STATUS='DELETE')
 1015     IF( i .GE. 0 ) GOTO 1030
          IF( ldeolist .GT. 0 ) CLOSE(UNIT=ldeolist,STATUS='DELETE')
          IF( stack .GT. 0 ) THEN
              istop_stack = 1
          ELSE
              PRINT *,' User stop with file "in".'
              istop = -1
              RETURN
          ENDIF
 1020     CLOSE(UNIT=lunin,STATUS='DELETE')
          OPEN(UNIT=lunin,FILE='IN',STATUS='UNKNOWN',IOSTAT=istat)
          READ(lunin,'(I4)',END=1030,ERR=1030)i
          CLOSE(UNIT=lunin,STATUS='DELETE')
          IF( i .LT. 0 ) GOTO 1015
 1030     CONTINUE
c****  Need the next DELETE incase an empty or bad file IN exists
          CLOSE(UNIT=lunin,STATUS='DELETE')
c      ENDIF
 1040 CONTINUE
      IF( stack .GT. 0 ) THEN
c****     the stack loop never times out because shooting may be temporarily suspended
c****     because of mammals.  We don't want to have to restart sioseis everytime that happens.
c****     A bit of Marx brothers - Who is on first and What is on second.
          OPEN( UNIT=stack, FILE=cpath,
     &          FORM='FORMATTED', STATUS='OLD')
          CALL rline1(stack)   ! read first line in file stack
          CALL getoke1( top1, n1chars )
c****     damn, the inode may exist, but the name may not - slow down!
          IF( top1(1:1) .EQ. ' ' ) THEN
              CLOSE (UNIT=stack)
              PRINT *,' waiting for a pathname in stack file ',cpath
              CALL sleep(1)
              GOTO 1010
          ENDIF
          CALL rline1(stack)   ! read second line in file stack
          CLOSE (UNIT=stack)   ! we're finished with file stack
c****     Another gotcha.  If we're always taking the second to last,
c****     we never process the last shot!
          CALL getoke1( top2, n2chars )
          IF( top2(1:1) .EQ. ' ' ) THEN
              CALL sleep(1)
              GOTO 1010
          ENDIF
          IF( nchars4 .LT. 0 ) THEN    ! nchars4 is the number of characters rline1 read
              PRINT *,' Stopping due to nothing in the segddin stack.'
              istop = -1
              RETURN
          ENDIF
          IF( istop_stack .GT. 0 ) THEN    ! finish up cleanly
              IF( top1 .EQ. filename ) THEN
                  PRINT *,' Normal stop of segddin stack.'
                  istop = -1
                  RETURN
              ENDIF
c****     it's unclear if the second from top has been used
c****     filename is the name of the last file processed
              IF( top2 .EQ. filename ) THEN
                  filename = top1
                  nchars = n1chars
              ELSE
                  filename = top2
                  nchars = n2chars
              ENDIF
          ELSE
              filename = top2
              nchars = n2chars
          ENDIF
c****     crap.  There might be other crap in the seisnet directory
          IF( filename(nchars-3:nchars) .NE. '.sgd' .AND.
     &        filename(nchars-4:nchars) .NE. '.SEIS' ) THEN
              CALL sleep(1)
              GOTO 1010
          ENDIF
          CALL getfil( 4, iunit, filename, istat )
          IF( istat .NE. 0 ) THEN
              PRINT *,' ***  ERROR  ***  Could not open file ',filename
              CALL sleep(1)
              GOTO 1010
          ENDIF
          IF( IAND(lprint,8) .NE. 0 ) 
     &        PRINT *,' opened file: ',filename(1:nchars)
          GOTO 100
      ENDIF
      IF( ldeolist .GT. 0 ) THEN
          IF( ldeoopen ) THEN
              CALL rline1(ldeolist)
              IF( nchars4 .GT. 0 ) GOTO 1802
              PRINT *,' Read end of ldeolist, deleting the list.'
              CLOSE(UNIT=ldeolist,STATUS='DELETE')
              ldeoopen = .FALSE.
          ENDIF
 1800     PRINT *,' Waiting for LDEOLIST.'
          CALL SLEEP(10)
          INQUIRE( FILE=cpath, EXIST=iexist )
c****    IF( NOT(iexist) ) never turns false in Sun Solaris!
          IF( iexist ) GOTO 1801
          GOTO 1010
 1801     OPEN( UNIT=ldeolist, FILE=cpath,
     &          FORM='FORMATTED', STATUS='OLD')
          CALL rline1(ldeolist)
 1802     CALL getoke1( token, nchars )
          CALL getfil( 4, iunit, token, istat )
          IF( istat .LT. 0 ) THEN
              PRINT *,' ***  ERROR  ***  Check LDEOLIST.'
          ELSE
              ldeoopen = .TRUE.
c              PRINT *,' Getting file: ',token
          ENDIF
          GOTO 100
      ENDIF
      IF( iformat .EQ. 4 .OR. iformat .EQ. 5 ) THEN
          CALL frefil( 2, iunit )
          IF( ifilen .EQ. lfilen ) GOTO 2000
          itemp = ifilen + 1
          CALL bldgname( cpath, itemp, token )
          CALL getfil( 4, iunit, token, istat )
          IF( istat .LT. 0 ) THEN
              istop = -1
              RETURN
          ENDIF
          GOTO 100
      ENDIF
c****
c****   No traces left, no trace in buf,  STOP now!
c****
 2000 CONTINUE
      istop = -1
      RETURN
      END
