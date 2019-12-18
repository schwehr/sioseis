      SUBROUTINE seg2ex( buf, lbuf, ibuf, scr, lscr, iscr, istop )
c
c  Written by Paul Henkart, Scripps Institution of Oceanography, August 1998
c  Copyright (C) 1998 The Regents of the University of California
c  ALL RIGHTS RESERVED.
c
c mod 17 Nov. 98 - Make the segy header entry trace id = 1
c mod 26 Apr 01,  If byte swapping, do the data format code also
c mod 29 Sep 08 - FFILEN no longer worked (I4 now gives leading blanks)
c mod 20 Mar 09 - Allow filen > 9999  (use I5)
c
      PARAMETER (MAX_NTRCS = 200 )
      COMMON /readt/ ilun, numhdr, nsamps, ihunit, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr,
     *               nrskip, nfiles
      COMMON /sioap/ iasgnd, irelse, in, iout, nextad, lapsiz, ifree,
     *     iuseap
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr, lsmusptr, lemusptr,
     *                 lsisptr,  lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr,   ifoldptr, icvleptr, lespnptr
      COMMON /sioln1/cbuf
      CHARACTER*200 cbuf
      COMMON /sioln2/ ichar1, nchars, iprint, lunpo
      COMMON /seg2in/ ipunit, nlists, npwrds, multi, iswap
      DIMENSION buf(111), lbuf(111), ibuf(111),
     &          scr(111), lscr(111), iscr(111)
      INTEGER*2 ibuf, iscr
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      INTEGER lprint, ftr, ltr, ffilen, lfilen
      DIMENSION indices(MAX_NTRCS)
      CHARACTER*100 token, path, raw_record
      CHARACTER*3 month, months(12)
      SAVE
      DATA mlists/0/, months/'JAN','FEB','MAR','APR','MAY','JUN','JUL',
     &     'AUG','SEP','OCT','NOV','DEC'/

      istop = 0
      in = 0                                                            ! the data will not be in the ap
      IF( mlists .EQ. 0 ) THEN
          CALL podisc( ipunit, 1, 0 )
          CALL rddisc( ipunit, scr, npwrds, istat  )
          IF( istat .NE. npwrds ) THEN
              PRINT *,' ***  ERROR  ***  Program error in seg2ex.',
     *           npwrds, istat
             STOP
         ENDIF
         CALL rddiscb( ipunit, path, 100, istat )
         mlists = mlists + 1
         lprint = lscr(1)
         ffilen = lscr(2)
         lfilen = lscr(3)
         ftr = lscr(4)
         ltr = lscr(5)
         IF( jntrcs .GT. MAX_NTRCS ) THEN
             PRINT *,'  ***  ERROR  ***  Too many traces for SEG2IN.'
             PRINT *,' Increase MAX_NTRCS in seg2ex.f and recompile.'
             STOP
         ENDIF
         IF( ftr .EQ. -12345 ) ftr = 1
         IF( ltr .EQ. 0 ) ltr = jntrcs
         ntrcs = ltr - ftr + 1
         itrace = 0
         ifilen = ffilen
      ENDIF
c**** the file is on ilun and was opened previously.  Get the trace
c**** indices on the first trace of the file.
      IF( itrace .EQ. 0 ) THEN
          CALL podiscb( ilun, 1, 32 )                                   ! position to byte 32
          CALL rddisc( ilun, indices, jntrcs, istat )
          IF( iswap .NE. 0 ) CALL swap32( indices, jntrcs )
          itrace = ftr
c****     Read and decode the file header
          cbuf(1:50) = ' '
          nchars = MIN0(indices(1)-(32+jntrcs*4),200)
          itemp = indices(1)-(32+jntrcs*4)
   90     nchars =  MIN0(itemp,150)
          CALL rddiscb( ilun, cbuf(51:51), nchars, istat )
          nchars = istat + 50
          ichar1 = 1
  100     CONTINUE
          CALL getoke( token, nchars1 )
          IF( IAND(lprint,4) .NE. 0 ) PRINT *,token(1:nchars1)
          IF( token(1:nchars1) .EQ. 'ACQUISITION_DATE' ) THEN
              IF( ichar1 .GT. 180 ) GOTO 220
              CALL getoke( token, nchars1 )
              IF( IAND(lprint,4) .NE. 0 ) PRINT *,token(1:nchars1)
              n = 1
c****         Is the day of month 1 or 2 characters?
              IF( token(3:3) .EQ. '/' ) n = 2
              token(n+1:n+1) = ' '
              CALL dcode( token, n, areal, istat )
              iday = NINT(areal)
              month = token(n+2:n+4)
              CALL upcase( month, 3 )
              DO imonth = 1, 12
                 IF( month .EQ. months(imonth) ) GOTO 200
              ENDDO
  200         CALL dcode( token(n+6:n+6), 4, areal, istat )
              iyear = NINT(areal)
              CALL caljul( imonth, iday, iyear, jday )
          ENDIF
          IF( token(1:nchars1) .EQ. 'ACQUISITION_TIME' ) THEN
              IF( ichar1 .GT. 180 ) GOTO 220
              CALL getoke( token, nchars1 )
              IF( nchars1 .NE. 8 ) THEN
                  token(2:nchars1+1) = token(1:nchars1)
                  token(1:1) = ' '
                  nchars1 = nchars1 + 1
              ENDIF
              IF( IAND(lprint,4) .NE. 0 ) PRINT *,token(1:nchars1)
              READ( token, '(I2,1x,I2,1x,I2)' ) ihour, imin, isec
          ENDIF
          IF( ichar1 .LT. 200 ) GOTO 100
  220     cbuf(1:50) = cbuf(151:200)
          itemp = itemp - 150
          IF( itemp .GT. 10 ) GOTO 90
      ENDIF
	 
c**** Ah shit.  There's a data file with a dropped byte.  The file is the
c**** correct length, but the trace descriptor is off 1 byte.  Let's do
c**** 1 retry before aborting.
      nerrs = 0
  400 CALL podiscb( ilun, 1, indices(itrace) )
      CALL rddiscb( ilun, scr, 32, istat )
      IF( iswap .NE. 0 ) THEN
          CALL swap16( iscr, 2 )
          CALL swap32( lscr(2), 2 )
          CALL swap16( iscr(7), 1 )
      ENDIF
      IF( iscr(1) .NE. 17442 ) THEN                                     ! 4422 hex
          IF( nerrs .GT. 20 ) THEN
              PRINT *,' ***  ERROR  ***  SEG2 trace descriptor error.'
              STOP
          ELSE
              nerrs = nerrs + 1
              indices(itrace) = indices(itrace) - 1
              PRINT *,' ***  WARNING  ***  Trace descriptor off ',nerrs,
     &                'bytes.'
              GOTO 400
          ENDIF
      ENDIF
      nbytes_blk = iscr(2)
      nbytes_data = lscr(2) - nerrs
      nsamps = lscr(3) - (nerrs+4)/4
      nhdr = nbytes_blk - 32
      cbuf(1:50) = ' '
      ncdone = 0
      DO j = 1, 5
         CALL rddiscb( ilun, cbuf(51:51), 150, istat )
         nchars = istat + 50
         ichar1 = 1
         IF( ncdone+150 .GT. nhdr ) THEN
             nchars = nhdr - ncdone + 50
             IF( nchars .LE. 0 ) GOTO 1010
         ENDIF
         ncdone = ncdone + nchars - 50
 1000    CONTINUE
            i = ICHAR(cbuf(ichar1:ichar1))
            IF( ICHAR(cbuf(ichar1:ichar1)) .GT. 122 ) GO TO 1010
            IF( ICHAR(cbuf(ichar1:ichar1)) .LT. 32 ) GOTO 1005
            CALL getoke( token, nchars1 )
            IF( IAND(lprint,4) .NE. 0 ) PRINT *,token(1:nchars1)
c****       getoke uses ! as the start of a comment and returns nchars=0
            IF( nchars1 .EQ. 0 .AND. ichar1 .LT. 200 ) THEN
                ichar1 = ichar1 + 1
                GOTO 1000
            ENDIF
            IF( token(1:nchars1) .EQ. 'SHOT_SEQUENCE_NUMBER' ) THEN
                CALL getoke( token, nchars1 )
                IF( IAND(lprint,4) .NE. 0 ) PRINT *,token(1:nchars1)
                CALL upcase( token, nchars1 )
                CALL dcode( token, nchars1, shotno, istat )
            ENDIF
            IF( token(1:nchars1) .EQ. 'DELAY' ) THEN
                CALL getoke( token, nchars1 )
                IF( IAND(lprint,4) .NE. 0 ) PRINT *,token(1:nchars1)
                CALL upcase( token, nchars1 )
                CALL dcode( token, nchars1, delay, istat )
            ENDIF
            IF( token(1:nchars1) .EQ. 'SKEW' ) THEN
                CALL getoke( token, nchars1 )
                IF( IAND(lprint,4) .NE. 0 ) PRINT *,token(1:nchars1)
                CALL upcase( token, nchars1 )
                CALL dcode( token, nchars1, skew, istat )
            ENDIF
            IF( token(1:nchars1) .EQ. 'RECEIVER_LOCATION' ) THEN
                CALL getoke( token, nchars1 )
                IF( IAND(lprint,4) .NE. 0 ) PRINT *,token(1:nchars1)
                CALL upcase( token, nchars1 )
                CALL dcode( token, nchars1, receiver, istat )
            ENDIF
            IF( token(1:nchars1) .EQ. 'SOURCE_LOCATION' ) THEN
                CALL getoke( token, nchars1 )
                IF( IAND(lprint,4) .NE. 0 ) PRINT *,token(1:nchars1)
                CALL upcase( token, nchars1 )
                CALL dcode( token, nchars1, source, istat )
            ENDIF
            IF( token(1:nchars1) .EQ. 'CHANNEL_NUMBER' ) THEN
                CALL getoke( token, nchars1 )
                IF( IAND(lprint,4) .NE. 0 ) PRINT *,token(1:nchars1)
                CALL upcase( token, nchars1 )
                CALL dcode( token, nchars1, channel, istat )
            ENDIF
            IF( token(1:nchars1) .EQ. 'DESCALING_FACTOR' ) THEN
                CALL getoke( token, nchars1 )
                IF( IAND(lprint,4) .NE. 0 ) PRINT *,token(1:nchars1)
                CALL upcase( token, nchars1 )
                CALL dcode( token, nchars1, descale, istat )
            ENDIF
            IF( token(1:nchars1) .EQ. 'STACK' ) THEN
                CALL getoke( token, nchars1 )
                IF( IAND(lprint,4) .NE. 0 ) PRINT *,token(1:nchars1)
                CALL upcase( token, nchars1 )
                CALL dcode( token, nchars1, stack, istat )
            ENDIF
            IF( token(1:nchars1) .EQ. 'SAMPLE_INTERVAL' ) THEN
                CALL getoke( token, nchars1 )
                IF( IAND(lprint,4) .NE. 0 ) PRINT *,token(1:nchars1)
                CALL upcase( token, nchars1 )
                CALL dcode( token, nchars1, si, istat )
            ENDIF
            IF( token(1:nchars1) .EQ. 'FIXED_GAIN' ) THEN
                CALL getoke( token, nchars1 )
                IF( IAND(lprint,4) .NE. 0 ) PRINT *,token(1:nchars1)
                CALL upcase( token, nchars1 )
                CALL dcode( token, nchars1, fixed_gain, istat )
            ENDIF
            IF( token(1:nchars1) .EQ. 'RAW_RECORD' ) THEN
                CALL getoke( raw_record, nchars1 )
                IF( IAND(lprint,4) .NE. 0 ) PRINT *,raw_record
                DO i = 1, nchars1
                   IF( ICHAR(raw_record(i:i)) .LT. 48 .OR. 
     &                 ICHAR(raw_record(i:i)) .GT. 57 ) THEN
                       nchars1 = i-1
                       GOTO 1004
                   ENDIF
                ENDDO
 1004           filen = 0.
                IF( nchars1 .GT. 0 )
     &              CALL dcode( raw_record, nchars1, filen, istat )
            ENDIF
 1005       ichar1 = ichar1 + 1
            IF( ichar1 .LT. 200 ) GOTO 1000
            cbuf(1:50) = cbuf(151:200)
      ENDDO
 1010 IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' shotno=',shotno,' delay=',delay,' si=',si
          PRINT *,' skew=',skew,' receiver=',receiver,' source=',source
          PRINT *,' channel=',channel,' stack=',stack,' descal=',descale
          PRINT *,' ifmt=',ifmt,' nsamps=',nsamps
      ENDIF
      nwrds = nsamps
      IF( ifmt .EQ. 1 ) nwrds = nsamps / 2
      IF( ifmt .EQ. 3 ) nwrds = nsamps * 20 / 32
      CALL podiscb( ilun, 1, indices(itrace)+nbytes_blk )
      CALL rddisc( ilun, scr, nwrds, istat )
      IF( istat .NE. nwrds ) THEN
          PRINT *,' ***  ERROR  ***  Bad read on trace ',itrace
          PRINT *,' read ',istat,' samples, wanted ',nwrds
          nwrds = istat
          nsamps = istat
          STOP
      ENDIF
      IF( ifmt .EQ. 1 ) THEN
          IF( iswap .NE. 0 ) CALL swap16( iscr, nsamps )
          DO i = 1, nsamps
             buf(numhdr+i) = REAL(iscr(i))
          ENDDO
      ENDIF
      IF( ifmt .EQ. 2 ) THEN
          IF( iswap .NE. 0 ) CALL swap32( lscr, nsamps )
          DO i = 1, nsamps
             buf(numhdr+i) = FLOAT(lscr(i))
          ENDDO
      ENDIF
      IF( ifmt .EQ. 3 ) THEN
          IF( iswap .NE. 0 ) THEN
              PRINT *,' ** ERROR ** Can not swap bytes on 20 bit data.'
              STOP
          ENDIF
          CALL segd20( iscr, buf(numhdr+1), nsamps )
      ENDIF
      IF( ifmt .EQ. 4 ) THEN
          IF( iswap .NE. 0 ) CALL swap32( lscr, nsamps )
          DO i = 1, nsamps
             buf(numhdr+i) = scr(i)
          ENDDO
      ENDIF
      DO i = 1, nsamps
         buf(numhdr+i) = buf(numhdr+i) * descale / stack
      ENDDO
c****
c****  Create the SEG-Y trace header
c****
      DO i = 1, numhdr
         lbuf(i) = 0
      ENDDO
      delay = delay + skew
c**** geez, try and figure out what the shot number is.  When IPATH is
c**** given and the SHOT_SEQUENCE is not in the data (e.g. Geometrics),
c**** then assume it is the Geometrics and the shot number is RAW_RECORD
      IF( ifilen .NE. 99999 ) THEN
          lbuf(lshotptr) = ifilen
      ELSE
          lbuf(lshotptr) = NINT(filen)
      ENDIF
      IF( shotno .GT. 0. ) lbuf(lshotptr) = NINT(shotno)
      lbuf(lshtrptr) = itrace
      ibuf(itridptr) = 1
      ibuf(idelmptr) = NINT(delay*1000.)
      buf(ldelsptr) = delay
      ibuf(isampptr) = nsamps
      ibuf(isiptr) = NINT(si*1000000.)
      buf(lsisptr) = si
      ibuf(iyrptr) = iyear
      ibuf(idayptr) = jday
      ibuf(ihrptr) = ihour
      ibuf(iminptr) = imin
      ibuf(isecptr) = isec
c****
c**** Now set up for the next trace or the end of job!
c****
      itrace = itrace + 1
      IF( itrace .LE. ltr ) RETURN
      itrace = 0
      CALL frefil( -2, ilun, istat )
      IF( path .EQ. ' ' ) THEN
          ifilen = ifilen + 1
          IF( ifilen .GT. lfilen ) GOTO 4000
          WRITE( token, 3000 ) ifilen
c****     strip leading blanks
          token(10:10) = CHAR(0)
          DO i = 1, 4
             IF( token(1:1) .EQ. ' ' ) token(1:9) = token(2:10)
          ENDDO
c 3000     FORMAT('SEG',I5.5,'.DAT')
 3000     FORMAT(I5,'.DAT')
          CALL getfil( -4, ilun, token, istat )
          IF( istat .NE. 0 ) THEN
              PRINT *,' ***  ERROR  ***  Could not open file ',token
              istop = 1
          ENDIF
          RETURN
      ENDIF
c****
c****  We finished a dataset, now see if there was another one in
c****  another seg2in list
c****
 4000 CONTINUE
      IF( mlists .GE. nlists ) THEN
          istop = 1
          RETURN
      ENDIF
      CALL rddisc( ipunit, scr, npwrds, istat  )
      IF( istat .NE. npwrds ) THEN
          PRINT *,' ***  ERROR  ***  Program error in seg2ex.',
     *           npwrds, istat
          istop = 1
          RETURN
      ENDIF
      CALL rddiscb( ipunit, path, 100, istat )
      mlists = mlists + 1
      lprint = lscr(1)
      ffilen = lscr(2)
      lfilen = lscr(3)
      ftr = lscr(4)
      ltr = lscr(5)
      IF( jntrcs .GT. MAX_NTRCS ) THEN
          PRINT *,'  ***  ERROR  ***  Too many traces for SEG2IN.'
          PRINT *,' Increase MAX_NTRCS in seg2ex.f and recompile.'
          istop = 1
          RETURN
      ENDIF
      IF( ftr .EQ. -12345 ) ftr = 1
      IF( ltr .EQ. 0 ) ltr = jntrcs
      ntrcs = ltr - ftr + 1
      itrace = 0
      ifilen = ffilen
      IF( path .NE. ' ' ) THEN
          token = path
      ELSE
          token = ' '
          WRITE( token, 3000 ) ffilen
c****     strip leading blanks
          token(10:10) = CHAR(0)
          DO i = 1, 4
             IF( token(1:1) .EQ. ' ' ) token(1:9) = token(2:10)
          ENDDO
      ENDIF
      CALL getfil( -4, ilun, token, istat )                             ! open the first SEG2 file
      IF( istat .NE. 0 ) THEN
          PRINT *,' ***  ERROR  ***  Can not open SEG2 file:',token
          istop = 1
          RETURN
      ENDIF
      CALL rddiscb( ilun, scr, 32, istat )
      iswap = 0
      IF( iscr(1) .EQ. 21818 ) iswap = 1                                ! 553a hex
      IF( iswap .NE. 0 ) CALL swap16( iscr, 4 )                         ! don't swap everything
      IF( iscr(1) .NE. 14933 ) THEN                                     ! must be 3a55 by now
          PRINT *,' ***  ERROR  ***  Input file is not SEG2.'
          PRINT *,' ***  ERROR  *** Must start with 3a55 or 553a.'
          istop = 1
      ENDIF

      RETURN
      END
