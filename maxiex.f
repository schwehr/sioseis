      SUBROUTINE maxiex( buf, ibuf, lbuf, istop )
c     MicroMAX disk file input.
c     Amoung the problems with this format are:
c  1)  I don't know what type of machine wrote the data.  I'm going to
c  assume that it was an IEEE machine.  I'll also assume that the machine
c  we're running on is IEEE.  That leaves big endian vs. little endian.
c  Both for the machine that created the data as well as the machine we're
c  running this on.  We need to swap bytes if they are different.  e.g.
c  if the data was created on a Compaq and sioseis is running on a Sun, we
c  need to byte swap.  Word 49 of header 1 (.hd1) is the number of data
c  channels actually written to the disk file.  If this is negative or
c  greater than 2**16-1, then we need to do a byte swap.
c
c
c  Copyright (C) 1992 Seismic Reflection Processors, Solana Beach, CA.
c  ALL RIGHTS RESERVED.
c
      INTEGER ultrix, vms, sun
      PARAMETER ( ULTRIX = 2 )
      PARAMETER ( VMS = 4 )
      PARAMETER ( SUN = 7 )
      DIMENSION buf(111), lbuf(111), ibuf(111)
      DIMENSION vax(80)
      INTEGER*2 ibuf
      INTEGER cdpno, cdptrno
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /readt/ itunit, numhdr, nsamps, ihunit, ireeln, jntrcs
      COMMON /maxin/ luni, lunhd1, lunhd2, fno, lno, noinc, ftr, ltr, 
     &       trinc, lprint
      LOGICAL first, swap
      SAVE
      DATA first/.TRUE./, swap/.FALSE./, cdpno/0/, cdptrno/0/
      DATA ntraces/0/, nhdr1/52/
c
      IF( first ) THEN
          first = .FALSE.
          CALL podisc( lun1, 1, 0 )                                     ! rewind all the files
          CALL podisc( lunhd1, 1, 0 )
          CALL podisc( lunhd2, 1, 0 )
          CALL rddisc( lunhd1, buf, nhdr1, istat )
          IF( icompt .EQ. SUN ) CALL swap32( buf, 52 )
          IF( icompt .EQ. VMS ) CALL pc2dr( buf(8), 1, buf(8) )
          ntrace = lbuf(1)
          si = buf(8) / 1000.
          nsamps = lbuf(9)
          icdpsrt = lbuf(47)                                            ! 0 = not sorted, 1 = sorted
          jntrcs = lbuf(49)
          iftype = lbuf(52)
          IF( IAND(lprint,1) .NE. 0 ) PRINT *,' si =',si,' nsamps=',
     &        nsamps ,' icdpsrt = ',icdpsrt,' jntrcs =',jntrcs,
     &        ' iftype = ',iftype
      ENDIF
  100 CALL rddisc( lunhd2, buf, 80, istat )
      IF( istat .NE. 80 ) THEN
          istop = -1
          RETURN
      ENDIF
      IF( icompt .EQ. SUN ) CALL swap32( buf, 80 )
      IF( icompt .EQ. VMS ) THEN
          DO 110, i = 1, 80
  110     vax(i) = buf(i)
          CALL pc2dr( vax, 80, vax )
          buf(6) = vax(6)
          buf(9) = vax(9)
          DO 120, i = 15, 46
  120     buf(i) = vax(i)
          buf(48) = vax(48)
          buf(49) = vax(49)
          buf(50) = vax(50)
          buf(57) = vax(57)
      ENDIF
      istrac = lbuf(1)
      ishot = lbuf(2)
      irecvr = lbuf(3)
      iscdp = lbuf(4)
      ischan = lbuf(5)
      offset = buf(6)
      idxtype = lbuf(7)
      iffid = lbuf(8)
      samrat = buf(9)
      nsamps = lbuf(10)
      nmute = lbuf(11)
      nmute2 = lbuf(12)
      ixcor = lbuf(13)
      iycor = lbuf(14)
      tstat = buf(15)
      tstogo = buf(16)
      tdatum = buf(17)
      tcdp = buf(18)
      anorml = buf(19)
      xtrces = buf(20)
      xsum = buf(21)
      ysum = buf(22)
      cdpxsavg = buf(23)
      fold = buf(24)
      fbxpick = buf(25)
      uhxtime = buf(26)
      elvxsrc = buf(27)
      elvxgrp = buf(28)
      zxhole = buf(29)
      cdpgrp = buf(30)
      fxbamp = buf(31)
      prefbxamp = buf(32)
      trcxamp = buf(33)
      grpxnum = buf(34)
      srcxnum = buf(35)
      jxgrpx = buf(36)
      jygrpx = buf(37)
      jxsrcx = buf(38)
      jysrcx = buf(39)
      dstos = buf(40)
      statrx = buf(41)
      statsx = buf(42)
      rstrx = buf(43)
      rstsx = buf(44)
      rfstrx = buf(45)
      rfstsx = buf(46)
      itrcdp = lbuf(47)
      tmultiplx = buf(48)
      uhxgrp = buf(49)
      v1grpx = buf(50)
      ixyear = lbuf(51)
      ixday = lbuf(52)
      ixhour = lbuf(53)
      ixmin = lbuf(54)
      ixsec = lbuf(55)
      ixrtype = lbuf(56)
      wgtcdp = buf(57)
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,(lbuf(i),i=1,5)
          PRINT *,buf(6),(lbuf(i),i=7,8),buf(9),lbuf(10)
          PRINT *,(lbuf(i),i=11,14),buf(15)
          PRINT *,(buf(i),i=16,20)
          PRINT *,(buf(i),i=21,25)
          PRINT *,(buf(i),i=26,30)
          PRINT *,(buf(i),i=31,35)
          PRINT *,(lbuf(i),i=36,39),buf(40)
          PRINT *,(buf(i),i=41,45)
          PRINT *,buf(46),lbuf(47),(buf(i),i=48,50)
          PRINT *,(lbuf(i),i=51,55)
          PRINT *,lbuf(56),buf(57),(lbuf(i),i=58,60)
      ENDIF
c**** count the traces read and set istop if the current trace is the last
      ntraces = ntraces + 1
      IF( ntraces .EQ. ntrace ) istop = 1
c****  
c****   We have to count the traces in the rp gather if it's been gathered
c****
      IF( icdpsrt .EQ. 1 ) THEN
          cdptrno = cdptrno + 1
          IF( iscdp .NE. cdpno ) THEN                                   ! is the MicroMAX bin number different?
              cdptrno = 1
          ENDIF
          cdpno = iscdp
      ENDIF
c****
c****   See if it's a shot/rp and trace the user wanted
c****
      IF( icdpsrt .EQ. 0 ) THEN
          IF( ishot .LT. fno ) GOTO 100
          IF( ischan .LT. ftr ) GOTO 100
          IF( lno .NE. 0 .AND. ishot .GT. lno ) GOTO 100
          IF( ltr .NE. 0 .AND. ischan .GT. ltr ) GOTO 100
      ELSE
          IF( iscdp .LT. fno ) GOTO 100
          IF( cdptrno .LT. ftr ) GOTO 100
          IF( lno .NE. 0 .AND. iscdp .GT. lno ) GOTO 100
          IF( ltr .NE. 0 .AND. cdptrno .GT. ltr ) GOTO 100
      ENDIF
c****
c****  set the SEGY trace header
c****
      si = samrat / 1000.
      DO 500 i = 1, numhdr
  500 lbuf(i) = 0
      lbuf(1) = istrac
      lbuf(3) = ishot
      lbuf(4) = ischan                                                  ! original recording channel number
      lbuf(6) = iscdp
      lbuf(7) = cdptrno
      lbuf(10) = NINT(offset)
      lbuf(11) = elvxgrp
      lbuf(12) = elvxsrc
      lbuf(19) = jxsrcx
      lbuf(20) = jysrcx
      lbuf(21) = jxgrpx
      lbuf(22) = jygrpx
      IF( cdptrno .EQ. itrcdp ) lbuf(51) = -1                           ! set the sioseis end of gather flag
      IF( nmute .NE. nsamps + 1 ) THEN
          ibuf(15) = 1
      ELSE
          ibuf(15) = 2
      ENDIF
      ibuf(16) = xtrces
      ibuf(17) = fold
      ibuf(48) = uhxtime
      ibuf(50) = statsx
      ibuf(51) = statrx
      ibuf(52) = tstat
      ibuf(57) = FLOAT(nmute) * si * 1000.
      ibuf(58) = nsamps
      ibuf(59) = samrat * 1000.
      ibuf(79) = ixyear
      ibuf(80) = ixday
      ibuf(81) = ixhour
      ibuf(82) = ixmin
      ibuf(83) = ixsec
      buf(49) = si
      buf(47) = FLOAT(nmute) * si
c****
c****  now get the data
c****
      IF( iftype .EQ. 0 ) THEN                                          ! 0 = 32 bit floating point (whose??)
          CALL rddisc( luni, buf(numhdr+1), nsamps, istat )
          IF( istat .NE. nsamps ) THEN
              istop = -1
              RETURN
          ENDIF
          IF( icompt .EQ. SUN ) CALL swap32( buf(numhdr+1), nsamps )
          IF( icompt .EQ. VMS )
     &        CALL pc2dr( buf(numhdr+1), nsamps, buf(numhdr+1) )
      ELSEIF( iftype .EQ. 1 ) THEN
          IF( icompt .EQ. VMS .AND. nsamps / 2 * 2 .NE. nsamps ) THEN
              PRINT *,' nsamps must be even when using 16 bit integers.'
c             The problem here is that the VMS Vax doesn't have byte disk I/O
              STOP
          ENDIF
          IF( icompt .EQ. VMS ) THEN
              CALL rddisc( luni, buf(numhdr+1), nsamps/2, istat )
              IF( istat .NE. nsamps/2 ) THEN
                  istop = -1
                  RETURN
              ENDIF
          ELSE
              CALL rddiscb( luni, buf(numhdr+1), nsamps*2, istat )
              IF( istat .NE. nsamps*2 ) THEN
                  istop = -1
                  RETURN
              ENDIF
          ENDIF
          IF( icompt .EQ. SUN ) CALL swap16( buf(numhdr+1), nsamps )
          DO 1000, i = 1, nsamps
             buf(numhdr+nsamps-i+1)  = ibuf(numhdr*2+i)
 1000     CONTINUE
      ENDIF

      

      END
