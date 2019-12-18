      SUBROUTINE maxoex( buf, ibuf, lbuf, scr, lscr, iscr, istop )
c     MicroMAX disk file output.
c     Combinations handled:
c        SIOSEIS machine     MicroMAX machine
c          SUN                   SUN
c          SUN                   PC
c          SUN                   VMS VAX
c          SUN                   Ultrix VAX
c          VMS VAX               SUN
c          VMS VAX               PC
c          VMS VAX               VMS VAX
c          VMS VAX               Ultrix VAX
c          Ultrix VAX            SUN
c          Ultrix VAX            PC
c          Ultrix VAX            VMS VAX
c          Ultrix VAX            Ultrix VAX
c
c
c  Copyright (C) 1992 Seismic Reflection Processors, Solana Beach, CA.
c  ALL RIGHTS RESERVED.
c
      INTEGER ultrix, vms, sun
      PARAMETER ( ULTRIX = 2 )
      PARAMETER ( VMS = 4 )
      PARAMETER ( SUN = 7 )

      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, jntrcs
      COMMON /maxout/ luno, lunhd1, lunhd2, swap, lprint, machine
      DIMENSION buf(111), lbuf(111), ibuf(111),
     &          scr(111), lscr(111), iscr(111)
      INTEGER*2 ibuf, iscr
      LOGICAL first, swap
      DIMENSION vax(80)
      SAVE
      DATA first/.TRUE./, swap/.FALSE./, ntraces/0/

      IF( first ) THEN
          first = .FALSE.
          CALL podisc( lun1, 1, 0 )                                     ! rewind all the files
          CALL podisc( lunhd1, 1, 0 )
          CALL podisc( lunhd2, 1, 0 )
          nsamps = ibuf(58)                                             ! all traces will be this length
          IF( lbuf(49) .LT. 0 .OR. lbuf(49) .GT. 131000 ) swap = .TRUE.
      ENDIF
c
      IF( istop .LT. 0 ) GOTO 9000
      ntraces = ntraces + 1
      IF( icompt .EQ. SUN ) THEN
          IF( machine .EQ. ULTRIX ) 
     &        CALL swap32( buf(numhdr+1), nsamps )
          IF( machine .EQ. VMS ) THEN
              CALL iee2dr( buf(numhdr+1), nsamps, buf(numhdr+1) )
              CALL swap16( buf(numhdr+1), nsamps*2 )
          ENDIF
      ELSEIF( icompt .EQ. VMS ) THEN
          IF( machine .EQ. SUN ) THEN
              CALL dr2iee( buf(numhdr+1), nsamps, buf(numhdr+1) )
              CALL swap16( buf(numhdr+1), nsamps*2 )
          ELSEIF( machine .EQ. ULTRIX ) THEN
              CALL dr2iee( buf(numhdr+1), nsamps, buf(numhdr+1) )
              CALL swap16( buf(numhdr+1), nsamps*2 )
              CALL swap32( buf(numhdr+1), nsamps )
          ENDIF
      ENDIF
      CALL wrdisc( luno, buf(numhdr+1), nsamps, istat )
      lscr(1) = ntraces
      lscr(2) = lbuf(3)
      IF( lbuf(7) .EQ. 0 ) THEN
          icdpsrt = 0
      ELSE
          icdpsrt = 1
      ENDIF
      lscr(4) = lbuf(6)
      lscr(5) = lbuf(4)
      scr(6) = lbuf(10)
      lscr(8) = lbuf(3)
      scr(9) = buf(49) * 1000.
      lscr(10) = nsamps
      lscr(11) = buf(47) / buf(49) + 1
      IF( ibuf(15) .EQ. 2 ) lscr(11) = nsamps + 1
      scr(20) = ibuf(16)
      scr(24) = ibuf(17)
      scr(26) = ibuf(48)
      scr(27) = lbuf(12)
      scr(28) = lbuf(11)
      lscr(36) = lbuf(21)
      lscr(37) = lbuf(22)
      lscr(38) = lbuf(19)
      lscr(39) = lbuf(20)
      scr(41) = ibuf(51)
      scr(42) = ibuf(50)
      lscr(51) = ibuf(79)
      lscr(52) = ibuf(80)
      lscr(53) = ibuf(81)
      lscr(54) = ibuf(82)
      lscr(55) = ibuf(83)
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' HDR2 ',(lscr(i),i=1,5)
          PRINT *,scr(6),(lscr(i),i=7,8),scr(9),lscr(10)
          PRINT *,(lscr(i),i=11,14),scr(15)
          PRINT *,(scr(i),i=16,20)
          PRINT *,(scr(i),i=21,25)
          PRINT *,(scr(i),i=26,30)
          PRINT *,(scr(i),i=31,35)
          PRINT *,(lscr(i),i=36,39),scr(40)
          PRINT *,(scr(i),i=41,45)
          PRINT *,scr(46),lscr(47),(scr(i),i=48,50)
          PRINT *,(lscr(i),i=51,55)
          PRINT *,lscr(56),scr(57),(lscr(i),i=58,60)
      ENDIF
      vax(6) = scr(6)
      vax(9) = scr(9)
      vax(20) = scr(20)
      vax(24) = scr(24)
      vax(26) = scr(26)
      vax(27) = scr(27)
      vax(28) = scr(28)
      vax(41) = scr(41)
      vax(42) = scr(42)
      IF( icompt .EQ. SUN ) THEN
          IF( machine .EQ. ULTRIX ) 
     &        CALL swap32( scr, 80 )
          IF( machine .EQ. VMS ) THEN
              CALL iee2dr( vax, 80, scr )
              CALL swap16( vax, 80*2 )
              CALL swap32( scr, 80 )
              scr(6) = vax(6)
              scr(9) = vax(9)
              scr(20) = vax(20)
              scr(24) = vax(24)
              scr(26) = vax(26)
              scr(27) = vax(27)
              scr(28) = vax(28)
              scr(41) = vax(41)
              scr(42) = vax(42)
          ENDIF
      ELSEIF( icompt .EQ. VMS ) THEN
          IF( machine .EQ. SUN ) THEN
              CALL dr2iee( scr, nsamps, scr )
              CALL swap16( scr, nsamps*2 )
          ELSEIF( machine .EQ. ULTRIX ) THEN
              CALL dr2iee( scr, nsamps, scr )
              CALL swap16( scr, nsamps*2 )
              CALL swap32( scr, nsamps )
          ENDIF
      ENDIF
      CALL wrdisc( lunhd2, scr, 80, istat )
      IF( istop .EQ. 0 ) RETURN
 9000 CONTINUE
      DO 9100 i = 1, 52
 9100 lscr(i) = 0
      lscr(7) = 1
c**** idtyp is tough.  sioseis set the fold on stacked data.  It also
c**** has the cdp trace number set to 1 and the shot trace number set to 0
      scr(8) = buf(49) * 1000.                                          ! sample interval
      lscr(9) = ibuf(58)                                                ! number of samples
      lscr(47) = icdpsrt
      lscr(49) = jntrcs
      vax(8) = scr(8)
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' HDR1',(lscr(i),i=1,5)
          PRINT *,lscr(6),lscr(7),scr(8),lscr(9),lscr(10)
          PRINT *,lscr(11),lscr(12),scr(13),scr(14),lscr(15)
          PRINT *,lscr(16),lscr(17),scr(18),scr(19),lscr(20)
          PRINT *,lscr(21),(scr(i),i=22,25)
          PRINT *,lscr(26),lscr(27),scr(28),scr(29),lscr(30)
          PRINT *,(scr(i),i=31,35)
          PRINT *,(scr(i),i=36,40)
          PRINT *,lscr(41),scr(42),lscr(43),lscr(44),lscr(45)
          PRINT *,(lscr(i),i=46,50)
          PRINT *,scr(51),lscr(52),lscr(53),lscr(54),lscr(55)
      ENDIF
      IF( icompt .EQ. SUN ) THEN
          IF( machine .EQ. ULTRIX ) 
     &        CALL swap32( scr, 52 )
          IF( machine .EQ. VMS ) THEN
              CALL swap32( scr, 52 )
              CALL iee2dr( vax, 1, vax)
              CALL swap16( vax, 2 )
              scr(8) = vax(8)
          ENDIF
      ELSEIF( icompt .EQ. VMS ) THEN
          IF( machine .EQ. SUN ) THEN
              CALL dr2iee( scr, 52, scr )
              CALL swap16( scr, 52*2 )
          ELSEIF( machine .EQ. ULTRIX ) THEN
              CALL dr2iee( scr, 52, scr )
              CALL swap16( scr, 52*2 )
              CALL swap32( scr, 52 )
          ENDIF
      ENDIF
      CALL wrdisc( lunhd1, scr, 52, istat )

      END
