      SUBROUTINE swellex( buf, lbuf, ibuf, scr, lscr, iscr,
     &                    istop, nready )
c****  SWELLEX.  Swell needs to know the water depth forward and backwards
c so that the average is in the center of the window.
c     Save the input to a temporary disk file, remembering that nsamps
c in the trace header may have been changed (is that true?).
c     Once there are enough traces on disk, start outputting traces after
c shifting the traces in time.
c
c   istop < 0, no trace in buf - just finish
c   istop = 0, trace in buf -  no end in sight
c   istop > 0, this is the last trace - trace in buf
c
c  Copyright (C) 2009, The Regents of the University of California
c  ALL RIGHTS RESERVED.
c
c  Written by Paul Henkart
c  mod 6 Nov 09 - remove ihdr & lhdr
c               - Add INDEX
c  mod 14 Jan 10 - nready was 1 too many when istop = -1
c  mod 21 Dec 10 - Dead traces have bad swell picks, so use last good pick.
c
      PARAMETER ( MAX_SWELL = 1000 )
      DIMENSION buf(111), lbuf(111), ibuf(111),
     &          scr(111), lscr(111), iscr(111)
      INTEGER*2 ibuf, iscr
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr,
     *               nrskip, nfiles
      COMMON /sioap/ iasgnd, irelse, in, iout, nextad, lapsiz, ifree,
     *     iuseap
      CHARACTER*20 token
      COMMON /swell/ lun_params, nparams
      DIMENSION weights( MAX_SWELL )
      INTEGER hdr, start_adr
      LOGICAL first
      REAL last_good/0./
      DATA first/.TRUE./, nread/0/, start_adr/0/, last_adr/0/
      SAVE
c
      IF( istop .LT. 0 ) THEN
          nready = (n+1) / 2 -1
          RETURN
      ENDIF
      nready = 0
      iout = 0
      CALL rlseap( buf(numhdr+1), numdat )
      IF( first ) THEN
          first = .FALSE.
          CALL getfil( 1, lun_data, token, istat )
          CALL podiscb( lun_params, 1, 0 )   ! rewind
          CALL rddisc( lun_params, scr, nparams, istat )
          n = lscr(1)
          real_n = FLOAT(n)
          lprint = lscr(2)
          index = lscr(3)
          hdr = lscr(4)
          CALL rddisc( lun_params, weights, n, istat )
      ENDIF
c**** save the header and trace to disk
      IF( istop .GE. 0 ) THEN
          DO i = 1, numhdr
             scr(i) = buf(i)
          ENDDO
          CALL long2ushort( numdat, iscr(58) )
          CALL podiscb( lun_data, 1, last_adr )
          CALL wrdisc( lun_data, scr, numhdr )
          CALL wrdisc( lun_data, buf(numhdr+1), numdat )
          CALL adrdisc( lun_data, last_adr )
          nread = nread + 1
      ENDIF
      IF( nread .LT. n ) THEN
          IF( nread .LT. (n+1)/2 ) nready = 1
          RETURN
      ELSE
          CALL podiscb( lun_data, 1, start_adr )
          sum = 0.
          DO i = 1, n
             IF( i .EQ. (n+1)/2 ) CALL adrdisc( lun_data, iaddress )
             CALL rddisc( lun_data, scr, numhdr, istat )
             nsamps = iscr(58)
             CALL podisc( lun_data, 2, nsamps )
c****  the second trace will be the first trace next time
             IF( i .EQ. 1 ) THEN
                 CALL adrdisc( lun_data, start_adr )
             ENDIF
             IF( ihdr+lhdr+hdr .EQ. 0 ) THEN
c****            assume the pick is bad if the trace is dead
                 IF( iscr(15) .EQ. 1 ) THEN
                     sum = sum + scr(50)
                     last_good = scr(50)
                 ELSE
                     sum = sum + last_good
                 ENDIF
             ELSEIF( ihdr .NE. 0 ) THEN
                 IF( iscr(15) .EQ. 1 ) THEN
c****                argh.  gfortran can't float a short integer in 1 step
                     ltemp = iscr(ihdr)
                     sum = sum + FLOAT(ltemp)
                     last_good = FLOAT(ltemp)
                 ELSE
                     sum = sum + last_good
                 ENDIF
             ELSEIF( lhdr .NE. 0 ) THEN
                 IF( iscr(15) .EQ. 1 ) THEN
                     sum = sum + FLOAT(lscr(lhdr))
                     last_good = FLOAT(lscr(lhdr))
                 ELSE
                     sum = sum + last_good
                 ENDIF
             ELSEIF( hdr .NE. 0 ) THEN
                 IF( iscr(15) .EQ. 1 ) THEN
                     sum = sum + scr(hdr)
                     last_good = scr(hdr)
                 ELSE
                     sum = sum + last_good
                 ENDIF
             ENDIF
          ENDDO
          average = sum / real_n
          IF( IAND(lprint,2) .NE. 0 )
     &        PRINT *,' sum=',sum,' average=',average
      ENDIF
c****
c****  Now apply the shift to the center trace
c****
      CALL podiscb( lun_data, 1, iaddress )
      CALL rddisc( lun_data, buf, numhdr, istat )
      IF( hdr .EQ. 0 ) THEN
          data_time = buf(50)
      ELSE
          data_time = buf(hdr)
      ENDIF
      shift = average - data_time
      IF( index .NE. 0 ) buf(index) = shift
      numdat = ibuf(58)
      ltemp = ibuf(59)
      si = FLOAT(ltemp) / 1000000.
      nshift = NINT( (average - data_time) / si )
      IF( average .EQ. 0 ) nshift = 0
      CALL rddisc( lun_data, buf(numhdr+1), numdat , istat )
      IF( IAND(lprint,2) .NE. 0 )
     &    PRINT *,lbuf(3),lbuf(4),' shift =',nshift,
     &   (average - data_time), data_time, average,' numdat=',numdat
      IF( nshift .GT. 0 ) THEN
          DO i = numdat, nshift, -1
             buf(numhdr+i) = buf(numhdr+i-nshift)
          ENDDO
          DO i = 1, nshift
             buf(numhdr+i) = 0
          ENDDO
      ELSEIF( nshift .LT. 0 ) THEN
          nshift = -nshift
          DO i = 1, numdat-nshift
             buf(numhdr+i) = buf(numhdr+i+nshift)
          ENDDO
          DO i = 1, nshift
             buf(numhdr+numdat+1-i) = 0
          ENDDO
      ENDIF
      nready = 1
      IF( istop .NE. 0 ) nready = (n+1) / 2
      RETURN
c****
c****
      ENTRY get_swell( buf, lbuf, ibuf )
c      CALL adrdisc( lun_data, junk)
      CALL rddisc( lun_data, buf, numhdr, istat )
      numdat = ibuf(58)
      CALL rddisc( lun_data, buf(numhdr+1), numdat , istat )
      RETURN
      END
