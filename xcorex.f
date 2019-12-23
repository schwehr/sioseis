      SUBROUTINE xcorex( buf, lbuf, ibuf, scr, lscr, iscr )
c
c  Written by Paul Henkart, Scripps Institution of Oceanography, October 1998
c  Copyright (C) 1998 The Regents of the University of California
c  ALL RIGHTS RESERVED.
c
c  mod 6 Apr 99 - Add parameter CATS - Use previous trace as the pilot
c
      COMMON /sioap/ iasgnd, irelse, in, iout, nextad, lapsiz, ifree,
     *     iuseap
      COMMON /apmem/ a(32766)
      COMMON /readt/ ilun, numhdr, nsamps, ihunit, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr,
     *               nrskip, nfiles
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr, lsmusptr, lemusptr,
     *                 lsisptr,  lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr,   ifoldptr, icvleptr, lespnptr
      COMMON /corr/ ipunit, nlists, npwrds
      DIMENSION buf(111), lbuf(111), ibuf(111),
     &          scr(111), lscr(111), iscr(111)
      DIMENSION setp(2), setd(2)
      INTEGER*2 ibuf, iscr
      INTEGER lprint, pilot, psno, ptr
      DATA lastpi/0/
      SAVE

c****
c****  If a dead trace, exit now!
c****
      IF( ibuf(15) .EQ. 2 ) RETURN
C****
c****
      IF( mlists .EQ. 0 ) THEN
          CALL podisc( ipunit, 1, 0 )
          CALL rddisc( ipunit, scr, npwrds, istat  )
          IF( istat .NE. npwrds ) THEN
              PRINT *,' ***  ERROR  ***  Program error in seg2ex.',
     *           npwrds, istat
             STOP
         ENDIF
         mlists = mlists + 1
         lprint = lscr(1)
         pilot = lscr(2)
         nlags = lscr(3)
         setp(1) = scr(4)
         setp(2) = scr(5)
         setd(1) = scr(6)
         setd(2) = scr(7)
         lunpilot = lscr(8)
         psno = lscr(9)
         ptr = lscr(10)
         idouble = lscr(11)
         icats = lscr(12)
c****    get a file for the pilot trace - the pilot will always be there
c****    Edit saved the pilot in lunpilot if PPATH was given.
         IF( lunpilot .EQ. 0 )
     &       CALL getfil( 1, lunpilot, lscr, istat )
         IF( iuseap .EQ. 1 ) THEN
             PRINT *,' ***  ERROR  ***  CORR not set up for AP.'
             STOP
         ENDIF
         nsamps_pilot = ibuf(58)                                        ! If xcored wrote it.
      ENDIF
      IF( lbuf(7) .EQ. 0 ) THEN
          no = lbuf(3)
          itrno = lbuf(4)
      ELSE
          no = lbuf(6)
          itrno = lbuf(7)
      ENDIF
      si = buf(lsisptr)
c      nsamps_data = ibuf(isampptr)
      nsamps_data = nsamps
      delay = buf(ldelsptr)
c****
c****  Get the data into the "ap"
c****
      CALL inap( buf(numhdr+1), nsamps_data )
c**** pilot was preset to 1 by the edit
      IF( itrno .EQ. pilot ) THEN                                       ! save the pilot on disk
          CALL podisc( lunpilot, 0, 0 )
          CALL wrdisc( lunpilot, buf(numhdr+1), nsamps_data )
          nsamps_pilot = nsamps_data
      ENDIF
      CALL podisc( lunpilot, 0, 0 )
      CALL rddisc( lunpilot, a(nextad), nsamps_pilot, istat )
c****
c****   If CATS, save this trace to use as the pilot on the next trace
c**** The number of the pilot trace that's used in the correlation is
c**** in segy header word 5.
c****
      IF( icats .NE. 0 ) THEN
          CALL podisc( lunpilot, 0, 0 )
          CALL wrdisc( lunpilot, buf(numhdr+1), nsamps_data )
          nsamps_pilot = nsamps_data
          IF( lastpi .NE. lbuf(3) ) THEN
              CALL podisc( lunpilot, 0, 0 )
              CALL rddisc( lunpilot, a(nextad), nsamps_pilot, istat )
          ENDIF
          lastpi = lbuf(3)
      ENDIF
c****
c****  Set the indices (p for pilot, d for data)
c****
      istartp = NINT((setp(1)-delay)/si)
      IF( setp(2) .NE. 0. ) THEN
          iendp = NINT((setp(2)-delay)/si)
      ELSE
          iendp = nsamps_pilot
      ENDIF
      nsampsp = iendp - istartp + 1
      istartd = NINT((setd(1)-delay)/si)
      IF( setd(2) .NE. 0. ) THEN
          iendd = NINT((setd(2)-delay)/si)
      ELSE
          iendd = nsamps_data
      ENDIF
      nsampsd = iendd - istartd + 1
      IF( nlags .EQ. 0 ) nlags = (MIN0(nsampsd,nsampsp)/2)
c****
c****  Do the correlation and bring it out of the ap
c****
      IF( IAND(lprint,4) .NE. 0 ) THEN
        PRINT *,' istartp= ',istartp,' iendp=',iendp,' nsampsp=',nsampsp
        PRINT *,' istartd= ',istartd,' iendd=',iendd,' nsampsd=',nsampsd
        PRINT *,' nlags=',nlags, ' nsamps_pilot=',nsamps_pilot,
     &    ' nextad=',nextad
      ENDIF
      IF( lunpilot .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  No pilot given.'
          STOP
      ENDIF
      IF( idouble .EQ. 0 ) THEN
          CALL convo( 2, a(in+istartd), nsampsd, a(nextad+istartp),
     &       nsampsp, a(nextad+nsamps_pilot), nlags )
      ELSE
          CALL dconvo( 2, a(in+istartd), nsampsd, a(nextad+istartp),
     &       nsampsp, a(nextad+nsamps_pilot), nlags )
      ENDIF
      itemp = nextad+nsamps_pilot-1
      DO i = 1, nlags
         buf(numhdr+i) = a(itemp+i)
      ENDDO
      ibuf(idelmptr) = 0
      ibuf(isampptr) = nlags
      CALL long2ushort( nlags, ibuf(isampptr) )
      numdat = nlags
      buf(ldelsptr) = 0.
      in = 0

      RETURN
      END
