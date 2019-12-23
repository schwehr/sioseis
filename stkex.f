      SUBROUTINE stkex( buf, lbuf, ibuf, scr, lscr, iscr,
     &                    istop, nready )
c mod Oct 02 - Trim/median stack bombed when first trace was zero
c mod 11 Mar 03 - Add panel.
c mod Apr 09 - Allow unsigned nsamps
c mod 15 June 09 - Apr 09 was done wrong!
c mmod 12 Apr 10 - Use end mute integer mils rather than real second (word 48)
c
      PARAMETER (IAPSIZ = 500000)
      DIMENSION buf(111), lbuf(111), ibuf(111),
     &          scr(111), lscr(111), iscr(111)
      INTEGER*2 ibuf, iscr
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, jntrcs,
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
      COMMON /trim/ lunparams, nlists, nwrds, luntrim
      COMMON /apmem/ apdata(IAPSIZ)
      PARAMETER (isize = 262144 )
      COMMON /transp/ tarray(isize)
      DIMENSION xxt(90), trhdr(60)
      DATA mlists/0/, mindel/40000/, last/0/, ntraces/0/
      SAVE

      IF( istop .EQ. -1 ) GOTO 90                                       ! means end, but no trace in buf
      nready = 0
      IF( ibuf(itridptr) .EQ. 2 .AND. lbuf(51) .NE. -1 ) RETURN
      IF( lbuf(7) .EQ. 0 ) THEN
          no = lbuf(3)
          itrno = lbuf(4)
      ELSE
          no = lbuf(6)
          itrno = lbuf(7)
      ENDIF
      si = buf(lsisptr)
c      nsamps = ibuf(isampptr)
      nsamps = numdat
      delay = buf(ldelsptr)
      idelay = NINT(delay / buf(lsisptr)) + 1
      IF( mlists .EQ. 0 ) THEN
          CALL podisc( lunparams, 1, 0 )                                ! get the first parameter list from disk
          CALL rddisc( lunparams, lscr, nwrds, istat )
          mlists = mlists + 1
          lprint = lscr(1)
          savein = scr(2)
          saveout = scr(3)
          IF( savein .NE. 0 ) pct = 1. - (savein / 100.)
          IF( saveout .NE. 0 ) pct = 1. - (saveout / 100.)
          panel = scr(4)
          nxxts = lscr(6)
          IF( nxxts .GT. 0 ) THEN
              DO i = 1, nxxts
                 xxt(i) = scr(5+i)
              ENDDO
          ENDIF
c****     make sure the "ap" gets initialize
          CALL inap( buf(numhdr+1), nsamps )
      ENDIF
      iout = 0                                                          ! force the data out of the ap
      CALL rlseap( buf(numhdr+1), nsamps )
c****
c****  Collect all the traces on disk until End-of-Gather or End-of-Job
c****
      nwrds = nsamps + numhdr
      CALL wrdisc( luntrim, buf(1), nwrds )
      ntraces = ntraces + 1
      mindel = MIN( mindel, idelay )
      last = MAX( last, idelay-1+nsamps )
      IF( ntraces .EQ. 1 ) THEN                                         ! save the first trace header
          DO i = 1, 60
             trhdr(i) = buf(i)
          ENDDO
      ENDIF
      IF( panel .EQ. 0. .AND. lbuf(51) .NE. -1 .AND. istop .EQ. 0 ) THEN
          nready = 0
          RETURN
      ENDIF
c****
c**** A stack panel is a progressive stack.  Each input trace of the
c**** gather is added to the stack and the current stacked trace is
c**** output.  Let's do the entire sum for each new trace so that
c**** things like mutes can be honored.
c****
      IF( panel .NE. 0. ) THEN
c****     The summed output goes in buf.
c****     The trace from disk goes in scr.
c****     The divisor goes in apdata
c****     let's assume all traces in the gather have the same trace
c****     length and delay.  Should/could check, but ....
c****     So nsamps is still good!
          DO i = 1, nsamps
             buf(numhdr+i) = 0.
             apdata(nextad+i-1) = 0.
          ENDDO
          CALL podisc( luntrim, 0, 0 )
          DO j = 1, ntraces
             CALL rddisc( luntrim, scr, numhdr+nsamps, istat )
c             muteend = NINT((scr(48)-delay) / si)
             muteend = NINT((iscr(57)*1000.-delay) / si)
             DO i = muteend+1, nsamps-muteend
                buf(numhdr+i) = buf(numhdr+i) + scr(numhdr+i)
                apdata(nextad+i-1) = apdata(nextad+i-1) + 1.
             ENDDO
          ENDDO
          DO i = 1, nsamps
             buf(numhdr+i) = buf(numhdr+i) / apdata(nextad+i-1)
          ENDDO
          lbuf(7) = ntraces
          in = 0                                                        ! the data is not in the "ap"
          nready = 1
          RETURN
      ENDIF
c****
c**** Do the whole dataset, make sure everything has the same delay
c**** and length.  Start by adding the pads.
c****
   90 CONTINUE
      CALL podisc( luntrim, 0, 0 )
      nsamps = last - mindel+1
c****
c****  trim stack here  (save inner traces or save outer traces)
c****
      IF( savein+saveout .NE. 0. ) THEN
          IF( nsamps*ntraces+nextad .GT. IAPSIZ ) THEN
              PRINT *,' ***  ERROR  ***  Not enoygh memory for TRIM.'
              PRINT *,' Decrease the record length.'
              PRINT *,' Make sure there is an end-of-gather flag.'
              STOP
          ENDIF
          index = nextad
          DO i = 1, ntraces
             CALL rddisc( luntrim, scr, numhdr, istat )
c             npts = iscr(isampptr)
             CALL ushort2long( iscr(isampptr), npts )
             delay = scr(ldelsptr)
             idelay = NINT(delay / scr(lsisptr))
c****        make every trace start at the same time
             IF( idelay .GT. mindel ) THEN
                 n = idelay - mindel
                 DO j = 1, n
                    apdata(index+j) = 0.
                 ENDDO
                 index = index + n
             ENDIF
             CALL rddisc( luntrim, apdata(index+1), npts, istat )
             index = index + npts
c****        make sure every trace ends at the same time
c****          last is from 1, as if no delay
             n = last - npts
             IF( n .GT. 0 ) THEN
                 DO j = 1, n
                    apdata(index+j) = 0.
                 ENDDO
                 index = index + n
             ENDIF
          ENDDO
          DO i = 1, nsamps
             n = 0
             index = nextad
             DO j = 1, ntraces
                IF( apdata(index+i) .NE. 0. ) THEN
                    n = n + 1
                    scr(n) = apdata(index+i)
                ENDIF
                index = index + nsamps
             ENDDO
             buf(numhdr+i) = 0.
             IF( n .LE. 0 ) GOTO 1000
             rn = FLOAT(n)
             rn2 = rn / 2.
             mid = NINT(rn2)
             itemp = rn*pct/2.
             divisor = 0.
             IF( n .GT. 1 ) CALL sort( n, scr )
             n1 = mid - itemp
             IF( n1 .LE. 0 ) n1 = 1
             n2 = mid + itemp
             IF( n2 .GT. n ) n2 = n
             IF( AND(lprint,8) .NE. 0 ) PRINT *,
     &          ' n=',n,' n1=',n1,' n2=',n2,' itemp=',itemp
             IF( saveout .EQ. 0. ) THEN
                 DO j = n1, n2
                    buf(numhdr+i) = buf(numhdr+i) + scr(j)
                    divisor = divisor + 1.
                 ENDDO
             ELSE
                 DO j = 1, n1
                    buf(numhdr+i) = buf(numhdr+i) + scr(j)
                    divisor = divisor + 1.
                 ENDDO
                 DO j = n2, n
                    buf(numhdr+i) = buf(numhdr+i) + scr(j)
                    divisor = divisor + 1.
                 ENDDO
             ENDIF
             IF( divisor .NE. 0. ) buf(numhdr+i) = buf(numhdr+i)/divisor
 1000        CONTINUE
         ENDDO
      ENDIF
c****
c****   XXT here - inner/outer muting
c****
c	 print *,' nxxts=',nxxts,' xxt=',(xxt(i),i=1,6)
      IF( nxxts .GT. 0 ) THEN
          DO i = 1, last
             scr(i) = 0.                                                ! the output trace
             apdata(nextad+i) = 0.                                      ! the divisor trace
          ENDDO
          DO i = 1, ntraces
             CALL rddisc( luntrim, buf, numhdr, istat )
             si = buf(lsisptr)
c             nsamps = ibuf(isampptr)
             CALL ushort2long( ibuf(isampptr), nsamps )
             delay = buf(ldelsptr)
             idelay = NINT(delay / buf(lsisptr)) + 1
             x = ABS(FLOAT(lbuf(ldisptr)))
c	 print *,' si=',si,' nsamps=',nsamps,' idelay=',idelay
             CALL rddisc( luntrim, buf, nsamps, istat )
             indexo = idelay - mindel
             indexap = nextad + indexo
             ixxt = 1
c	 print *,' input=',(buf(j),j=1,10)
             DO j = 1, nsamps
                time = FLOAT(j-1)*si + delay
c	 print *,' time=',time,' ixxt=',ixxt,' xxt=',xxt(ixxt+2)
                IF( time .GE. xxt(ixxt+2) .AND. ixxt+3 .LT. nxxts )
     &              ixxt = ixxt + 3
                IF( x .GE. xxt(ixxt) .AND. x .LE. xxt(ixxt+1) ) THEN
                    scr(indexo+j) = scr(indexo+j) + buf(j)
                    apdata(indexap+j) = apdata(indexap+j) + 1
                ENDIF
             ENDDO
          ENDDO
c	 print *,' lats=',last
          DO i = 1, last
             IF( apdata(nextad+i) .NE. 0. ) THEN
                 buf(numhdr+i) = scr(i) / apdata(nextad+i)
             ELSE
                 buf(numhdr+i) = 0.
             ENDIF
          ENDDO
      ENDIF
c****
c****
c**** Do the trace header before leaving
c****
      DO i = 1, 60
         buf(i) = trhdr(i)
      ENDDO
      lbuf(3) = 0
      lbuf(4) = 0
      lbuf(6) = no
      lbuf(7) = 1
      lbuf(10) = 0
      lbuf(15) = 1
c****  the fold is not ntraces, but worry about it later
      lbuf(17) = NINT(divisor)                                          ! the number of traces in the stack (fold)
      ibuf(55) = NINT(mindel*si*1000.)
      ibuf(58) = nsamps
      numdat = nsamps
      lbuf(51) = 0
      mindel = 40000
      last = 0
      ntraces = 0
      notrcs = 1
      nready = 1
      CALL podisc( luntrim, 0, 0 )
      RETURN
      END
