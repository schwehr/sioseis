      SUBROUTINE gainex( buf, lbuf, ibuf, scr, num_gains )
c
c  ARGUMENTS:
c  buf   - The trace, with SEGY header as TYPE REAL
c  lbuf  - The trace, with SEGY header as TYPE INTEGER*4
c  ibuf   - The trace, with SEGY header as TYPE INTEGER*2
c  scr   - A scratch array.
c
c  COPYRIGHT (C) Seismic Reflection Processors, Solana Beach, CA. 92075
c  ALL RIGHTS RESERVED.
c  Written by Paul Henkart, SRP, 26 June 1990
c  mod 5 November 1990 - added the OSU range scaling method
c  mod 2 Apr 92 - add type 7 - take modulus of a complex trace
c  ?????   - Apollo f77 has to have the entire function on 1 line.
c  mod 5 May 93 - On type scaling (OSU)
c           1) No scaling should be done is abs(range) < rscale
c           2) gain should use abs(range).
c  mod 12 feb 96 - type 3 & 5 with subwb were wrong
c  mod 1 Apr 96 - type 2 with negative ranges killed the trace.
c  mod 17 Oct 96 - type 1 used etime incorrectly
c  mod 3 Apr 99 - subwb was wrong. geez get it right!
c  mod 15 Nov 99 - on type 7 gain (complex numbers), don't change the
c              segy header entries for the delay.
c  mod 19 May 00 - Use numdat rather than segy header for nsamps so
c          that modulus works for nsamps > 32k
c  mod Jul 00 - Add gain type 9 and parameters TGP and ADDWB.
c  mod Oct 00 - Add TADD and TMULT
c  mod 6 Nov 01 - Save the TGP in the ap simulator
c  mod 7 Jan 03 - idelay wasn't being used, so comment it out
c  mod 14 Feb 03 - params array was too small
c  mod 9 Jun 03 - Add WINLEN (amplitude running average window length)
c  mod 1 Dec 03 - Make multiple list with TGP work.
c  mod 24 Mar 04 - Allow subwb on tgp (type 9)
c  mod 17 Feb 05 - Double the sample interval after doing complex modulus
c  mod 24 Mar 05 - Convert the integer sample interval by NINT after the floating
c  mod 6 Sep 05 - Allow nsamps > 32767 on complex modulus.
c  mod 5 Jan 07 - Allow 3 process gains - add num_gains
c  mod 9 Jan 07 - Don't assume ap address is 1 if it's non-zero.
c  mod 5 Mar 07 - Type 5 - Don't do anything when (time-wbt) LE 0
c  mod 20 Jun 07 - Add some diagnostics to type 5.
c  mod 20 Jun 07 - Type 9 subwb didn't work.
c  mod 11 Jun 08 - Correct typo in an error message.
c  mod 22 Jun 08 - Set the start to 1 if it goes negative due to bad wbt
c  mod 17 Jul 08 - Change warning message when bad water bottom time.
c  mod 19 Nov 08 - Skip dead traces
c  mod 1 Dec 08 - Kill the trace if subwb yes and the water bottom time is 0.
c  mod 18 Aug 09 - Kill the trace if subwb yes, type 5, and wb < delay.
c  mod 18 Feb 10 - Initial first not set correctly.
c  mod 14 Jun 11 - Add type 10 (20log(t*v))
c                - Add parameter v
c  mod 28 Jun 12 - type 5 subwb was wrong.
c
      PARAMETER ( max_gains = 3 )
      DIMENSION buf(1111), lbuf(1111), ibuf(1111), scr(1111)
      INTEGER*2 ibuf
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr
      COMMON /sioap/ iasgnd, irelse, in, iout, nextad, lapsiz, ifree,
     *     iuseap
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr,  lsmusptr,lemusptr,
     *                 lsisptr,  lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr,   ifoldptr, icvleptr, lespnptr
      COMMON /apmem/ a(500000)
      DIMENSION params1(20), lparams1(20)
      EQUIVALENCE (params1(1),lparams1(1))
      INTEGER gaintype(max_gains), fno(max_gains)
      COMMON /gains/ igunit(max_gains), nglists(max_gains),
     &      ngwrds(max_gains)
      LOGICAL first(max_gains)
      DIMENSION etime(max_gains), lprint(max_gains), alpha(max_gains),
     &       rscale(max_gains), isubwb(max_gains), lno(max_gains),
     &       ntgp(max_gains), iaddwb(max_gains), tadd(max_gains),
     &       tmult(max_gains), winlen(max_gains), indextgp(max_gains),
     &       mlists(max_gains),v(max_gains)
      SAVE
      DATA first/.TRUE.,.TRUE.,.TRUE./, mlists/0,0,0/
c
c
      IF( ibuf(itridptr) .EQ. 2 ) RETURN                                ! forget dead traces
   10 IF( mlists(num_gains) .EQ. 0 ) THEN
          CALL podisc( igunit(num_gains), 1, 0 )                        ! get the first parameter list from disk
          CALL rddisc( igunit(num_gains), params1, ngwrds(num_gains),
     &        istat )
          mlists(num_gains) = mlists(num_gains) + 1
          gaintype(num_gains) = lparams1(1)
          etime(num_gains) = params1(2)
          lprint(num_gains) = lparams1(3)
          alpha(num_gains) = params1(4)
          rscale(num_gains) = params1(5)
          isubwb(num_gains) = lparams1(6)
          fno(num_gains) = lparams1(7)
          lno(num_gains) = lparams1(8)
          ntgp(num_gains) = lparams1(9)
          iaddwb(num_gains) = lparams1(10)
          tadd(num_gains) = params1(11)
          tmult(num_gains) = params1(12)
          winlen(num_gains) = params1(13)
          v(num_gains) = params1(14)
          IF( ntgp(num_gains) .GT. 0 ) THEN
c****         save tgp in the apsimulator at ap(indextgp)
              CALL inap( buf(numhdr+1), numdat )
              indextgp(max_gains) = nextad
              CALL rddisc( igunit(num_gains), a(indextgp(max_gains)),
     &               ntgp(num_gains), istat)
              nextad = nextad + ntgp(num_gains)
          ENDIF
      ENDIF
      IF( lbuf(7) .EQ. 0 ) THEN
          no = lbuf(3)
          itrno = lbuf(4)
      ELSE
          no = lbuf(6)
          itrno = lbuf(7)
      ENDIF
      IF( no .LT. fno(num_gains) ) THEN
          IF( mlists(num_gains) .EQ. 1 ) RETURN
          mlists(num_gains) = 0
          GOTO 10
      ENDIF
      IF( no .GT. lno(num_gains) ) THEN
          IF( mlists(num_gains) .EQ. nglists(num_gains) ) RETURN
          CALL rddisc( igunit(num_gains), params1, ngwrds(num_gains),
     &        istat )
          mlists(num_gains) = mlists(num_gains) + 1
          gaintype(num_gains) = lparams1(1)
          etime(num_gains) = params1(2)
          lprint(num_gains) = lparams1(3)
          alpha(num_gains) = params1(4)
          rscale(num_gains) = params1(5)
          isubwb(num_gains) = lparams1(6)
          fno(num_gains) = lparams1(7)
          lno(num_gains) = lparams1(8)
          ntgp(num_gains) = lparams1(9)
          iaddwb(num_gains) = lparams1(10)
          tadd(num_gains) = params1(11)
          tmult(num_gains) = params1(12)
          winlen(num_gains) = params1(13)
          IF( ntgp(num_gains) .GT. 0 )
     &        CALL rddisc( igunit(num_gains), a(indextgp(max_gains)),
     &             ntgp(num_gains), istat)
      ENDIF
      IF( ibuf(itridptr) .EQ. 2 ) RETURN                                ! don't bother with dead traces
c      nsamps = ibuf(isampptr)
      nsamps = numdat
      delay = buf(ldelsptr)
      si = buf(lsisptr)                                                 ! sample interval in seconds
      wbt = buf(lwbtsptr)                                               ! water bottom time in seconds
      IF( IAND(lprint(num_gains),2) .NE. 0 ) THEN
          PRINT *,' gaintype=',gaintype(num_gains),' etime =',
     &         etime(num_gains),' alpha=',alpha(num_gains)
         PRINT *,' nsamps=',nsamps,' delay=',delay,' si=',si,' wbt=',wbt
          PRINT *,' tadd=',tadd(num_gains),' tmult=',tmult(num_gains),
     &         ' iaddwb=',iaddwb(num_gains),' v=',v(num_gains)
      ENDIF
      IF( in .GT. 1 ) THEN
          PRINT *,' SIOSEIS error in gain.  Ap simulator: in=',in
          STOP
      ENDIF
      IF( isubwb(num_gains) .NE. 0 .AND. wbt .LE. 0 ) THEN
          ibuf(itridptr) = 2
          RETURN
      ENDIF

c****
c****  The USGS time (millisecond) method
c****
      IF( gaintype(num_gains) .EQ. 1 ) THEN
c     assume the sample interval remains constant.
c     assume no deep water delay
          IF( first(num_gains) ) THEN
              stime = delay
              IF( etime(num_gains) .LT. 0. )
     &            etime(num_gains) = stime + nsamps * si
              stimemil = stime * 1000.
              etimemil = etime(num_gains) * 1000.
c             save the gain in the ap so we don't recompute it every time
c             subsequent traces may not exceed this length
              CALL inap( buf(numhdr+1), nsamps )
              igain = nextad
              nextad = nextad + nsamps
              first(num_gains) = .FALSE.
          ENDIF
          IF( iuseap .NE. 0 .AND. in .NE. 0 ) THEN
              PRINT *,' ***  ERROR  ***  type 1 gain can not use AP.'
              STOP
          ENDIF
          IF( delay .NE. stime ) THEN
              PRINT *,' ***  ERROR  ***  type 1 gain requires all data',
     &           ' to have the same start time.'
              STOP
          ENDIF
c          idelay = ibuf(idelmptr)                                       ! the delay in mils
          dt = REAL(ibuf(isiptr)) / 1000.                              ! the sample interval in mils
          timmax = (delay + nsamps*si) * 1000.
          IF( IAND(lprint(num_gains),2) .NE. 0 ) THEN
              PRINT *,' igain=',igain,' dt=',dt,' sm=',stimemil,' em=',
     &          etimemil,' nsamps=',nsamps,' timmax=',timmax,' alpha)=',
     &          alpha(num_gains)
          ENDIF
          IF( in .EQ. 0 ) THEN                                          ! is it in the ap simulator?
              CALL pgain( buf(numhdr+1), a(igain), dt, nsamps,
     &           stimemil, etimemil, timmax, alpha(num_gains) )
          ELSE
              CALL pgain( a(1), a(igain), dt, nsamps,
     &           stimemil, etimemil, timmax, alpha(num_gains) )
          ENDIF
          IF( IAND(lprint(num_gains),4) .NE. 0)
     &        PRINT *,(a(i),i=igain,igain+nsamps)
          GOTO 9000
      ENDIF
c****
c****  The OSU range scaling method
c****
      IF( gaintype(num_gains) .EQ. 2 ) THEN
          range = lbuf(ldisptr)
          IF( ABS(range) .LT. rscale(num_gains) ) GOTO 9000
          scalar = ( ABS(range) / SIGN(rscale(num_gains),range)) **
     &            alpha(num_gains)
          IF( IAND(lprint(num_gains),2) .NE. 0 )
     &        PRINT *,' scalar=',scalar
          IF( in .EQ. 0 ) THEN                                          ! is it in the ap simulator?
              DO 200 i = 1, nsamps
  200         buf(numhdr+i) = buf(numhdr+i) * scalar
          ELSE
              DO 210 i = 1, nsamps
  210         a(i) = a(i) * scalar
          ENDIF
          GOTO 9000
      ENDIF
c****
c****     TYPE 3
c****
      IF( gaintype(num_gains) .EQ. 3 ) THEN
          time = delay
          n = nsamps
          index = 1
          IF( isubwb(num_gains) .NE. 0 ) time = delay - wbt
          time = time * tmult(num_gains) + tadd(num_gains)
          IF( in .EQ. 0 ) THEN                                          ! is it in the ap simulator?
              DO i = 0, n-1
                 temp = time + FLOAT(i) * si
                 IF( temp .LE. si ) temp = si                           ! don't kill the first sample!
                 buf(numhdr+index+i) = buf(numhdr+index+i) * temp **
     &              alpha(num_gains)
              ENDDO
          ELSE
              DO i = 0, n-1
                 temp = time + FLOAT(i) * si
                 IF( temp .LE. 0. ) temp = si
                 a(index) = a(index) * temp ** alpha(num_gains)
                 index = index + 1
              ENDDO
          ENDIF
          GOTO 9000
      ENDIF
c****
c****     TYPE 4
c****
      IF( gaintype(num_gains) .EQ. 4 ) THEN
          IF( in .EQ. 0 ) THEN                                          ! is it in the ap simulator?
              DO i = 1, nsamps
                 buf(numhdr+i) = buf(numhdr+i) ** alpha(num_gains)
              ENDDO
          ELSE
              DO i = 1, nsamps
                 a(i) = a(i) ** alpha(num_gains)
              ENDDO
          ENDIF
          GOTO 9000
      ENDIF
c****
c****     TYPE 5
c****
      IF( gaintype(num_gains) .EQ. 5 ) THEN
          time = delay
          istart = 1
          IF( isubwb(num_gains) .NE. 0 ) THEN
              time = -wbt + delay
c   remember that time 0 is the first sample, so add 1 sample
              istart = (wbt - delay) / si + 1 + 1
          ENDIF
          IF( IAND(lprint(num_gains),2) .NE. 0 ) PRINT *,
     &        ' gains type 5: istart ',istart,
     &        ' alpha = ',alpha(num_gains),' time=',time,' si=',si
          IF( istart .LT. 10 ) THEN
              IF( isubwb(num_gains) .NE. 0 ) THEN
                  IF( wbt .EQ. 0 ) PRINT *,
     &' ***  WARNING  ***  Use process WBT parameter VEL to get time.'
c                  IF( wbt .NE. 0 ) PRINT *,
                  IF( wbt .NE. 0 ) THEN
                      ibuf(15) = 2
                      PRINT *,
     &' ***  WARNING  ***  Gains has a bad water bottom time of ',wbt
                      RETURN
                  ENDIF
              ENDIF
              IF( istart .LT. 1 ) THEN
                  PRINT *,' ***  ERROR  ***  GAINS: bad wbt of ',wbt,
     &                ' or bad delay of ',delay
                  istart = 1
              ENDIF
          ENDIF
          index = in + istart - 1
          IF( in .EQ. 0 ) THEN                                          ! is it in the ap simulator?
              DO i = istart, nsamps
                 temp = (time + FLOAT(i-1) * si) * tmult(num_gains) +
     &                  tadd(num_gains)
                 buf(numhdr+i) =
     &                buf(numhdr+i) * EXP(alpha(num_gains)*temp)
              ENDDO
          ELSE
              DO i = istart, nsamps
                 temp = (time + FLOAT(i-1) * si) * tmult(num_gains) +
     &                  tadd(num_gains)
                 a(index) = a(index) * EXP(alpha(num_gains)*temp)
                 index = index + 1
              ENDDO
          ENDIF
          GOTO 9000
      ENDIF
c****
c****     TYPE 6
c****
      IF( gaintype(num_gains) .EQ. 6 ) THEN
          IF( in .EQ. 0 ) THEN                                          ! is it in the ap simulator?
              DO 600 i = 1, nsamps
  600            buf(numhdr+i) =
     &              SIGN(buf(numhdr+i)**alpha(num_gains),buf(numhdr+i))
          ELSE
              DO 610 i = 1, nsamps
  610            a(i) = SIGN(a(i)**alpha(num_gains), a(i))
          ENDIF
          GOTO 9000
      ENDIF
c****
c****     TYPE 7 -  Complex Modulus
c****
      IF( gaintype(num_gains) .EQ. 7 ) THEN
          IF( in .EQ. 0 ) THEN                                          ! is it in the ap simulator?
              DO 700 i = 1, nsamps/2
  700            buf(numhdr+i) = SQRT(buf(numhdr+i*2-1)**2
     &                         +      buf(numhdr+i*2)**2)
          ELSE
              DO  i = 1, nsamps/2
                 j = i + i
                 a(i) = SQRT(a(j-1)*a(j-1)+a(j)*a(j))
c	 print *,i,j,a(i),a(j-1),a(j)
              ENDDO
          ENDIF
c****     the sample interval is twice as big and the number of samples is half
          buf(lsisptr) = si * 2.
          ibuf(isiptr) = NINT(buf(lsisptr) * 1000000.)
          numdat = numdat / 2
          CALL long2ushort( numdat, ibuf(isampptr) )
c          IF( numdat .LT. 32768 ) THEN
c              ibuf(isampptr) = numdat
c          ELSE
c              lbuf(29) = numdat
c          ENDIF
c          ibuf(idelmptr) = ibuf(idelmptr) / 2
c          buf(ldelsptr) = buf(ldelsptr) / 2.
          GOTO 9000
      ENDIF
c****
c****  Type 8   - another range dependent scalar
c****
      IF( gaintype(num_gains) .EQ. 8 ) THEN
          range = lbuf(ldisptr)
          IF( ABS(range) .LT. rscale(num_gains) ) GOTO 9000
          scalar = ABS(range / rscale(num_gains) ) ** alpha(num_gains)
          IF( IAND(lprint(num_gains),2) .NE. 0 )
     &        PRINT *,' GAINS type 8:  scalar=',scalar
          IF( in .EQ. 0 ) THEN                                          ! is it in the ap simulator?
              DO i = 1, nsamps
                 buf(numhdr+i) = buf(numhdr+i) * scalar
              ENDDO
          ELSE
              DO  i = 1, nsamps
                  a(i) = a(i) * scalar
              ENDDO
          ENDIF
          GOTO 9000
      ENDIF
c****
c****  Type 9 - TGP    time-gain-pairs
c****
c**** There's no spatial variation.
c**** The only way it changes is if ADDWB is used and the depth changes.
c**** or the deep water delay changes.  Screw it. Make a new table
c**** for every trace.
      IF( gaintype(num_gains) .EQ. 9 ) THEN
          time = delay
          IF( isubwb(num_gains) .NE. 0 ) time = delay - wbt
          itgp = 1
          IF( IAND(lprint(num_gains),2) .NE. 0 )
     &        PRINT *,' GAINS type 9:  time=',time
c****     the tgp is in the ap at a(indextgp)
          DO i = 1, nsamps
  910        CONTINUE
             t1 = a(indextgp(max_gains)+itgp-1)
             IF( iaddwb(num_gains) .NE. 0 ) t1 = t1 + wbt
             g1 = a(indextgp(max_gains)+itgp)
             t3 = a(indextgp(max_gains)+itgp+1)
             IF( iaddwb(num_gains) .NE. 0 ) t3 = t3 + wbt
             g3 = a(indextgp(max_gains)+itgp+2)
             IF( time .LE. t1 .OR. itgp+2 .GT. ntgp(num_gains) ) THEN
                 scr(i) = g1
             ELSEIF( time .GT. t3 .AND. itgp+2.LE.ntgp(num_gains)) THEN
                 itgp = itgp + 2
                 GOTO 910
             ELSE
                 scr(i) = g3 - (g3-g1)*(t3-time)/(t3-t1)
             ENDIF
c	 print *,' time=',time,' scr=',scr(i),' t1=',t1,' wbt=',wbt
             IF( isubwb(num_gains) .NE. 0 ) THEN
                 time = (delay - wbt) + FLOAT(i) * si
             ELSE
                 time = delay + FLOAT(i) * si
             ENDIF
          ENDDO
          IF( in .EQ. 0 ) THEN                                          ! is it in the ap simulator?
              DO i = 1, nsamps
                 buf(numhdr+i) = buf(numhdr+i) * scr(i)
              ENDDO
          ELSE
              DO i = 0, nsamps-1
                 a(in+i) = a(in+i) * scr(i+1)
              ENDDO
          ENDIF
          GOTO 9000
      ENDIF
c****
c****  Type 10 -   20log(t*v)   - spherical spreading 20log(R)
c****
      IF( gaintype(num_gains) .EQ. 10 ) THEN
          IF( etime(num_gains) .GE. 0. ) THEN
              end_time = etime(num_gains)
              IF( iaddwb(num_gains) .NE. 0 ) end_time = end_time + wbt
              itemp = end_time / si + 1
              nsamps = MIN0(nsamps,itemp)
          ENDIF
c****     nsamps is set to numdat or etime
c****     Anything after etime gets the same as etime
          DO i = 1, numdat
             time = delay + FLOAT(i-1) * si
             r = time * v(num_gains)
             IF( r .GT. 10. ) THEN
                 IF( i .LE. nsamps ) tl = 20. * log10(r)
                 IF( in .EQ. 0 ) THEN                                          ! is it in the ap simulator?
                     buf(numhdr+i) = buf(numhdr+i) * tl
                 ELSE
                     a(in+i-1) = a(in+i) * tl
                 ENDIF
              ENDIF
          ENDDO
          GOTO 9000
      ENDIF
c****
c**** All gains should come here.  Do post gain processing.
c**** e.g. WINLEN is an amplitude running average.
c****
 9000 IF( winlen(num_gains) .NE. 0. ) THEN
c****     read the data from the ap and write it to trace buffer
          CALL inap( buf(numhdr+1), numdat )
          iwinlen = NINT(winlen(num_gains) / si) + 1
          middle = iwinlen / 2
          sum = a(in)
          DO i = 2, iwinlen
             sum = sum + a(in+i-1)
             ave = sum / FLOAT(i)
             middle = (i+1) / 2
             buf(numhdr+middle) = ave
          ENDDO
          middle = iwinlen / 2
          rnum = FLOAT(iwinlen)
          DO i = iwinlen, nsamps-1
             sum = sum - a(in+i-iwinlen) + a(in+i)
             ave = sum / rnum
             buf(numhdr-middle+i+1) = ave
          ENDDO
          DO i = 1, middle
             sum = sum - a(in+nsamps-iwinlen+i-1)
             ave = sum / FLOAT(iwinlen-i)
             buf(numhdr+nsamps-middle+i) = ave
          ENDDO
          in = 0
      ENDIF
c****
c****
      RETURN
      END
