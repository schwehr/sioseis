      SUBROUTINE WBTEX(LBUF,BUF,IBUF, scr, iwbtyp,nready,istop, numwbt )
C     WBTEX PUTS THE USER DEFINED WATER BOTTOM TIME IN TRACE HEADER WORD 50.
C  WBTEX IS THE EXECUTION PHASE OF THE SEISMIC REFLECTION PROCESS WBT.  THE
C  TIME IN THE HEADER IS IN FLOATING POINT SECONDS.  THE USER'S WATER BOTTOM
C  TIMES MUST BE IN DISC FILE IWUNIT (SEE SUBROUTINE WBTED) AND MUST BE BY RP
C  NUMBERS AND THOSE NUMBERS MUST BE INCREASING ORDER WITHIN THHE DISC FILE.
C
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, MARCH 1980
C  SOME OF THE AWFUL GYRATIONS WITH MULTIPLYING THE DAY/GMT IS DUE TO A
C  COMPILER BUG ON THE PRIME.
c  mod 24 July 90 to make the rp numbers and gmt floating point so that
c      smoother interpolation can be done.
c  mod 2 aug 90 - remove the prime gyrations discussed above!
c    mod 21 Sep 95 - Add SEL, SES, SOLRAT - an auto picker.
c                  - Remove TYPE
c  mod 29 Jan 96 - Add (ses-delay) to the time of the pick!
c  mod 7 Feb 96 - Convert GMT to decimal minute before interpolation
c  mod 24 Apr 96 - Do not modify trace header delay when auto picking!
c  mod 11 June 96 - Was using lrptrptr rather than lrpnptr!  When was
c                 that change made?  Damn
c  mod 6 June 97 - Add threshold picking
c  mod 22 Oct 98 - Add PEAK picker
c  mod May 99 - Add SEPP (start and end time peak picker)
c  mod 26 Apr 00 - PRESTK didn't work with THRES or PEAK
c  mod June 00 - g77 doesn't like arithmetic inside FLOAT
c  mod 21 Jun 00 - Added INDEX
c  mod June 01 - Add numwbt and make it re-enterable.
c  mod 31 Jul 01 - On THRES, use the time of the last good pick
c                  if this pick doesn't happen.
c  mod 3 Aug 01 - Add parameter TRACK to SEPP and THRES
c  mod 8 Jan 03 - Change slave.f to skip the short window if it's 0.
c  mod 9 Sep 05 - Honor the scalar in segy bytes 69-70 (word 35)
c               - Eliminate old Seabeam scanning by making NSCAN and VEL needed.
c               - VEL only mean simply convert depth to time using vel.
c  mod 22 Feb 08 - Honor track when doing VEL.
c  mod 23 Jun 08 - When doing vel, check for unrealistic depths.
c  mod 27 Jun 08 - When doing vel, check for byte swapped water depth (Knudsen).
c  mod 18 Nov 08 - When doing vel, kill the trace if we haven't seen a non-zero
c                  depth.  (o.w. use last non-zero depth).
c  mod 3 Sep 09 - When vel and track, count nonzero and determine bad only
c                 after 5(?) nozero depths have occurred.
c  mod 5 Oct 09 - Add offline bandpass filtering.
c  mod 25 Nov 09 - Change prestk to be shots as well as rps.
c                - Do the prestk logic early so that it applies to all methods
c  mod 31 Mar 10 - get rid of arithmetic IF
c  mod 13 Aug 10 - The offline filter changes caused procs filter wbt to be bad.
c  mod 8 Jun 12 - Honor prestk when using vel.
c  mod 10 Jul 14 - Add guided & seg
c  mod 7 Oct 14 - get si from common rather than trace header
c  mod 20 Oct 14 - get delay from common since rev 1 uses a scalar
C
      PARAMETER (max = 100)
      PARAMETER (MAXWBTS=3)
      PARAMETER (NPTS = 55)
      DIMENSION BUF(1111),LBUF(1111),IBUF(1111), scr(*)
      INTEGER*2 IBUF
      REAL l, l1, l2, lastrp
      DIMENSION sbdep(max), iaddr(max), filpts(NPTS)
      DIMENSION ldummy(4)
      EQUIVALENCE (l1,ldummy(1)), (time1,ldummy(2))
      EQUIVALENCE (l2,ldummy(3)), (time2,ldummy(4))
      CHARACTER*80 cscr
      SAVE
      INTEGER prestk
      COMMON /WBOT/ IWUNIT(MAXWBTS), lday(MAXWBTS), maxwrd(MAXWBTS),
     &  lprint(MAXWBTS), loffst(MAXWBTS), vel(MAXWBTS), nscan(MAXWBTS),
     &  sol(MAXWBTS), sel(MAXWBTS,2), ses(MAXWBTS,2), prestk(MAXWBTS),
     &  thres(MAXWBTS), peak(MAXWBTS), sep(MAXWBTS),
     & sepp(MAXWBTS,2), jndex(MAXWBTS), track(MAXWBTS), pass(MAXWBTS,2),
     &  guide(MAXWBTS), seg(MAXWBTS,2)
      INTEGER lastno(MAXWBTS)
      REAL picktime(MAXWBTS), picklast(MAXWBTS)
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr,
     *               nrskip, nfiles, irewind, delay, segyrev, si
      COMMON /sioap/ iasgnd,irelse,in,iout,nextad
      LOGICAL FIRST(MAXWBTS)
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr, lsmusptr, lemusptr,
     *                 lsisptr,  lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr,   ifoldptr, icvleptr, lespnptr, ilagaptr,
     *                 ilagbptr
      COMMON /apmem/ a(32768)
      DATA FIRST/MAXWBTS*.TRUE./, LASTRP/-32768/, ndone/0/, ntogo/0/,
     &  jprint/0/,lastno/MAXWBTS*-1/, picklast/MAXWBTS*-99./, depth1/0/,
     &   nnonzero/0/, nfilpts/0/
C
      iwbtyp = 0
      nready = 1
      IF( vel(numwbt) .NE. 0. ) GOTO 1000
      IF( sol(numwbt) + thres(numwbt) + peak(numwbt) + sepp(numwbt,1) +
     &    sepp(numwbt,2) + guide(numwbt) .NE. 0 ) GOTO 2000 
      L = LBUF(lrpnptr)                                                 ! THE RP NUMBER OF THIS TRACE
      IF( LDAY(numwbt) .NE. 0 ) THEN
          rmin = REAL(ibuf(80)) * 24. * 60. +
c     &           REAL(ibuf(81) * 60. + ibuf(82)) + REAL(ibuf(83))/60.
     &           REAL(ibuf(81)) * 60. + REAL(ibuf(82)) +
     &           REAL(ibuf(83))/60.
          l = ibuf(80)*10000.+IBUF(81)*100.+IBUF(82)+ibuf(83)/60.
      ENDIF
      IF(L.EQ.LASTRP) GO TO 100                                         ! IS THIS RP THE SAME AS THE LAST RP
      IF(.NOT.FIRST(numwbt)) GO TO 200
      FIRST(numwbt)=.FALSE.
      IF(LBUF(6)+LBUF(7).NE.0.OR.LDAY(numwbt).NE.0) GO TO 50
      PRINT 10
   10 FORMAT(' ***  ERROR  ***  WBT REQUIRES SHOT/RP DATA TO HAVE ',
     *  'PROCESS GEOM APPLIED FIRST.')
      STOP
   50 CALL podisc( iwunit, 1, 0 )                                       !  GET THE FIRST PARAMETER LIST FROM THE DISC
      CALL rddisc( iwunit, ldummy, 2, istat )
      IEND=0
      L2=L1
      TIME2=TIME1
      IF( LDAY(numwbt) .NE. 0 ) IDAY = L1/10000                         ! MAKE THE FIRST WBT BE ON THE RIGHT DAY
      IF(L.GT.L1) GO TO 250
   80 TIME=TIME1                                                        !  THE RP IS BEFORE THE USER'S FIRST GIVEN RP
   90 LASTRP=L
  100 BUF(jndex(numwbt))=TIME
      IF( IAND(lprint(numwbt),2) .NE. 0 ) PRINT *,' wbt = ',
     &    BUF(jndex(numwbt))
      LASTL2=L2
      RETURN
  200 CONTINUE
c      IF(L-L1)50,80,250
      IF( l - l1 .LT. 0 ) GOTO 50
      IF( l - l1 .EQ. 0 ) GOTO 80
  250 IF(L.GT.L2) GO TO 300
C    INTERPOLATE THE TIME IF BETWEEN TWO GIVEN BY THE USER
      IF( lday(numwbt) .EQ. 0 ) THEN
c         g77 didn't like the FLOATs on the next statement because
c         l, l1 and l2 are already REAL
c          TIME = FLOAT(L-L1)/FLOAT(L2-L1)*(TIME2-TIME1)+TIME1
          TIME = (L-L1)/(L2-L1)*(TIME2-TIME1)+TIME1
      ELSE
          itempday = l1 / 10000
          itemphr = (l1 - itempday * 10000) / 100
          itempmin = l1 - itempday * 10000 - itemphr * 100
          rmin1 = FLOAT(itempday) * 24. * 60. +
     &            FLOAT(itemphr) * 60. + FLOAT(itempmin)
          itempday = l2 / 10000
          itemphr = (l2 - itempday * 10000) / 100
          itempmin = l2 - itempday * 10000 - itemphr * 100
          rmin2 = FLOAT(itempday) * 24. * 60. +
     &            FLOAT(itemphr) * 60. + FLOAT(itempmin)
          time = (rmin-rmin1) / (rmin2-rmin1)*(TIME2-TIME1)+TIME1
      ENDIF
      GO TO 90
  300 IF(IEND.EQ.1) GO TO 410                                           ! ANY MORE LISTS
      L1=L2                                                             !  GET THE NEXT PARAMETER LIST
      TIME1=TIME2
      CALL rddisc( iwunit, ldummy(3), 2, istat )
      IF( istat .EQ. 2 ) GO TO 250                                      ! EOF?
      IEND=1                                                            !  NO MORE LIST ON THE DISC
  410 TIME=TIME2                                                        ! USE THE LAST TIME
      GO TO 90
c****
c****      SIO SeaBeam water bottom depth
c****
 1000 CONTINUE
      depth = lbuf(16)                                                  ! depth at source
      IF( ibuf(35) .NE. 0 ) THEN
          temp = REAL(ibuf(35))                                         ! SEGY scalar
          IF( temp .LT. 0 ) THEN
              depth = depth / (-temp)
          ELSE
              depth = depth * temp
          ENDIF
      ENDIF
c      depth = ibuf(107)
c     a zero depth means we're hard aground
      IF( depth .EQ. 0 ) THEN
c****     kill the trace if there haven't been any good depths yet
          IF( depth1 .EQ. 0 ) THEN
          PRINT *,' ***  WARNING  ***  Zero water depth, killing shot',
     &        lbuf(3)
              ibuf(15) = 2
          ENDIF
          depth = depth1                                                ! We get a 0 depth from Seabeam occasionally
      ELSE
          nnonzero = nnonzero + 1
      ENDIF
      IF( depth .LT. 0 .OR. depth .GT. 10000) THEN
c****   Knudsen KEB files have a byte swap bug, so see if that's the problem
          ltemp = lbuf(16)
          CALL swap32(ltemp,1)
          IF( temp .LT. 0 ) THEN
              depth = FLOAT(ltemp) / (-temp)
          ELSE
              depth = FLOAT(ltemp) * temp
          ENDIF
          IF( depth .LT. 0 .OR. depth .GT. 10000 ) THEN
              PRINT *, ' ***  WARNING  ***  Unrealistic water depth.'
              depth = 0
          ELSE
              lbuf(16) = ltemp
          ENDIF
      ENDIF
c****   When prestack, use the depth from the first trace for all others in the shot/rp
      IF( prestk(numwbt) .NE. 0 ) THEN
          itraceno = lbuf(7)
          IF( lbuf(7) .EQ. 0 ) itraceno = lbuf(4)
          IF( itraceno .NE. 1 ) depth = depth1     ! use the previous depth
      ENDIF
      IF( track(numwbt) .NE. 99. .AND. depth1 .NE. 0 .AND. 
     &    nnonzero .GT. 5 ) THEN
          IF( ABS(depth-depth1)*2./vel(numwbt) .GT. track(numwbt) ) 
     &        depth = depth1
      ENDIF
c****  Knudsen also gives zero when it can't find the water botton.
c****  Use the last good one if this is zero.
      depth1 = depth
      buf(jndex(numwbt)) = depth / (vel(numwbt) / 2.)
      IF( IAND(lprint(numwbt),2) .NE. 0 ) PRINT *,' shot ',lbuf(3),
     &   ' trace ',lbuf(4),' depth=',depth,' time= ',buf(jndex(numwbt))
      IF( nscan(numwbt) .EQ. 0 ) RETURN
      nready = 0
      iwbtyp = 2
      IF( ntogo .NE. 0 .OR. istop .EQ. -1 ) GOTO 1040                   !ntogo is nonzero when we should just get rid of the traces on disk
      IF( depth .EQ. 0 ) depth = 99999.
      IF( first(numwbt) ) THEN
          first(numwbt) = .FALSE.
          vel(numwbt) = vel(numwbt) / 2.
          nscan2 = nscan(numwbt) / 2 + 1
          DO 1010 i = 1, max
 1010     sbdep(i) = 99999.
          CALL getfil( 1, lun, cscr, istat )                            ! get a disk file to hold the traces
          iaddr(1) = 1
          DO 1030 i = 1, nscan2
             CALL wrdisc( lun, buf, maxwrd(numwbt) )                    ! some computers insist that you write before positioning!
             iaddr(i) = iaddr(i-1) + maxwrd(numwbt)
 1030     CONTINUE
      ENDIF
      ndone = ndone + 1
      n = nscan(numwbt)
      IF( ndone .LT. nscan(numwbt) ) n = ndone 
      sbdep(n+loffst(numwbt)) = depth
      n2 = nscan2
      IF( ndone .LT. nscan2 ) n2 = ndone
      laddr = iaddr(n2)
      CALL podisc( lun, 1, laddr )                                      ! write this trace to disk
      nwrds = numhdr + numdat
      iout = 0                                                          ! getthe data out of the ap
      CALL rlseap( buf(numhdr+1), numdat)
      CALL wrdisc( lun, buf(1), nwrds)
      IF( ndone .LT. nscan2 ) THEN                                      ! can we output a trace?
          nready = 0
          RETURN
      ENDIF
 1040 CONTINUE
      depth = sbdep(1)
      DO 1050 i = 2, nscan(numwbt)                                      ! find the minimum depth
         IF( sbdep(i) .LT. depth ) depth = sbdep(i)
 1050 CONTINUE
      laddr = iaddr(1)                                                  ! get ready to get rid of a trace
      CALL podisc( lun, 1, laddr )
      CALL rddisc( lun, buf, numhdr, istat )
      numdat = ibuf(58)
      CALL rddisc(lun, buf(numhdr+1), numdat, istat )
      buf(jndex(numwbt)) = depth / vel(numwbt)
      IF( IAND(lprint(numwbt),2) .NE. 0 ) THEN
          PRINT *,' shot ',lbuf(3),' tr ',lbuf(4),' wbt=',
     &        buf(jndex(numwbt))
      ENDIF
      DO 1100 i = 2, nscan2                                             ! rotate the disk address table
 1100 iaddr(i-1) = iaddr(i)
      iaddr(nscan2) = laddr
      IF( ndone .GE. nscan(numwbt) ) THEN
          DO 1110 i = 2, nscan(numwbt)+loffst(numwbt)
 1110     sbdep(i-1) = sbdep(i)
      ENDIF
      IF( ntogo .NE. 0 ) THEN                                           ! nonzero only after last trace has been sent in
          ntogo = ntogo - 1
          nready = nready - 1
          IF( ntogo .EQ. 0 ) istop = 1
          RETURN
      ELSE
          nready = 1
      ENDIF
      IF( istop .NE. 0 .AND. ntogo .EQ. 0 ) THEN
          ntogo = nscan2 - 1
          nready = nscan2
          istop = 0
      ENDIF
      RETURN
c****
c****  Auto pickers
c****
 2000 CONTINUE
c      delay = buf(ldelsptr)
c      si = buf(lsisptr)
      IF( guide(numwbt) .EQ. 0 ) buf(jndex(numwbt)) = 0.
      IF( ibuf(itridptr) .EQ. 2 ) RETURN
      no = lbuf(3)
      itrno = lbuf(4)
      IF( lbuf(7) .NE. 0 ) THEN
          no = lbuf(6)
          itrno = lbuf(7)
      ENDIF
c     if prestack and this trace belongs to the same rp/shot as the last
c     then use the wbt from the previous trace!
c     index was saved! delay and si may have changed!
      IF( prestk(numwbt) .NE. 0 ) THEN
          IF( lastno(numwbt) .EQ. no )THEN
              buf(jndex(numwbt)) = picktime(numwbt)
              RETURN
          ENDIF
      ENDIF
c****
c****  Put all the data into a(nextad) then filter it if requested
c****
c**** force use of the "ap" so it's memory gets allocated
      CALL inap( buf(numhdr+1), numdat )
c**** get the data out of the ap into scr array a(nextad)
      iout = 0
      CALL rlseap( a(nextad), numdat )
c**** that's a problem.  rlseap sets the data to not being in the ap so the
c**** next process thinks the data is in buf, but's it's not since we put
c**** it in a(nextad).  Put it back in a(in).
      CALL inap( a(nextad), numdat )
      IF( pass(numwbt,2) .NE. 0. ) THEN
          IF( nfilpts .EQ. 0 ) THEN
              CALL bpass( pass(numwbt,1), pass(numwbt,2), filpts,
     &             NPTS, si, scr )
              nfilpts = NPTS
          ENDIF
          ldummy(1) = 1
          ldummy(2) = numdat
          CALL tvfilt( a(nextad), scr, a(nextad+numdat), filpts,
     &         nfilpts, 1., ldummy, 1, numdat )
          DO i = 1, numdat
             a(nextad+i-1) = scr(i)
          ENDDO
      ENDIF
      IF( sol(numwbt) .NE. 0 ) GOTO 2100 
      IF( thres(numwbt) .NE. 0 ) GOTO 3000 
      IF( peak(numwbt) .NE. 0. ) GOTO 4000
      IF( guide(numwbt) .NE. 0. ) GOTO 5000
      PRINT *,' ***  PROGRAM ERROR   ***   WBT'
      STOP
c****
c****  Short/Long average picker
c****
 2100 CONTINUE
      istartl = NINT((sel(numwbt,1)-delay) / si) + 1
      IF( istartl .LT. 1 ) istartl = 1
      nlong =  NINT((sel(numwbt,2) - sel(numwbt,1)) / si)
      istarts = NINT((ses(numwbt,1)-delay) / si) + 1
      IF( istarts .LT. 1 ) istarts = 1
      nshort = NINT((ses(numwbt,2) - ses(numwbt,1)) / si )
      jprint = IAND(lprint(numwbt),4)
      iadds = 0
 2005 CONTINUE
c      IF( in .NE. 0 ) THEN
c          CALL slave( a(in+istartl-1), nlong, a(in+istarts+iadds-1),
c     &        nshort, numdat, sol(numwbt), index, aves, avel, jprint )
c      ELSE
c          CALL slave( buf(numhdr+istartl), nlong, 
c     &                buf(numhdr+istarts+iadds), nshort,
c     &                numdat, sol(numwbt), index, aves, avel, jprint )
c      ENDIF
       CALL slave( a(nextad+istartl), nlong, 
     $             a(nextad+istarts+iadds), nshort,
     &             numdat, sol(numwbt), index, aves, avel, jprint )
 2010 IF( index .GT. 0 ) THEN
          time = delay + FLOAT(index+istarts+iadds-1)*si
          buf(jndex(numwbt)) = time
      ELSE
          IF( avel .EQ. 0. ) THEN
              PRINT *,' No pick. The long window average is 0.'
          ELSE
              PRINT *,' No pick.  Decrease solrat.'
          ENDIF
          IF( track(numwbt) .EQ. 99. ) RETURN
          PRINT *,' Using last pick of ',picklast(numwbt)
          time = picklast(numwbt)
      ENDIF
      IF( IAND(lprint(numwbt),2) .NE. 0 ) THEN
          PRINT *,' istartl=',istartl,' nlong=',nlong,' istarts=',
     &            istarts,' nshort=',nshort,' numdat=',numdat
          IF( index .NE. 0 ) THEN
              PRINT *,' wbt pick =',buf(jndex(numwbt)),index, aves, avel
          ENDIF
      ENDIF
      picktime(numwbt) = time
      IF( track(numwbt) .NE. 99 ) THEN
          IF( picklast(numwbt) .EQ. -99. ) picklast(numwbt) = time
          IF( time .GE. picklast(numwbt) - track(numwbt) .AND.
     &            time .LE. picklast(numwbt) + track(numwbt) ) THEN
                  picklast(numwbt) = picktime(numwbt)
                  buf(jndex(numwbt)) = picktime(numwbt)
                  RETURN
          ENDIF
c**** If the pick is before the track window, try the next pick by
c**** advancing the short window to the current.
          IF( time .LE. picklast(numwbt) ) THEN
              iadds = iadds + index - nshort/2
              GOTO 2005
          ENDIF
c**** If we get here, then the pick is past the tracking window, so
c**** use the last good pick.
          picktime(numwbt) = picklast(numwbt)
          PRINT *,' ***  WARNING  ***  Pick of ',time,
     &        ' is outside the track.'
      ENDIF
      picklast(numwbt) = picktime(numwbt)
      buf(jndex(numwbt)) = picktime(numwbt)
      lastno(numwbt) = no
      RETURN
c****
c****   Pick it if it exceeds a threshold
c****
 3000 CONTINUE
      IF( thres(numwbt) .NE. 0. ) THEN
          IF( first(numwbt) ) THEN
              first(numwbt) = .FALSE.
              picklast(numwbt) = -99.
          ENDIF
          pick = 0.
          DO i = 1, numdat
             IF( ABS(a(nextad-1+i)) .GT. thres(numwbt) ) THEN
                 pick = FLOAT(i-1) 
c                 i = numdat
                 GOTO 3010
                 ENDIF
          ENDDO
 3010     CONTINUE
          IF( pick .EQ. 0. ) THEN
              PRINT *,' ***  WARNING  ***  No pick, using last.'
          ELSE
              time = delay + pick*si
              IF( picklast(numwbt) .EQ. -99. ) picklast(numwbt) = time
c****         see if this pick is within the track of the last pick
              IF( time .GE. picklast(numwbt) - track(numwbt) .AND.
     &            time .LE. picklast(numwbt) + track(numwbt) ) THEN
                  picktime(numwbt) = time
              ELSE
                  picktime(numwbt) = picklast(numwbt)
                  PRINT *,' ***  WARNING  ***  Pick of ',time,
     &                ' is outside the track.'
              ENDIF
          ENDIF
          picklast(numwbt) = picktime(numwbt)
          buf(jndex(numwbt)) = picktime(numwbt)
          IF( IAND(lprint(numwbt),2) .NE. 0) PRINT *,' picked time is ',
     &        buf(jndex(numwbt))
          RETURN
      ENDIF
c****
c****    Simple PEAK/TROUGH picker
c****
 4000 CONTINUE
      IF( first(numwbt) ) THEN
          first(numwbt) = .FALSE.
          picklast(numwbt) = -99.
      ENDIF
      IF( sep(numwbt) .NE. 0. ) THEN
          istart = NINT((sepp(numwbt,1)-delay) / si)
          IF( istart .LT. 1 ) istart = 0
          npeak =  NINT((sepp(numwbt,2) - sepp(numwbt,1)) / si)
      ELSE
          istart = 1
          npeak = numdat
      ENDIF
      npeak = istart + npeak-1
      n = istart
      pick = 0.
c**** positive peak
      IF( peak(numwbt) .EQ. 1. ) THEN
          DO i = istart-1, npeak-1
             IF( a(nextad-1+i) .GT. pick ) THEN
                  n = i
                  pick = a(nextad-1+i)
             ENDIF
          ENDDO
      ENDIF
c**** negative peak
      IF( peak(numwbt) .EQ. 2. ) THEN
          DO i = istart-1, npeak-1
             IF( a(nextad-1+i) .LT. pick ) THEN
                 n = i
                 pick = a(nextad-1+i)
             ENDIF
          ENDDO
      ENDIF
c**** absolute value peak
      IF( peak(numwbt) .EQ. 3. ) THEN
          DO i = istart-1, npeak-1
             IF( ABS(a(nextad-1+i)) .GT. pick ) THEN
                 n = i
                 pick = ABS(a(nextad-1+i))
             ENDIF
          ENDDO
      ENDIF
c**** Don't honor the pick if it was the first sample
      IF( n .EQ. istart ) RETURN
      time = delay + FLOAT(n-1) * si
      IF( picklast(numwbt) .EQ. -99. ) picklast(numwbt) = time
      IF( time .GE. picklast(numwbt) - track(numwbt) .AND.
     &    time .LE. picklast(numwbt) + track(numwbt) ) THEN
          picktime(numwbt) = time
      ELSE
          picktime(numwbt) = picklast(numwbt)
          PRINT *,' ***  WARNING  ***  Pick of ',time,
     &            ' is outside the track.'
      ENDIF
      picklast(numwbt) = picktime(numwbt)
      buf(jndex(numwbt)) = picktime(numwbt)
      lastno(numwbt) = no
      IF( IAND(lprint(numwbt),2) .NE. 0 ) PRINT 4500, 
     &    no, itrno, buf(jndex(numwbt)), pick
 4500 FORMAT (' no ',I6,' trace ',I4,' picked time is ',F8.6,
     &  ' peak is ', G20.10)
      RETURN
c****
c****  Guided - the peak or trough within window SEG which is centered around the 
c****           water bottom time in buf(jndex(numwbt))
c****
 5000 CONTINUE
c**** this is similar to 4000 above
      IF( first(numwbt) ) THEN
          first(numwbt) = .FALSE.
          picklast(numwbt) = -99.
      ENDIF
      istart = NINT((seg(numwbt,1)+buf(jndex(numwbt))-delay) / si)
      IF( istart .LT. 1 ) istart = 0
      npeak =  NINT((seg(numwbt,2) - seg(numwbt,1)) / si)
      npeak = istart + npeak-1
      n = istart
      pick = 0.
c**** positive peak
      IF( guide(numwbt) .EQ. 1. ) THEN
          DO i = istart-1, npeak-1
             IF( a(nextad-1+i) .GT. pick ) THEN
                  n = i
                  pick = a(nextad-1+i)
             ENDIF
          ENDDO
      ENDIF
c**** negative peak
      IF( guide(numwbt) .EQ. 2. ) THEN
          DO i = istart-1, npeak-1
             IF( a(nextad-1+i) .LT. pick ) THEN
                 n = i
                 pick = a(nextad-1+i)
             ENDIF
          ENDDO
      ENDIF
c**** absolute value peak
      IF( guide(numwbt) .EQ. 3. ) THEN
          DO i = istart-1, npeak-1
             IF( ABS(a(nextad-1+i)) .GT. pick ) THEN
                 n = i
                 pick = ABS(a(nextad-1+i))
             ENDIF
          ENDDO
      ENDIF
c**** Don't honor the pick if it was the first sample
      IF( n .EQ. istart ) RETURN
      time = delay + FLOAT(n-1) * si
      IF( picklast(numwbt) .EQ. -99. ) picklast(numwbt) = time
      IF( time .GE. picklast(numwbt) - track(numwbt) .AND.
     &    time .LE. picklast(numwbt) + track(numwbt) ) THEN
          picktime(numwbt) = time
      ELSE
          picktime(numwbt) = picklast(numwbt)
          PRINT *,' ***  WARNING  ***  Pick of ',time,
     &            ' is outside the track.'
      ENDIF
      picklast(numwbt) = picktime(numwbt)
      buf(jndex(numwbt)) = picktime(numwbt)
      lastno(numwbt) = no
      IF( IAND(lprint(numwbt),2) .NE. 0 ) PRINT 4500,
     &    no, itrno, buf(jndex(numwbt)), pick
      RETURN

      END
