      SUBROUTINE PLOTEX( BUF, LBUF, IBUF, SCR, LSCR, ISCR, istop )
C     PLOTEX IS THE EXECUTION PHASE OF THE SEISMIC REFLECTION PROCESS PLOT
C  (SEISMIC SECTION PLOT).  THE USER'S PARAMETERS MUST BE IN
C  DISC FILE MUNIT (IN COMMON /PLOT/) AND THE TRACE WITH TRACE HEADER
C  WILL BE IN MEMORY ARRAY BUF.
C
C  ARGUMENTS:
C  BUF    - THE TRACE TO BE PROCESSED, INCLUDING THE TRACE HEADER.  THE FIRST
C           DATA SAMPLE MUST BE AT TIME DELAY.  THIS IS THE FLOATING
C           POINT (REAL) TRACE ARRAY.
C  LBUF   - THE LONG INTEGER TRACE ARRAY.  THIS IS REALLY THE SAME AS BUF, BUT
C           PRIME FORTRAN DOESN'T ALLOW EQUIVALENCING ANYTHING TO AN ARGUMENT.
C  IBUF   - THE SHORT INTEGER TRACE ARRAY.  NEEDED FOR 16 BIT TRACE HEADER
C           ADDRESSES.
C  SCR    - A SCRATCH ARRAY FOR READING THE PARAMETERS.  THEREFORE, SCR MUST
C           BE AT LEAST 56 32BIT WORDS BIG.  SCR MAY BE DESTROYED BY THE CALLING
C           ROUTINE.
C  LSCR   - THE SAME SCRATCH ARRAY BECAUSE OF THE EQUIVALENCING PROBLEM.
C
C  COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, AUGUST 1983
C
c mod 15 Nov 89  to add annyp 8 (espn)
c mod 8 May 90 added seconds to the annotation by GMT
c mod 18 Jul 90 added anntyp 9
c mod 23 Jul 90 to correct error when stime < delay, thus the data has
c       to be shifted and zeroes inserted at the beginning of the trace.
c mod 2 aug 90 to make ftr, ltr and ninc work!
c mod Nov 90 to straighten out stime, nsecs, delay and nsamps
c mod 6 feb 91 to zero the back end of the plot when stime > delay
c Aug 91 - Add some of the OSU stuff (frange/lrange/, refplot)
c  20 Aug 91 - Add rstime
c mod 24 Jan 92 Iris-Segy where nsamps = lbuf(58) rather than ibuf(58)
c mod 30 Jan 92 - Allow negative stime by changing preset to -99999.
c mod 4 Feb 92 - Add hidden user parameter absval for special case
c              - Add hdr, ihdr, lhdr
c mod 6 June 93 - honor frange/lrange when hscale not used.
c mod Feb 95 - Add time line annotation
c mod 21 Nov 95 - When hscale and nsecs are given and nsecs > nsamps,
c                only plot nsamps.
c mod 10 Sept 96 - change GMT annotation to be day and GMT.
c mod 23 Sep 99 - Truncate the shot/rp number to 4 characters when using SH&TR
c mod 30 Mar 01 - Add lunhead - write each SEG-Y header to file on lunhead
c mod 1 May 01 - "straighten out" confusion over annotation by GMTRP and GMTSEC
c mod 18 Dec 01 - Add warnings if the plot window and data window do
c                 not overlap.
c mod 14 Oct 02 - Add anntyp2 and rectify
c mod 21 May 04 - anninc was not honored by fanno
c mod 6 Aug. 05 - Shot number of 0 was a show stopper.
c mod 29 Jul 06 - When stime is not given make it mod 10 mils
c mod 2 Aug 06 - Do NOT use fno when doing GMT because of decreasing shot numbers
c mod 12 Aug 06 - Don't try to plot the trace when the data is outside the plot
c mod 10 Oct 06 - fanno was incremented by annin before ann2 was done
c mod 14 Mar 07 - Add CHART
c mod 27 Apr 07 - HPATH file is headers only, so set number of sample to 0
c mod 8 May 07 - Add warning if CHART is given WBT needs to be given.
c mod 23 Jul 07 - set n2plot according to the plotter size when nsecs = 0.
c mod 14 Sep 07 - Add logic for CHART given but STIME is not.
c mod 23 May 08 - n2plot preset when nsecs=0 was bad
c mod 24 Jun 08 - With CHART, don't create a break too often
c               - CHART and stime < delay caused a bomb
c               - Chart with creating dead traces and annotation caused a bomb.
c                 I suspect it's a trplot bug with plot spaces and buffers.
c mod 4 Aug 08 - 23 May change made nsecs given to be ignored.
c mod 22 Aug 08 - chart stime was wrong.
c mod 19 Nov 08 - zero ddead traces AFTER getting out of the ap
c mod 13 Jan 09 - Use delay in seconds (buf(46)) so big delays work (>32k mils)
c mod 15 Aug 09 - Use NINT(delay*1000) for first time line
c mod 27 Aug 09 - Don't adjust stime to nearest 10 mils if si < 1 mil
c mod 22 Oct 10 - the 19 Nov 08 zeroed the saved trace in scr rather than buf!
c mod 28 Oct 10 - HPATH needs byte swap
c mod 23 Sep 11 - Modify the "too many plot samples" message.
C
      PARAMETER ( MAX_WBTS = 10)
      DIMENSION wbts(MAX_WBTS)
      DIMENSION BUF(*),LBUF(*),IBUF(*),SCR(*),LSCR(*),ISCR(*)
      INTEGER*2 IBUF,ISCR
      COMMON /READT/ ILUN,NUMHDR,NUMDAT,IUNHDR, ireeln, intrcs
      COMMON /PLOT/MUNIT,NLISTS,FTAG,TAGINC,FSPACE,NSPACE,SPACEI,IDIR,
     &       irecsp, lunhead, nraster
      INTEGER FTAG,TAGINC,FSPACE,SPACEI,FANNO,ANNTYP,DECIMF,ANNINC, hdr,
     &        anntyp2
      COMMON /SEISPL/ OFFSET,DEF,PCTFIL,VSCALE,TRPIN,JTYPE,TLINES(4),
     *       STIMEL,TIMEL,IUNPLT,WIGGLE,BIAS,LANN,ICDOTS,scalar,clip,
     *       itlan, irectify, ndptr, chart(2), size, itrim
      COMMON /lanno/ lanno, lanno2
      CHARACTER*8 LANNO, lanno2
      COMMON /MAXSCs/ NPRMU,RELAMP
      REAL*4 NSECS
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP,
     *     IDECIM
      COMMON /VERSAT/ NIBS,RNIBS
      INTEGER FNO,FDAY,FTR, ptype, frange, absval
      COMMON /edits/ ierror, iwarn, irun, now, icompt, isite, maxsamps,
     & nbperw, ireal
      COMMON /apmem/ a(5000000)
      LOGICAL FIRST
      SAVE
      DATA wbts/MAX_WBTS*0./, n_wbts/0/, lastspace/0/
      DATA NDONE/0/, delay/-1./, lastmn/-1/, n2plot/0/
      DATA ntnull/0/, ntlive/0/, ntother/0/, hscale/0./                 ! for OSU hscale that's not in here, but ....
      DATA FIRST /.TRUE./
c
      IF( istop .LT. 0 ) THEN
          IF( hscale .GT. 0. ) GOTO 240
          RETURN
      ENDIF
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      delay = buf(46)
      si = buf(49)
      IF(.NOT.FIRST) GO TO 90
   10 CONTINUE                                                          ! GET THE FIRST PARAMETER LIST INT0 MEMORY ARRAY SCR
      nprmu=1
      REWIND MUNIT
      READ(MUNIT) FNO,FDAY,FTR,LNO,LDAY,LTR,STIME,NSECS,VSCALE,DEF,
     *    TRPIN,RELAMP,ANNTYP,DECIMF,IHDRUN,NCOMMS,ANNINC,FANNO,ninc,
     *    hscale, plotsi, ptype, frange, lrange, rstime, absval, hdr,
     *    ihdr, lhdr, iwrap, anntyp2, ndptr
      IF( hscale .NE. 0 ) trpin = rnibs
      dist = lbuf(10)
      MLISTS=1
      IF( chart(2) .NE. 0 ) THEN
          nspace = 150
          IF( stime .LT. 0 ) THEN
              IF( buf(50) .LE. .0001 ) PRINT *,
     &' ***  WARNING  ***  CHART needs the water bottom TIME.  Use WBT.'
              IF( si .LT. .001 ) THEN
                  temp = buf(50) * 10.
                  itemp = INT(temp)
                  stime = FLOAT(itemp) / 10.
                  IF( buf(50) .GT. stime + (nsecs * chart(1)) + .05 )
     &                stime = stime + .05
                  IF( buf(50) .GT. stime + nsecs * chart(2) )
     &                stime = stime - .05
              ELSE
                  stime = INT(buf(50))
c****             don't let it be too close to either end
                  IF( buf(50) .GT. stime + (nsecs * chart(1))+ .5 )
     &                stime = stime + .5
                  IF( buf(50) .GT. stime + nsecs * chart(2) )
     &                stime = stime - .5
              ENDIF
          ENDIF
          IF( vscale .EQ. 0 ) THEN
              vscale = 1.25
              IF( si .LT. .001 ) vscale = 10.
          ENDIF
c          IF( nsecs .EQ. 0 ) nsecs = 12. / vscale
          IF( nsecs .EQ. 0 ) nsecs = nsamps * si
      ENDIF
      ostime=stime
c**** This is a tough one.  What does fno really mean anyway?
c**** This causes decreasing shot numbers to be dropped, so stop doing it
c      IF( fno .LT. 0 ) THEN
c          fno = lbuf(3)
c          IF( lbuf(7) .GT. 0 ) fno = lbuf(6)
c      ENDIF
   90 FIRST=.FALSE.
      range = lbuf(10)
      LNUM=LBUF(3)                                                      ! SHOT NUMBER
      LTRAC=LBUF(4)                                                     ! SHOT TRACE NUMBER
      IF( LBUF(7) .NE. 0 ) THEN                                         ! IS THE DATA SORTED ACCORDING TO RP?
          LNUM=LBUF(6)                                                  ! THE RP NUMBER
          LTRAC=LBUF(7)                                                 ! TRACE NUMBER WITHIN THE RP
      ENDIF
      IF( FDAY .NE. 0 ) LNUM=IBUF(80)*10000+IBUF(81)*100+ibuf(82)       ! convert into gmt fno
   95 CONTINUE
c      print *,' lnum=',lnum,' fno=',fno,' lno=',lno,' ninc=',ninc,
c     &' ltrac=',ltrac,' ftr=',ftr,' ltr=',ltr
c      print *,' 7=',lbuf(4),lbuf(7),' intrcs=',intrcs
      IF(LNUM.LT.FNO.AND.LTRAC.LT.FTR.AND.MLISTS.GT.1) GO TO 10         ! IS THIS SHOT BEFORE THIS PARAMTER LIST
      IF(LNUM.LT.FNO.OR.LTRAC.LT.FTR) RETURN                            ! IS THIS SHOT BEFORE ANY LIST?
      IF( ltrac .GT. ltr ) RETURN
      IF( ninc .GT. 1 ) THEN
          IF( lbuf(7) .NE. 0 ) THEN
              IF( lbuf(51) .EQ. -1) fno = fno + ninc                    ! if rp sorted and last trace of the rp
          ELSE
              IF( lbuf(4) .EQ. intrcs .OR. ltrac .EQ. ltr ) fno=fno+ninc
          ENDIF
      ENDIF
c      IF( hscale .EQ. 0 ) THEN
c          IF( frange .NE. 99999 .AND. range .LT. frange ) RETURN
c          IF( lrange .NE. 99999 .AND. range .GT. lrange ) RETURN
c      ENDIF
      IF( LNUM .GT. LNO ) THEN                                          ! USE THE PARAMETERS OF THIS LIST
          IF(MLISTS.GE.NLISTS) RETURN                                   ! ANY MORE USER PARAM LISTS ON DISC
C****
C****     GET ANOTHER USER PARAMETER LIST FROM DISC
C****
          MLISTS=MLISTS+1
          READ(MUNIT) FNO,FDAY,FTR,LNO,LDAY,LTR,STIME,NSECS,VSCALE,DEF,
     *       TRPIN,RELAMP,ANNTYP,DECIMF,IHDRUN,NCOMMS,ANNINC,FANNO,ninc,
     *       hscale, plotsi, ptype, frange, lrange, rstime, absval, hdr,
     *       ihdr, lhdr, iwrap, anntyp2
          GO TO 95
      ENDIF
C****
C****   GOT SOMETHING TO PLOT!!! DO IT.
C****
      nsamps = numdat
      IF( nsamps .GT. maxsamps ) THEN
          PRINT *,' ***  WARNING  ***  Trace not plotted - nsamps of ',
     &      nsamps, ' exceeds the maximum of ',maxsamps
          RETURN
      ENDIF
c**** MOVE THE TRACE FROM THE AP SIMULATOR TO MEMORY (IT SIMPLIFIES THINGS!
c**** rlseap gets it out of the ap simulator and sets in to 0, so,
c**** in = 0 means the data is in buf and in = 1 means it's in the ap
c**** iout = 0 means transfer it to buf!
      IOUT=0
      IF(IUSEAP.EQ.0.AND.IN.NE.0) CALL RLSEAP(BUF(NUMHDR+1),NUMDAT)
      DO 110 I=1,NUMHDR                                                 ! SAVE THE TRACE HEADER
  110 SCR(I)=BUF(I)
      IF( IN .EQ. 0 .OR. IUSEAP .EQ. 0 ) THEN                           ! IS THE DATA IN THE AP?
          DO 130 I=1,NSAMPS                                             ! DO THE MOVE IN MEMORY
  130     SCR(NUMHDR+I)=BUF(NUMHDR+I)
      ELSE
          CALL APGET(SCR(NUMHDR+1),IN,NSAMPS,2)                         ! SAVE THE TRACE IN THE SCRATCH AREA
      ENDIF
      IF( ibuf(15) .EQ. 2 ) THEN                                        ! zero out dead traces
           DO i = 0, nsamps-1
              buf(numhdr+i) = 0.
           ENDDO
      ENDIF
      IF( DECIMF .NE. 1 ) THEN                                          ! DO WE NEED TO DECIMATE THE DATA?
         NUMDAT = NSAMPS/DECIMF                                         ! OBVIOUSLY WE HAVE FEWER SAMPLES AFTER DECIMATION!
         NSAMPS = NUMDAT
         BUF(49) = BUF(49)*FLOAT(DECIMF)                                ! CHANGE THE SAMPLE INTERVAL
c****   the data to plot are in buf
         J = 1
         DO 160 I=1,NSAMPS                                              ! DO THE DECIMATION IN MEMORY
            BUF(NUMHDR+I)=BUF(NUMHDR+J)
  160    J = J+DECIMF
      ENDIF
      NDONE=NDONE+1                                                     ! COUNT THE NUMBER OF TRACES PLOTTED
      IF( plotsi .GT. 0. ) si = plotsi
      ST=STIME
      IF( STIME .EQ. -99999. ) THEN
          st = delay
          IF( si .GE. .001 ) THEN
              temp = delay * 1000
              itemp = NINT(temp)
              itemp = itemp / 10 * 10
              st = FLOAT(itemp) / 1000.
          ENDIF
      ENDIF
      stimel = st
      IF( rstime .NE. -99999. ) st = delay + rstime
      st = st - delay                                                   ! SUBTRACT OUT THE DEEP WATER DELAY
c****
c****  Replicate a "chart recorder" by adjusting the plot stime based
c****  on the water bottom time.  If the wb is rising and is close to
c****  the top of the plotter, decrease STIME.  If the wb is increasing
c****  and is below chart(2) (the middle of the plotter?), increase
c****  STIME.  The object is to keep the wb in the top of the plot.
c****    (46) = delay,  (50) = wbt
c****
      IF( chart(2) .NE. 0. .AND. buf(50) .NE. 0. ) THEN
          IF( n_wbts .NE. MAX_WBTS ) THEN
              n_wbts = n_wbts + 1
          ELSE
              DO i = 1, n_wbts - 1
                 wbts(i) = wbts(i+1)
              ENDDO
          ENDIF
          wbts(n_wbts) = buf(50)
          ave_wbt = wbts(1)
          IF( n_wbts .GT. 1 ) THEN
              DO i = 2, n_wbts
                 ave_wbt = ave_wbt + wbts(i)
              ENDDO
              ave_wbt = ave_wbt / FLOAT(n_wbts)
          ENDIF
          IF( (buf(50) .LT. stime + nsecs * chart(1)) .AND.
     &        (ave_wbt .LT. stime + nsecs * chart(1)) ) THEN
c****         The -.05 and +.05 (5%) fudge factor is needed to allow a little
c****         heave or wiggle so the next trace doesn't trigger another change.
              IF( si .GE. .001 ) THEN
                  stime = INT(buf(50)) - nsecs * (chart(2)-.05)
              ELSE
                  temp = buf(50) * 10.
                  itemp = INT(temp)
                  stime = FLOAT(itemp) / 10. - nsecs * (chart(2)-.05)
              ENDIF
          ELSEIF( (buf(50) .GT. stime + nsecs * (chart(2)+.05) ) .AND.
     &        (ave_wbt .GT. stime + nsecs * (chart(2)+.05) ) ) THEN
              IF( si .GE. .001 ) THEN
                  stime = INT(buf(50)) - nsecs * chart(1)
              ELSE
                  temp = buf(50) * 10.
                  itemp = INT(temp)
                  stime = FLOAT(itemp) / 10. - nsecs * chart(1)
              ENDIF
          ENDIF
          IF( stime .LT. 0. ) stime = 0.
          st = stime - delay
          IF( st .LT. 0. ) THEN
              st = 0.
              stime = delay
          ENDIF
      ENDIF
c****
      IF( nsecs .EQ. 0 .AND. n2plot .EQ. 0 ) n2plot = numdat
      IF( nsecs .GT. 0 .AND. n2plot .EQ. 0 ) THEN
          n2plot = nsecs / si + .5
c          IF( n2plot .GT. numdat ) n2plot = numdat
c      ELSE
c          n2plot = MIN(FLOAT(n2plot), (size-offset)/vscale/si)
c          IF( n2plot .GT. numdat ) n2plot = numdat
      ENDIF
      IF( n2plot .GT. maxsamps ) THEN
          PRINT *,' ***  ERROR  ***  Too many samples in the plot.'
          PRINT *,' Requested ',n2plot,' samples, max is ',maxsamps
          PRINT *,' Use parameters SECS or DECIMF to reduce the plot.'
          CALL EXIT
      ENDIF
      IF( delay .NE. buf(46) .AND. stime .LT. 0 .AND. hscale .EQ. 0.
     &    .AND. rstime .LE. -100. .AND. chart(1) .EQ. 0 ) THEN         ! should we make a gap in the plot for a new delay?
          DO i=1,nspace
             CALL trplot(scr(nsamps+1),si,n2plot,0,1,2)
          ENDDO
          ntother = ntother + nspace                                    ! count the blank traces for refraction
      ENDIF
c**** hate to do this, but ....
      IF( stimel + FLOAT(n2plot)*si .LT. delay ) THEN
          PRINT *,' ***  WARNING  ***  Plot window is before the data.'
          RETURN
      ENDIF
      IF( stime .GT. delay + REAL(ibuf(58)) * si ) THEN
          PRINT *,' ***  WARNING  ***  Plot window is after the data.'
          RETURN
      ENDIF
      IF( ostime .NE. stime .AND. ndone .GT. 1 .AND. hscale .EQ. 0.
c     &    .AND. rstime .LE. -100. .AND. lastspace .LT. ndone-100 ) THEN
     &    .AND. rstime .LE. -100. ) THEN
c****     watch out for too frequent jumps
c****     create a space in the plot when the time axis changes
          DO i=1,nspace
             CALL trplot(scr(nsamps+1),si,n2plot,0,1,2)
          ENDDO
          ntother = ntother + nspace
          IF( chart(2) .NE. 0 ) THEN
c****         If a chart, annotate the new scale (time lines)
              stimel = stime
              IF( tlines(1) .NE. 0. ) THEN
c                 annotate the new
                  CALL trplot(scr(nsamps+1),si,n2plot,0,1,-3)
              ENDIF
          ENDIF
          lastspace = ndone
      ENDIF
c**** If record space, then create a space before trace 1 of a shot
      IF( irecsp .EQ. 1 ) THEN
          IF( lbuf(7) .EQ. 0 .AND. lbuf(4) .EQ. 1 ) THEN
              DO i=1,nspace
                 CALL trplot(scr(nsamps+1),si,n2plot,0,1,2)
              ENDDO
              ftag = ndone
          ENDIF
      ENDIF
  240 ostime=stime
      IF( hscale .NE. 0 ) THEN
c      Why did this exist?  Before polint?
c          IF( n2plot .GT. nsamps ) n2plot = nsamps
          CALL refplot( hscale, scr(nsamps+1), buf, lbuf, frange,
     &         lrange, istop, prange, ishotskip, si, n2plot, absval )
          IF( ishotskip .NE. 0 ) THEN
              ndone = ndone - 1
              RETURN
          ENDIF
      ENDIF
      IF( istop .LT. 0 ) RETURN
c****
c****  If the plot starts after the beginning of the data, shift it so
c****     that it starts at the beginning AND zero the backend
c****
      ISTART = NUMHDR + NINT(ST/SI)
      IF( istart .GT. numhdr ) THEN
          idiff = istart - numhdr
          n = nsamps - idiff
c****   the data being plotted are in buf, not scr or the ap
          DO i = 1, n
              buf(numhdr+i) = buf(numhdr+idiff+i)
          ENDDO
          DO i = 1, idiff
              buf(numhdr+n+i) = 0.
          ENDDO
          nsamps = n
          istart = numhdr
      ENDIF
c****
c****  If the plot starts before the data, fill the front of the trace
c****
      IF( ISTART .LT. NUMHDR ) THEN
          N = -ISTART + NUMHDR                                          ! THIS IS THE NUMBER OF ZEROES WE NEED TO PAD THE FRONT OF THE DATA
c****   the data being plotted are in buf, not scr or the ap
          DO i = 1, nsamps                                       ! MOVE THE TRACE DOWN THE BUFFER TO MAKE ROOM FOR THE PAD
             BUF(NUMHDR+nsamps+n-I+1)=BUF(NUMHDR+nsamps-I+1)
          ENDDO
          DO I=1,N
             BUF(NUMHDR+I)=0.                                           ! PAD THE FRONT WITH ZEROES
          ENDDO
          ISTART = NUMHDR                                                ! THE DATA NOW STARTS AT THE BEGINNING OF BUF
          nsamps = nsamps + n
      ENDIF
c****
c****  If there is less data than the amount to plot, fill the back end
c****
      IF( ISTART - NUMHDR + NSAMPS .LT. n2plot ) THEN                   ! ARE WE PLOTTING MORE THAN THERE IS DATA?
          N = ISTART - NUMHDR + n2plot - nsamps                         ! PAD THE END OF THE TRACE WITH ZEROES
          IF( IN .EQ. 0 ) THEN                                          ! IS THE DATA IN THE AP?
              DO 505 I=1,N
  505         BUF(NUMHDR+nsamps+I)=0.
          ELSE
              CALL VCLR(IN+nsamps,1,N)                                  ! ZERO IT OUT IN THE AP
          ENDIF
          nsamps = nsamps + n
      ENDIF
c****
c****   Rectify the data if asked to
c****
      IF( irectify .NE. 0 ) THEN
          IF( in .NE. 0 .AND. iuseap .EQ. 1 ) STOP
          DO i = 1, n2plot
             buf(istart+i) = ABS(buf(istart+i))
          ENDDO
      ENDIF
c****
c****  If the plot is reverse direction, flip it and reverse it
c****
      IF( IDIR .EQ. -1 ) THEN                                           ! NEGATE THE TRACE IF THE PLOT IS BACKWARDS
          IF( IN .NE. 0 .AND. IUSEAP .EQ. 1 ) THEN                      ! DO IT IN THE AP IF IT IS ALREADY IN THE AP!
              INDX = IN + ISTART - NUMHDR                               ! THE INDEX IN THE AP OF THE FIRST SAMPLE TO PLOT
              CALL VNEG(INDX,1,NEXTAD,1,n2plot)
              CALL VMOV(NEXTAD+NSAMPS-1,-1,INDX,1,n2plot)               ! REVERSE THE ORDER
          ELSE
              DO 520 I = 1, n2plot
  520         BUF(ISTART+I) = -BUF(ISTART+I)
              N2 = n2plot / 2
              DO 530 I = 1, N2                                          ! NOW FLIP THE ORDER (SO THE PLOT IS UPSIDE DOWN)
                 TEMP = BUF(ISTART+I)
                 BUF(ISTART+I) = BUF(ISTART+n2plot+1-I)
                 BUF(ISTART+n2plot+1-I)=TEMP
  530         CONTINUE
c****         Shift the whole thing because the data starts at zero
c****          rather than 1!
              DO 540 i = n2plot, 2, -1
  540         buf(istart+i) = buf(istart+i-1)
              buf(istart+1) = 0.
          ENDIF
      ENDIF
C****
C****   DO THE TAGGING AND ANNOTATION
C****
      ITAG=0                                                            ! SET THE TAG SIGNAL TO NO TAG
      IF( anntyp .EQ. 5 ) GOTO 550                                      ! annotation by even minute?
      IF(FTAG.NE.NDONE.OR.TAGINC.EQ.0) GOTO 700
      ITAG=1
      FTAG=FTAG+TAGINC
      itemp = anntyp
      IF( anntyp .EQ. 6 .AND. hscale .NE. 0. ) THEN
          fanno = prange
          itemp = 1
      ENDIF
c**** annotate by even gmt interval?
  550 IF( anntyp .EQ. 5 ) THEN
c****  stupid compiler insists mod has same integer length!
          itemp = ibuf(82)
          IF( MOD(itemp,ANNINC) .NE. 0 .OR. ibuf(82) .EQ. lastmn )
     &        GOTO 700
          itemp = 4
          itag = 1
          lastmn = ibuf(82)
      ENDIF
      CALL getlanno( itemp, hdr, lhdr, ihdr,
     &     buf, lbuf, ibuf, fanno, lanno )
      CALL getlanno( anntyp2, hdr, lhdr, ihdr,
     &     buf, lbuf, ibuf, fanno, lanno2 )
      IF( anntyp .EQ. 4 .OR. anntyp .EQ. 1 ) fanno = fanno + anninc
C****
C****   NOW PLOT THE THING!
C****
  700 CONTINUE
      IF(IN.NE.0) IN=ISTART-NUMHDR+IN
      IF( ishotskip .NE. 1 ) THEN                                       ! refraction plot may have tossed it away!
          CALL TRPLOT(BUF(ISTART+1),SI,n2plot,0,1,ITAG)
          ntlive = ntlive + 1
          IF( lunhead .NE. 0 ) THEN
              lbuf(1) = nraster
              DO i = 1, 60
                 scr(i) = buf(i)
              ENDDO
              iscr(58) = 0
              IF( stime .NE. -99999. ) THEN
                  scr(46) = stime
                  iscr(55) = NINT(stime*1000.)
              ENDIF
              IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) THEN
                  CALL swap32( lscr(1), 7 )
                  CALL swap16( iscr(15), 1 )
                  CALL swap16( iscr(17), 1 )
                  CALL swap32( lscr(10), 8 )
                  CALL swap16( iscr(35), 2 )
                  CALL swap32( lscr(19), 4 )
                  CALL swap16( iscr(45), 2 )
                  CALL swap16( iscr(53), 7 )
                  CALL swap16( iscr(79), 6 )
                  CALL swap32( lscr(46), 15 )
              ENDIF
              CALL wrdisc( lunhead, scr, numhdr )
          ENDIF
      ENDIF
      LANNO=' '                                                         !  CLEAR THE ANNOTATION BUFFER
      index = istart + 1
      nums = numdat - n2plot
      n = n2plot
  710 CONTINUE
c**** Wrap the trace around if the user asked to.  The gotcha on nums >1
c**** is that if the trace is 4001 samples we'll put the last sample on
c**** a new line.
      IF(iwrap .NE. 0 .AND. nums .GT. 1) THEN
           index=index+n
           n=min0(n2plot,nums)
           CALL trplot(buf(index),si,n,0,1,0)
           ntother = ntother + 1
           nums=nums-n
           GOTO 710
      ENDIF
      IF( iwrap .NE. 0 .AND. hscale .EQ. 0. ) THEN
          DO i=1,nspace
             CALL trplot(scr,si,n2plot,0,1,2)
          ENDDO
          ntother = ntother + nspace
      ENDIF
      IF( FSPACE .EQ. NDONE .AND. ndone .GT. 0 ) THEN
          ITAG=2                                                        ! CREATE THE SPACES BETWEEN SHOTS/RPS
          DO i=1,nspace
             CALL trplot(scr,si,n2plot,0,1,2)
          ENDDO
          ntother = ntother + nspace
          FSPACE=FSPACE+SPACEI
      ENDIF
c**** If record space, then create a space after the last trace of
c**** a gather (process gather writes a -1 in word 51)
      IF( irecsp .EQ. 1 ) THEN
          IF( lbuf(7) .NE. 0 .AND. lbuf(51) .EQ. -1 ) THEN
              DO i=1,nspace
                 CALL trplot(scr,si,n2plot,0,1,2)
              ENDDO
              ftag = ndone + 1
              ntother = ntother + 1
          ENDIF
      ENDIF

C****
C****
      NUMDAT=ISCR(58)                                                   ! NOW RESTORE THE TRACE INTO THE PIPE
      ITEMP=NUMHDR+NUMDAT
      DO 810 I=1,ITEMP
  810 BUF(I)=SCR(I)
      RETURN
      END
