      SUBROUTINE irisex( buf, lbuf, ibuf, scr )
c
c                               PROCESS IRIS
c                               ------- ----
c  Doc date: 13 May 1990
c
c    Convert IRIS data logger data into standard SEGY formated data.
c  It is assumed that all the data input through process diskin in a
c  single job are a "single event".  The DISKIN parameter FORMAT may be
c  used to read the IRIS data traces.
c
c    This does the following:
c 1)  This process will make the data into SIOSEIS type RP sorted data.
c     That is, it will increment the rp trace number by 1 until the end
c     of the job.  The last trace does NOT have the sioseis end of
c     gather flag set, so it may be prudent to read any rps produced with
c     process diskox with the diskin parameter forgat.
c 2)  Process IRIS will also correct the shot times (the time of the
c     first sample - recorded in the SEGY GMT) to be at the same GMT 
c     rather than the IRIS trigger time.  The mil of the GMT is dropped 
c     also.  This should make timing a little easier to see on a seismic
c     plot.  On output, all data is relative to the GMT in the SEGY
c     header.  
c     Proceedurally, this process uses the GMT of the first trace
c     encountered, it subtracts 1, and uses that as the output time
c     reference (ignoring the milliseconds).
c      e.g.  trace 1 has day 110 hour 2 min 24 sec 51 mil 107
c            trace 2 has day 110 hour 2 min 24 sec 50 mil 974
c            trace 3 has day 110 hour 2 min 24 sec 51 mil 2
c       then the output GMT is set to day 110 hour 2 min 24 sec 50
c       and trace 1 has a delay of 1.107
c           trace 2 has a delay of 0.974
c           trace 3 has a delay of 1.002
c      
c     THIS VERSION ACTUALLY TIME SHIFTS THE DATA
c
c  ARGUMENTS:
c  buf   - The trace, with SEGY header as TYPE REAL
c  lbuf  - The trace, with SEGY header as TYPE INTEGER*4
c  ibuf   - The trace, with SEGY header as TYPE INTEGER*2
c  scr   - A scratch array.
c
c  COPYRIGHT (C) The Regents of the University of California
c  ALL RIGHTS RESERVED.  
c  Written by Paul Henkart, Scripps Institution of Oceanography, 6 May 1990
c
      DIMENSION buf(111), lbuf(111), ibuf(111), scr(111)
      INTEGER*2 ibuf
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr
      SAVE
c
      DATA lfor/0/, lprint/2/
c
      IF( lfor .EQ. 0 ) THEN
          lfor = lfor + 1
          ltrace = 1
c         set the time of the last shot to this one
          lyear = ibuf(79)
          lday = ibuf(80)
          lhour = ibuf(81)
          lmin = ibuf(82)
          lsec = ibuf(83)
          lmil = ibuf(104)
      ENDIF
      shift = REAL( ibuf(104) ) / 1000. +                              ! mils
     &        FLOAT( ibuf(83) - (lsec-1) ) +                            ! second
     &        FLOAT( ibuf(82) - lmin ) * 60. +                          ! minutes
     &        FLOAT( ibuf(81) - lhour) * 60. * 60. +
     &        FLOAT( ibuf(80) - lday ) *24. * 60. * 60.
c**** set the SEGY header
      lbuf(6) = lfor
      lbuf(7) = ltrace
      ltrace = ltrace + 1
      ibuf(83) = lsec -1
c****
c****   Shift the data - assume a positive shift
c****
      IF( ibuf(15) .EQ. 2 ) RETURN                                      ! forget dead traces
      nsamps = ibuf(58)
      si = buf(49)                                                      ! sample interval in seconds
      nshift = NINT( shift / si )
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' trigger (day, hour, min, sec, mil) =',
     &        ibuf(106), ibuf(107), ibuf(108), ibuf(109), ibuf(110)
          PRINT *,' GMT (day, hour, min, sec, mil) =',
     &        ibuf(80), ibuf(81), ibuf(82), ibuf(83), ibuf(104)
          PRINT *,' shift=',shift,' nshift=',nshift
      ENDIF
      IF( nshift .EQ. 0 ) RETURN
      IF( nshift .LT. 0 ) THEN
          DO 500 i = 1, nsamps - nshift
  500     buf(numhdr+i) = buf(numhdr+nshift+i)
          ibuf(58) = ibuf(58) - nshift
          numdat = ibuf(58)
          RETURN
      ENDIF
c**** a positive shift means zero out the front, then move the data
      DO 600 i = 1, nsamps - nshift
  600 scr(i) = buf(numhdr+i)
      DO 610 i = 1, nshift
  610 buf(numhdr+i) = 0.
      DO 620 i = 1, nsamps - nshift
  620 buf(numhdr+nshift+i) = scr(i)
      RETURN
      END
