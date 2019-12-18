      SUBROUTINE grdoutex( buf, lbuf, ibuf, scr, lscr, iscr, istop )

c  896 byte header  followed by the transposed
c         seismics.  Transposed means sorted by time.
c  The x dimension is space.
c  The y dimension is time.
c  The z diemnsion is amplitude.
c  SEGY data is stored (y,x)  GMT wants it (x,y)
c  The 896 byte header is:
c
c  GRD Format 1
c  bytes 1 - 4    = integer*4  nx
c  bytes 5 - 8    = integer*4  ny
c  bytes 9 - 12   = integer*4  node offset
c  bytes 13 - 16  = integer*4  dummy so that c lines up the 8 byte double
c  bytes 17 - 24  = real*8 xmin
c  bytes 25 - 32  = real*8 xmax
c  bytes 33 - 40  = real*8 ymin
c  bytes 41 - 48  = real*8 ymax
c  bytes 49 - 56  = real*8 zmin
c  bytes 57 - 64  = real*8 zmax
c  bytes 65 - 72  = real*8 xinc
c  bytes 73 - 80  = real*8 yinc
c  bytes 81 - 88  = real*8 z scale
c  bytes 89 - 96  = real*8 z add offset
c  bytes 1 - 80 (97 - 176)   = ASCII  x units
c  bytes 81 - 160 (177 - 256) = ASCII  y units
c  bytes 161 - 240(257 - 336) = ASCII  z units
c  bytes 241 - 320(337 - 416) = ASCII  title
c  bytes 321 - 640(417 - 736) = ASCII  GMT commands, 1 per 80 characters
c  bytes 641 - 800(737 - 896) = ASCII  comments
c
c*****   grd version 2
c/* This section is flexible. It is not copied to any grid header */
c  bytes 1 - 4    = integer*4  nx
c  bytes 5 - 8    = integer*4  ny
c  bytes 9 - 12   = integer*4  node offset
c  bytes 13 - 16 = integer*4  type      /* Grid format  *
c  bytes 17 - 272 = character*256  name    /* Actual name of the file after any =<stuff> has been removed */
c  bytes 273 - 276 int y_order;                    /* NetCDF: 1 if S->N, -1 if N->S */
c  bytes 277 - 280 int z_id;                       /* NetCDF: id of z field */
c  bytes 281 - 284 int ncid;                       /* NetCDF: file ID */
c  bytes 285 - 288 int t_index[3];                 /* NetCDF: index of higher coordinates */
c  bytes 289 - 296 double nan_value;               /* Missing value as stored in grid file */
c  bytes 297 - 312 double xy_off;                  /* 0.0 (node_offset == 0) or 0.5 ( == 1) */
c  bytes 305 - 320 = real*8 xmin
c  bytes 313 - 328 = real*8 xmax
c  bytes 321 - 336 = real*8 ymin
c  bytes 329 - 344 = real*8 ymax
c  bytes 337 - 352 = real*8 zmin
c  bytes 345 - 360 = real*8 zmax
c  bytes 361 - 368 = real*8 xinc
c  bytes 369 - 376 = real*8 yinc
c  bytes 377 - 384 = real*8 z scale
c  bytes 385 - 392 = real*8 z add offset
c  bytes 393 - 472 = 80 ASCII  x units
c  bytes 473 - 552 = 80 ASCII  y units
c  bytes 553 - 632 = 80 ASCII  z units
c  bytes 633 - 712 = 80 ASCII  title
c  bytes 713 - 1032 = 320 ASCII  GMT commands, 1 per 80 characters
c  bytes 1033 - 1192 = 160 ASCII remarks
c****  end version 2 ****
c
c  These must be null terminated
c
c   gmt_grd.h says:
c   Notes on node_offset:
c   Assume x_min = y_min = 0 and x_max = y_max = 10 and
c          x_inc = y_inc = 1.
c   For a normal node grid we have:
c   (1) nx = (x_max - x_min) / x_inc + 1 = 11
c       ny = (y_max - y_min) / y_inc + 1 = 1
c   (2) node # 0 is at (x,y) = (x_min, y_max) = (0,10) and
c       represents the surface value in a box with dimensions 
c       (1,1) centered on the node.
c    For a pixel grid we have:
c    (1) nx = (x_max - x_min) / x_inc = 10
c        ny = (y_max - y_min) / y_inc = 10
c    (2) node # 0 is at (x,y) =
c            (x_min + 0.5*x_inc, y_max - 0.5*y_inc) = (0.5, 9.5)
c            and represents the surface value in a box with 
c            dimensions (1,1) centered on the node.
c
c  Written by Paul Henkart, with some input and motivation from Dan
c  Copyright (C) by the Regents of the University of California, 2001
c
c  mod. June 11, 2001 to flip the trace in time.
c  mod 30 Nov 01 to change WARNING message from nt to ny
c  mod 18 Jul 03 - Use REAL*8 for gmt variables.
c  mod 13 Jan 04 - Add parameter ipad.  Some machines (Sun, SGI) should
c                  have a dummy long integer (4 bytes) in the header
c                  so that the DOUBLE are on a word boundary.  GMT's
c                  OSX version does not.
c  mod 24 Jan 08 - Add parameter GRDVER
c                - Add grd version 2
c
      PARAMETER (itsize = 262144 )
      COMMON /transp/ t(itsize)
      DIMENSION buf(111), lbuf(111), ibuf(111), 
     &          scr(111), lscr(111), iscr(111)
      INTEGER*2 ibuf, iscr
      REAL*8 dtemp(10)
      CHARACTER*80 cards(10), xunits, yunits, zunits, title, comment,
     &             command
      CHARACTER*256 name
      COMMON /grdname/ name
      COMMON /grdout/ luno, set(2), idummy, dx, lformat, lprint, xmin,
     &       xmax, ymin, ymax, zmin, zmax, xinc, yinc,
     &       zscale, xunits, yunits, zunits, title, comment, command,
     &       ipad, y_order, z_id, t_index(3)
      REAL*8 dx, xmin, xmax, ymin, ymax, zmin, zmax, xinc, yinc, zscale,
     &       dreal, nan, xy_off
      INTEGER y_order, z_id, t_index
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr
      COMMON /sioap/ iasgnd, irelse, in, iout, nextad, lapsiz, ifree,
     *     iuseap
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr, lsmusptr, lemusptr,
     *                 lsisptr,  lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr,   ifoldptr, icvleptr, lespnptr
      COMMON /edits/ ierror, iwarn, irun, now, icompt, isite, maxsamps,
     & nbperw, ireal
      DATA lunt/0/
      SAVE
c
c**** istop:   < 0 means no trace
c****          = 0 means not end, more traces to come
c****          > 0 means this is the last trace

      IF( istop .LT. 0 ) GOTO 1000
      delay = buf(ldelsptr)
      si = buf(lsisptr)
c      nsamps = ibuf(isampptr)
      nsamps = numdat
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' set=',set,' delay=',delay,' si=',si,' ns=',nsamps
      ENDIF
c****
c**** Set up the first time through
c****
      IF( lunt .EQ. 0 ) THEN
          CALL getfil( 1, lunt, scr, istat )
c****     The tranpose (trans4) needs a 1 word dummy at the beginning
          CALL wrdisc( lunt, scr, 1 )
          nx = 0
c****     preset set if not given.
          IF( set(1) .LT. 0. ) THEN
              set(1) = delay
              set(2) = delay + (nsamps-1) * si
          ENDIF
          IF( set(1) .GE. 0 ) THEN
              istarti = NINT(( set(1) - delay ) / si) + numhdr
              nt = NINT((set(2) - set(1)) / si ) + 1
          ENDIF
      ENDIF
c**** trace id = 2 means a bad trace, so make it zero
      IF( ibuf(itridptr) .NE. 2 ) THEN
          iout = 0                                                      ! force the data out of the ap
          CALL rlseap( buf(numhdr+1), nsamps )
      ELSE
          DO i = 1, nt
             scr(i) = 0.
          ENDDO
          GOTO 900
      ENDIF
      ndone = 0
      indexo = 0
c**** delay might change on every trace
      IF( set(1) .GE. 0 ) THEN
          istarti = NINT(( set(1) - delay ) / si) + numhdr
      ENDIF
c**** pad the front of the trace if the plot starts before the data
      IF( istarti .LT. numhdr ) THEN
          npad = numhdr - istarti
          DO i = 1, npad
             scr(indexo+i) = 0.
          ENDDO
          indexo = indexo + npad
          ndone = ndone + npad
      ENDIF
      ntodo = MIN(nsamps,nt-ndone)
      DO i = 1, ntodo
         temp = buf(istarti+i)
         scr(indexo+i) = temp
         zmin = MIN(zmin,temp)
         zmax = MAX(zmax,temp)
      ENDDO
      indexo = indexo + ntodo
      ndone = ndone + ntodo
c**** Pad back end if not enough data in the trace to get to end time
      IF( ndone .LT. nt ) THEN
          DO i = 1, nt-ndone
             scr(indexo+i) = 0.
          ENDDO
      ENDIF
  900 CONTINUE
c**** reverse the time because GMT wants positive time to go up!
      n = nt / 2
      n1 = nt + 1
      DO i = 1, n
         temp = scr(i)
         scr(i) = scr(n1-i)
         scr(n1-i) = temp
      ENDDO
      CALL wrdisc( lunt, scr, nt )
      nx = nx + 1
      IF( IAND(lprint,8) .NE. 0 ) THEN
          PRINT *,' istarti= ', istarti, ' ntodo= ',ntodo, ' nt= ',nt,
     *            ' nx= ',nx
      ENDIF
      IF( istop .EQ. 0 ) RETURN
c****
c****  Write the GMT - GRD header
c****
 1000 CONTINUE
      lscr(1) = nx
      lscr(2) = nt
      lscr(3) = 0                                                       ! normal node grid
      IF( lformat .EQ. 1 ) THEN
          lscr(4) = 0                                                   ! dummy
          IF( IAND(lprint,8) .NE. 0 ) PRINT *,' format ',lformat,
     &        ' int 1-4= ',(lscr(i),i=1,4)
          IF( ipad .NE. 0 ) THEN
              CALL wrdiscb( luno, lscr, 16 )
          ELSE
              CALL wrdiscb( luno, lscr, 12 )
          ENDIF
      ENDIF
      IF( lformat .EQ. 2 ) THEN
          lscr(4) = lformat     ! grid format
          IF( IAND(lprint,8) .NE. 0 ) PRINT *,' format ',lformat,
     &        ' int 1-4= ',(lscr(i),i=1,4)
          CALL wrdiscb( luno, lscr, 16 )
          CALL wrdiscb( luno, name, 256 )
          lscr(1) = y_order    ! N->S
          lscr(2) = z_id
          lscr(3) = ncid
          lscr(4) = t_index(1)
          lscr(5) = t_index(2)
          lscr(6) = t_index(3)
          CALL wrdiscb( luno, lscr, 24 )
          dtemp(1) = nan
          dtemp(2) = xy_off
          CALL wrdiscb( luno, dtemp, 16 )
      ENDIF
      dtemp(1) = xmin
      IF( xmax .EQ. 0 ) xmax = nx-1
      dtemp(2) = xmax
      dtemp(3) = ymin
      IF( ymax .EQ. 0 ) ymax = nt-1
      dtemp(4) = ymax
      dtemp(5) = zmin
      dtemp(6) = zmax
      IF( xinc .EQ. 0 ) xinc = 1
      dtemp(7) = xinc
      IF( yinc .EQ. 0 ) yinc = 1
      dtemp(8) = yinc
      dtemp(9) = zscale
      dtemp(10) = 0.
c      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap64(dtemp,10)
c	print *,' xmin, xmax ',(dtemp(i),i=1,2)
c	print *,' ymin, ymax ',(dtemp(i),i=3,4)
c	print *,' zmin, zmax ',(dtemp(i),i=5,6)
c	print *,' xinc, yinc ',(dtemp(i),i=7,8)
      CALL wrdiscb( luno, dtemp, 80 )
c      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap64(dtemp,10)
      cards(1) = xunits
      cards(2) = yunits
      cards(3) = zunits
      cards(4) = title
      cards(5) = command
      cards(9) = comment
      CALL wrdiscb( luno, cards, 800 )
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' GRD header: ',(lscr(i),i=1,4)
          PRINT *,(dtemp(i),i=1,10)
          DO i = 1, 10
             PRINT *,cards(i)
          ENDDO
      ENDIF
      IF( IAND(lprint,4) .NE. 0 ) THEN
          PRINT *,' GRD header: '
          PRINT *,' Title: ',cards(4)
          PRINT *,' Command: ',cards(5)
          PRINT *,' Comment: ',cards(9)
          PRINT *,' node offest ',lscr(3),' (0 means normal mode)'
          PRINT *,' x_min:',dtemp(1)
          PRINT *,' x_max:',dtemp(2)
          PRINT *,' x_inc:',dtemp(7)
          PRINT *,' x_units:', cards(1)
          PRINT *,' nx: ',lscr(1)
          PRINT *,' y_min: ',dtemp(3)
          PRINT *,' y_max:',dtemp(4)
          PRINT *,' y_inc:',dtemp(8)
          PRINT *,' y_units:', cards(2)
          PRINT *,' ny: ',lscr(2)
          PRINT *,' z_min: ',dtemp(5)
          PRINT *,' z_max:',dtemp(6)
          PRINT *,' z_units:', cards(3)
          PRINT *,' z_scale:',dtemp(9)
          PRINT *,' z_offset:',dtemp(10)
      ENDIF
      IF( ((xmax - xmin + xinc) / xinc) .NE. nx ) THEN
         PRINT *,' ***  WARNING  *** ((xmax - xmin + 1) / xinc) .NE. nx'
          PRINT *,' xmax=',xmax,' xmin=',xmin,' xinc=', xinc,' nx=',nx
      ENDIF
      IF( ((ymax - ymin + yinc) / yinc) .NE. nt ) THEN
         PRINT *,' ***  WARNING  *** ((ymax - ymin + 1) / yinc) .NE. ny'
          PRINT *,' ymax=',ymax,' ymin=',ymin,' yinc=', yinc,' ny=',nt
      ENDIF
c****
c****  Do the transpose in array t if the whole thing fits, o.w.
c****   the disk file is ordered ok for trans4.
c****
c      IF( nx * nt .LE. itsize ) THEN
c          CALL podisc( lunt, 1, 0 )
c          DO i = 1, nx
c             CALL rddisc( lunt, t, nt, istat )
c          ENDDO
c      ENDIF
      DO i = 1, nt
         CALL trans4( scr, nx, nt, lunt )
c         IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap32( scr, nx )
         CALL wrdisc( luno, scr, nx )
      ENDDO
      RETURN
      END
